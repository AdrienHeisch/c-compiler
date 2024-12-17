module Compiler (compile) where

import Constant (Constant (Constant))
import Context (Context, addVar, addVars, anonLabel, getVar, addLabel, getLabel)
import Context qualified (new)
import Control.Monad.State.Lazy (State, evalState, get, modify, put)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Expr (Expr (Expr))
import Expr qualified (Expr (..), eval)
import Expr qualified as ED (ExprDef (..))
import Identifier (Id(..))
import Instruction (Instruction (..), Program (..), Register (..), Value (..))
import Op (getBinaryAssignOp)
import Op qualified
import Statement (Statement)
import Statement qualified
import Statement qualified as SD (StatementDef (..))
import Type (Type (Int))
import Utils (Display (display), maybeListToList)

compile :: [Statement] -> Program
compile decls = Program $ {- JMP (Lbl 0) :  -}evalState (declarations decls) (Context.new Nothing)

declarations :: [Statement] -> State Context [Instruction]
declarations decls = do
  case map Statement.def decls of
    [] -> return []
    SD.FuncDec ty name params body : _ -> do ins <- funcDef ty name params body; go ins
    -- DD.FuncDef ty name params : _ -> do ins <- funcDef ty name params; go ins
    SD.Invalid str : _ -> error $ "Invalid definition : " ++ str
    _ -> error $ "Declaration not implemented yet : " ++ show (head decls)
  where
    go ins = do
      more <- declarations (drop 1 decls)
      return $ ins ++ more

funcDef :: Type -> Id -> [(Type, Maybe Id)] -> [Statement] -> State Context [Instruction]
funcDef _ _ params body = do
  -- lbl <- (case name of Id "main" -> return 0; _ -> anonLabel)
  context <- get
  let namedParams = mapMaybe (\(t, n) -> (t,) <$> n) params
      newContext = addVars namedParams $ Context.new (Just context)
  put newContext
  ins <- statements body
  put context
  return $ [{- LABEL lbl,  -}NOP, SET BP (Reg SP)] ++ ins

statements :: [Statement] -> State Context [Instruction]
statements sts = case sts of
  [] -> return []
  st : sts' -> do
    insSt <- statement st
    insSts <- statements sts'
    return $ insSt ++ insSts

statement :: Statement -> State Context [Instruction]
statement st = case Statement.def st of
  SD.Empty -> return []
  SD.Block block -> statements block
  SD.Expr e -> expr e
  SD.Var (v :| vs) -> vars (v : vs)
  SD.If cond then_ else_ -> if_ cond then_ else_
  -- SD.Switch {eval :: Expr, body :: Statement} ->
  SD.While cond body -> while cond body
  SD.DoWhile body cond -> dowhile cond body
  -- SD.For {finit :: Maybe Expr, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement} ->
  -- SD.ForVar {fdecl :: Statement, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement} ->
  -- SD.Break ->
  -- SD.Continue ->
  -- SD.Return (Maybe Expr) ->
  -- SD.Case (Constant IntRepr) ->
  SD.Goto lblName -> goto lblName
  SD.Labeled lblName st' -> label lblName st'
  SD.Invalid str -> error $ "Invalid statement : " ++ str
  _ -> error $ "Statement not implemented yet : " ++ display st
  where
    vars :: [(Type, Id, Maybe Expr)] -> State Context [Instruction]
    vars vs = do
      ins <- mapM (\(ty, name, e) -> var ty name e) vs
      return . concat $ ins

var :: Type -> Id -> Maybe Expr -> State Context [Instruction]
var ty name mexpr = case mexpr of
  Nothing -> do
    modify $ addVar (ty, name)
    return []
  Just ex -> do
    modify $ addVar (ty, name)
    ins <- expr ex
    return $ ins ++ [PUSH (Reg R0)]

if_ :: Expr -> Statement -> Maybe Statement -> State Context [Instruction]
if_ cond then_ else_ = do
  lblElse <- anonLabel
  lblPost <- anonLabel
  insCond <- expr cond
  insThen <- statement then_
  insElse <- maybe (return Nothing) (fmap Just . statement) else_
  return $
    insCond
      ++ [JEQ R0 (Lbl lblElse)]
      ++ insThen
      ++ [JMP (Lbl lblPost), LABEL lblElse]
      ++ maybeListToList insElse
      ++ [LABEL lblPost] -- TODO avoid double label if no else

while :: Expr -> Statement -> State Context [Instruction]
while cond body = do
  lblPre <- anonLabel
  lblPost <- anonLabel
  insCond <- expr cond
  insBody <- statements [body]
  return $ LABEL lblPre : insCond ++ [JEQ R0 (Lbl lblPost)] ++ insBody ++ [JMP (Lbl lblPre)] ++ [LABEL lblPost]

dowhile :: Expr -> Statement -> State Context [Instruction]
dowhile cond body = do
  lblPre <- anonLabel
  lblPost <- anonLabel
  insCond <- expr cond
  insBody <- statements [body]
  return $ LABEL lblPre : insBody ++ insCond ++ [JEQ R0 (Lbl lblPost)] ++ [JMP (Lbl lblPre)] ++ [LABEL lblPost]

goto :: Id -> State Context [Instruction]
goto (Id lblName) = do
  context <- get
  let mlbl = getLabel context lblName
  case mlbl of
      Nothing -> error $ "Undefined label : " ++ show lblName
      Just lbl -> return [JMP (Lbl lbl)]

label :: Id -> Statement -> State Context [Instruction]
label (Id lblName) st = do
  lbl <- addLabel lblName
  ins <- statement st
  return $ LABEL lbl : ins

expr :: Expr -> State Context [Instruction]
expr e = case Expr.def e of
  ED.Id name -> do
    context <- get
    case getVar context name of
      Nothing -> error $ "Undefined identifier : " ++ show name
      Just (idx, _) ->
        return [SET R0 (Reg BP), ADD R0 (Cst idx), LOAD R0 (Reg R0)]
  ED.IntLiteral (Constant Type.Int int) -> do
    return [SET R0 (Cst int)]
  -- ED.FltLiteral flt -> []
  -- ED.StrLiteral str -> []
  -- ED.ArrayDecl exs -> []
  -- ED.UnopPre op ex -> []
  -- ED.UnopPost op ex -> []
  ED.Binop (Expr (ED.Id name) _) Op.Assign right -> assign name right
  ED.Binop left op right ->
    case (left, getBinaryAssignOp op) of
      (Expr (ED.Id name) _, Just innerOp) -> do
        insBinop <- binop (evalOrThrow e) left innerOp right
        insAssign <- assign name right
        return $ insBinop ++ insAssign
      (_, Just _) -> error "Can't assign to constant"
      (_, Nothing) -> binop (evalOrThrow e) left op right
  -- ED.Ternary ter_cond ter_then ter_else -> []
  -- ED.Call ex args -> []
  ED.Parenthese ex -> expr ex
  ED.Invalid str -> error $ "Invalid expression : " ++ str
  _ -> error $ "Expression not implemented yet : " ++ show e

assign :: Id -> Expr -> State Context [Instruction]
assign name ex = do
  context <- get
  case getVar context name of
    Nothing -> error $ "Undefined identifier : " ++ show name
    Just (idx, _) -> do
      insEx <- expr ex
      return $ insEx ++ [SET R1 (Reg BP), ADD R1 (Cst idx), STORE R1 (Reg R0)]

binop :: Type -> Expr -> Op.Op -> Expr -> State Context [Instruction]
binop _ left op right = do
  insL <- expr left
  insR <- expr right
  let insOp = case op of
        Op.AddOrPlus -> ADD R0 (Reg R4)
        Op.SubOrNeg -> SUB R0 (Reg R4)
        Op.MultOrIndir -> MUL R0 (Reg R4)
        Op.Div -> DIV R0 (Reg R4)
        _ -> error $ "Operator not implemented : " ++ show op
  return $ insR ++ [SET R4 (Reg R0)] ++ insL ++ [insOp]

evalOrThrow :: Expr -> Type
evalOrThrow ex = case Expr.eval ex of Left ty -> ty; Right err -> error err