module Compiler (compile) where

import Constant (Constant (Constant))
import Context (Context, declareFunc, defineFunc, newFunction, newScope)
import Context qualified (addLabel, addVar, addVars, getVar, hasLabel, makeAnonLabel, new)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Expr (Expr (Expr))
import Expr qualified (Expr (..), eval)
import Expr qualified as ED (ExprDef (..))
import Identifier (Id (..))
import Instruction (Instruction (..), Program (..), Register (..), Value (..))
import Op (getBinaryAssignOp)
import Op qualified
import Statement (Statement)
import Statement qualified
import Statement qualified as SD (StatementDef (..))
import Type (Type)
import Type qualified (Type (..))
import Utils (Display (display), maybeListToList)

compile :: [Statement] -> Program
compile decls = Program $ evalState (statements decls) Context.new

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
  SD.FuncDec ty name params -> funcDec ty name params
  SD.FuncDef ty name params body -> funcDef ty name params body
  SD.Block sts -> block sts
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

funcDec :: Type -> Id -> [(Type, Maybe Id)] -> State Context [Instruction]
funcDec ret name params = do
  Context.declareFunc (Type.Function ret (map fst params), name)
  return []

funcDef :: Type -> Id -> [(Type, Maybe Id)] -> [Statement] -> State Context [Instruction]
funcDef ret name params body = do
  Context.defineFunc (Type.Function ret (map fst params), name)
  context <- get
  put $ Context.newFunction context
  let namedParams = mapMaybe (\(t, n) -> (t,) <$> n) params
  Context.addVars namedParams
  ins <- statements body
  put context
  let Id nameStr = name
  return $ [LABEL nameStr, SET BP (Reg SP)] ++ ins

var :: Type -> Id -> Maybe Expr -> State Context [Instruction]
var ty name mexpr = case mexpr of
  Nothing -> do
    Context.addVar (ty, name)
    return []
  Just ex -> do
    Context.addVar (ty, name)
    ins <- expr ex
    return $ ins ++ [PUSH (Reg R0)]

block :: [Statement] -> State Context [Instruction]
block sts = do
  context <- get
  put $ Context.newScope context
  ins <- statements sts
  put context
  return ins

if_ :: Expr -> Statement -> Maybe Statement -> State Context [Instruction]
if_ cond then_ else_ = do
  lblElse <- Context.makeAnonLabel
  lblPost <- Context.makeAnonLabel
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
  lblPre <- Context.makeAnonLabel
  lblPost <- Context.makeAnonLabel
  insCond <- expr cond
  insBody <- statements [body]
  return $ LABEL lblPre : insCond ++ [JEQ R0 (Lbl lblPost)] ++ insBody ++ [JMP (Lbl lblPre)] ++ [LABEL lblPost]

dowhile :: Expr -> Statement -> State Context [Instruction]
dowhile cond body = do
  lblPre <- Context.makeAnonLabel
  lblPost <- Context.makeAnonLabel
  insCond <- expr cond
  insBody <- statements [body]
  return $ LABEL lblPre : insBody ++ insCond ++ [JEQ R0 (Lbl lblPost)] ++ [JMP (Lbl lblPre)] ++ [LABEL lblPost]

goto :: Id -> State Context [Instruction]
goto (Id lbl) = do
  hasLabel <- Context.hasLabel lbl
  if hasLabel
    then return [JMP (Lbl lbl)]
    else error $ "Undefined label : " ++ show lbl

label :: Id -> Statement -> State Context [Instruction]
label (Id lbl) st = do
  Context.addLabel lbl
  ins <- statement st
  return $ LABEL lbl : ins

expr :: Expr -> State Context [Instruction]
expr e = case Expr.def e of
  ED.Id name -> do
    mvar <- Context.getVar name
    case mvar of
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
  mvar <- Context.getVar name
  case mvar of
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