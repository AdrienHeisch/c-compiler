module Compiler (compile) where

import Constant (Constant (Constant))
import Context (Context, declareFunc, defineFunc, getFunc, getLocals, newFunction, newScope)
import Context qualified (addLabel, addVar, getVar, hasLabel, makeAnonLabel, new)
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe, maybeToList)
import Expr (Expr)
import Expr qualified (Expr (..), eval)
import Expr qualified as ED (ExprDef (..))
import Identifier (Id (..))
import Instruction (Instruction (..), Program (..), Register (..), Value (..))
import Op (Op)
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
  SD.For finit fcond fincr fbody -> for finit fcond fincr fbody
  -- SD.Break ->
  -- SD.Continue ->
  SD.Return mexpr -> return_ mexpr
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
  !_ <- Context.declareFunc (Type.Function ret (map fst params), name)
  return []

funcDef :: Type -> Id -> [(Type, Maybe Id)] -> [Statement] -> State Context [Instruction]
funcDef ret name params body = do
  !_ <- Context.defineFunc (Type.Function ret (map fst params), name)
  context <- get
  Context.newFunction
  collectLabels body
  let namedParams = mapMaybe (\(ty, nm) -> (ty,) <$> nm) params
  loadParams namedParams
  ins <- statements body
  put context
  let Id nameStr = name
  return $ LABEL nameStr : ins

loadParams :: [(Type, Id)] -> State Context ()
loadParams params = case params of
  [] -> return ()
  (ty, name) : rest -> do
    !_ <- Context.addVar (ty, name)
    loadParams rest

var :: Type -> Id -> Maybe Expr -> State Context [Instruction]
var ty name mexpr = do
  !_ <- Context.addVar (ty, name)
  case mexpr of
    Nothing -> return [PUSH (Cst 0)]
    Just ex -> do
      ins <- expr ex
      return $ ins ++ [PUSH (Reg R0)]

block :: [Statement] -> State Context [Instruction]
block sts = do
  context <- get
  Context.newScope
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

for :: Either Statement (Maybe Expr) -> Maybe Expr -> Maybe Expr -> Statement -> State Context [Instruction]
for finit fcond fincr fbody = do
  lblPre <- Context.makeAnonLabel
  lblPost <- Context.makeAnonLabel
  insInit <- case finit of
    Left st -> statement st
    Right (Just e) -> expr e
    Right Nothing -> return []
  insCond <- concat <$> sequence (maybeToList $ expr <$> fcond)
  insIncr <- concat <$> sequence (maybeToList $ expr <$> fincr)
  insBody <- statement fbody
  return $ insInit ++ [LABEL lblPre] ++ insCond ++ [JEQ R0 (Lbl lblPost)] ++ insBody ++ insIncr ++ [JMP (Lbl lblPre), LABEL lblPost]

goto :: Id -> State Context [Instruction]
goto (Id lbl) = do
  hasLabel <- Context.hasLabel lbl
  if hasLabel
    then return [JMP (Lbl lbl)]
    else error $ "Undefined label : " ++ show lbl

label :: Id -> Statement -> State Context [Instruction]
label (Id lbl) st = do
  exists <- Context.hasLabel lbl
  if exists
    then do
      ins <- statement st
      return $ LABEL lbl : ins
    else error "Label does not exist"

expr :: Expr -> State Context [Instruction]
expr e = case Expr.def e of
  ED.Id name -> do
    insVar <- getVarAddr name
    return $ insVar ++ [LOAD R0 (Reg R1)]
  ED.IntLiteral (Constant Type.Int int) -> do
    return [SET R0 (Cst int)]
  -- ED.FltLiteral flt -> []
  -- ED.StrLiteral str -> []
  -- ED.ArrayDecl exs -> []
  ED.UnopPre op ex -> unop op ex
  -- ED.UnopPost op ex -> []
  ED.Binop left op right -> case Op.getBinaryAssignOp op of
    Just innerOp -> do
      insBinop <- binop (evalOrThrow e) left innerOp right
      insAssign <- binop (evalOrThrow e) left Op.Assign right
      return $ insBinop ++ insAssign
    Nothing -> binop (evalOrThrow e) left op right
  -- ED.Ternary ter_cond ter_then ter_else -> []
  ED.Call ex args -> call ex args
  ED.Parenthese ex -> expr ex
  ED.Invalid str -> error $ "Invalid expression : " ++ str
  _ -> error $ "Expression not implemented yet : " ++ display e

unop :: Op -> Expr -> State Context [Instruction]
unop op ex =
  let insOp = case op of
        Op.MultOrIndir -> [LOAD R0 (Reg R0)]
        Op.BitAndOrAddr -> [SET R0 (Reg R1)]
        _ -> error $ "Operator not implemented : " ++ show op
   in case op of
        _ | Op.isUnopAddressing op -> do
          insAddr <- exprAddress ex
          return $ insAddr ++ insOp
        _ | otherwise -> do
          insEx <- expr ex
          return $ insEx ++ insOp

binop :: Type -> Expr -> Op -> Expr -> State Context [Instruction]
binop _ left op right =
  let insOp = case op of
        Op.AddOrPlus -> [ADD R0 (Reg R4)]
        Op.SubOrNeg -> [SUB R0 (Reg R4)]
        Op.MultOrIndir -> [MUL R0 (Reg R4)]
        Op.Div -> [DIV R0 (Reg R4)]
        Op.Mod -> [MOD R0 (Reg R4)]
        Op.Assign -> [STORE R5 (Reg R0)]
        Op.Subscript -> [ADD R4 (Reg R0), LOAD R0 (Reg R4)]
        _ -> error $ "Operator not implemented : " ++ show op
   in case op of
        _ | Op.isBinopAddressing op -> do
          insAddr <- exprAddress left
          insVal <- expr right
          return $ insAddr ++ [SET R5 (Reg R1)] ++ insVal ++ insOp
        _ | otherwise -> do
          insL <- expr left
          insR <- expr right
          return $ insR ++ [SET R4 (Reg R0)] ++ insL ++ insOp

call :: Expr -> [Expr] -> State Context [Instruction]
call ex params = case Expr.def ex of
  ED.Id name -> do
    mvar <- Context.getFunc name
    case mvar of
      Nothing -> error $ "Undefined identifier : " ++ show name
      Just (_, Id nameStr, _) -> do
        insParams <- map (\ins -> ins ++ [PUSH (Reg R0)]) <$> mapM expr params
        return $ [PUSH (Reg LR), PUSH (Reg BP), SET BP (Reg SP)] ++ concat insParams ++ [CALL (Lbl nameStr), SET R0 (Reg RR), POP BP, POP LR]
  _ -> error "Unimplemented"

return_ :: Maybe Expr -> State Context [Instruction]
return_ mexpr = do
  vars <- Context.getLocals
  let frameSize = length vars
      insFrame = replicate frameSize DROP
  case mexpr of
    Nothing -> return $ insFrame ++ [RET (Cst 0)]
    Just ex -> do
      insEx <- expr ex
      return $ insEx ++ insFrame ++ [RET (Reg R0)]

exprAddress :: Expr -> State Context [Instruction]
exprAddress e = case Expr.def e of
  ED.Id name -> getVarAddr name
  ED.UnopPre Op.MultOrIndir e' -> do
    insEx <- expr e'
    return $ insEx ++ [SET R1 (Reg R0)]
  _ -> error $ "Can't get address of : " ++ display e

getVarAddr :: Id -> State Context [Instruction]
getVarAddr name = do
  mvar <- Context.getVar name
  case mvar of
    Nothing -> error $ "Undefined identifier : " ++ show name
    Just (idx, _) ->
      return [SET R1 (Reg BP), ADD R1 (Cst idx)]

collectLabels :: [Statement] -> State Context ()
collectLabels sts = case map Statement.def sts of
  [] -> return ()
  SD.Labeled (Id lbl) _ : _ -> do
    Context.addLabel lbl
    collectLabels $ tail sts
  _ : _ -> collectLabels $ tail sts

evalOrThrow :: Expr -> Type
evalOrThrow ex = case Expr.eval ex of Left ty -> ty; Right err -> error err