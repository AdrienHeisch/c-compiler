module Compiler (compile) where

import Constant (Constant (Constant))
import Context (Context, addVar, addVars, getLabel, getVar)
import Context qualified (new)
import Control.Monad.State.Lazy (State, evalState, get, modify, put)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Expr (Expr (Expr))
import Expr qualified (Expr (..), eval)
import Expr qualified as ED (ExprDef (..))
import Identifier (Id)
import Instruction (Instruction (..), Program (..), Register (..), Value (..), getTyRegs)
import Op (getBinaryAssignOp)
import Op qualified
import Statement (Statement)
import Statement qualified
import Statement qualified as SD (StatementDef (..))
import Type (Type (Int))

compile :: [Statement] -> Program
compile decls = Program $ evalState (declarations decls) (Context.new Nothing)

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
  context <- get
  let namedParams = mapMaybe (\(t, n) -> (t,) <$> n) params
      newContext = addVars namedParams $ Context.new (Just context)
  put newContext
  ins <- statements body
  put context
  return $ SET BP (Reg SP) : ins

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
  -- SD.If cond then_ else_ -> if_ cond then_ else_
  -- SD.Switch {eval :: Expr, body :: Statement} ->
  SD.While cond body -> while cond body
  -- SD.DoWhile {body :: Statement, cond :: Expr} ->
  -- SD.For {finit :: Maybe Expr, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement} ->
  -- SD.ForVar {fdecl :: Statement, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement} ->
  -- SD.Break ->
  -- SD.Continue ->
  -- SD.Return (Maybe Expr) ->
  -- SD.Case (Constant IntRepr) ->
  -- SD.Goto Id ->
  -- SD.Labeled Id Statement ->
  SD.Invalid str -> error $ "Invalid statement : " ++ str
  _ -> error $ "Statement not implemented yet : " ++ show st
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
    let (r0, _) = {- trace (show $ getTyRegs ty) $ trace (show ty) -} getTyRegs ty
     in return $ ins ++ [PUSH (Reg r0)]

-- if_ :: Expr -> Statement -> Maybe Statement -> State Context [Instruction]
-- if_ cond then_ else_ = do
--   let rcond = case Expr.eval cond of
--         Type.Int -> I0
--         ty -> error $ "Type not implemented in binop : " ++ show ty
--   econd <- expr cond
--   return
--     [ {- econd,
--       JEQ rcond  -}
--     ]

while :: Expr -> Statement -> State Context [Instruction]
while cond body = do
  let (rcond, _) = getTyRegs $ Expr.eval cond
  insCond <- expr cond
  insBody <- statements [body]
  lblPre <- getLabel
  lblPost <- getLabel
  return $ LABEL lblPre : insCond ++ [JEQ rcond (Lbl lblPost)] ++ insBody ++ [JMP (Lbl lblPre)] ++ [LABEL lblPost]

expr :: Expr -> State Context [Instruction]
expr e = case Expr.def e of
  ED.Id name -> do
    context <- get
    case getVar context name of
      Nothing -> error $ "Undefined identifier : " ++ show name
      Just (idx, ty) ->
        let (r0, _) = getTyRegs ty
         in return [SET r0 (Reg BP), ADD r0 (Cst idx), LOAD r0 (Reg r0)]
  ED.IntLiteral (Constant Type.Int int) -> do
    return [SET I0 (Cst int)]
  -- ED.FltLiteral flt -> []
  -- ED.StrLiteral str -> []
  -- ED.ArrayDecl exs -> []
  -- ED.UnopPre op ex -> []
  -- ED.UnopPost op ex -> []
  ED.Binop (Expr (ED.Id name) _) Op.Assign right -> assign name right
  ED.Binop left op right ->
    case (left, getBinaryAssignOp op) of
      (Expr (ED.Id name) _, Just innerOp) -> do
        insBinop <- binop (Expr.eval e) left innerOp right
        insAssign <- assign name right
        return $ insBinop ++ insAssign
      (_, Just _) -> error "Can't assign to constant"
      (_, Nothing) -> binop (Expr.eval e) left op right
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
    Just (idx, ty) -> do
      insEx <- expr ex
      let (r0, r1) = getTyRegs ty
       in return $ insEx ++ [SET r1 (Reg BP), ADD r1 (Cst idx), STORE r1 (Reg r0)]

binop :: Type -> Expr -> Op.Op -> Expr -> State Context [Instruction]
binop ty left op right = do
  insL <- expr left
  insR <- expr right
  let (r0, r1) = getTyRegs ty
  let insOp = case op of
        Op.AddOrPlus -> ADD r0 (Reg r1)
        Op.SubOrNeg -> SUB r0 (Reg r1)
        Op.MultOrIndir -> MUL r0 (Reg r1)
        Op.Div -> DIV r0 (Reg r1)
        _ -> error $ "Operator not implemented : " ++ show op
  return $ insR ++ [PUSH (Reg r0)] ++ insL ++ [POP r1, insOp]