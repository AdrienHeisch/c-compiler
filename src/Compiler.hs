module Compiler (compile) where

import Constant (Constant (Constant))
import Context (Context, addVar, addVars, getVar)
import Context qualified (new)
import Control.Monad.State.Lazy (State, evalState, get, modify, put)
import Declaration (Declaration)
import Declaration qualified (Declaration (..))
import Declaration qualified as DD (DeclarationDef (..))
import Expr (Expr)
import Expr qualified (Expr (..), eval)
import Expr qualified as ED (ExprDef (..))
import Identifier (Id)
import Instruction (Instruction (..), Program (..), Register (..), Value (..), getTyRegs)
import Op qualified
import Statement (Statement)
import Statement qualified
import Statement qualified as SD (StatementDef (..))
import Type (Type (Int))
import Debug.Trace (trace)

compile :: [Declaration] -> Program
compile decls = Program $ evalState (declarations decls) (Context.new Nothing)

declarations :: [Declaration] -> State Context [Instruction]
declarations decls = do
  case map Declaration.def decls of
    [] -> return []
    DD.FuncDec ty name params body : _ -> do ins <- funcDef ty name params body; go ins
    DD.Invalid str : _ -> error $ "Invalid definition : " ++ str
    _ -> error $ "Declaration not implemented yet : " ++ show (head decls)
  where
    go ins = do
      more <- declarations (drop 1 decls)
      return $ ins ++ more

funcDef :: Type -> Id -> [(Type, Id)] -> [Statement] -> State Context [Instruction]
funcDef _ _ params body = do
  context <- get
  let newContext = addVars params $ Context.new (Just context)
  put newContext
  ins <- statements body
  put context
  return $ SET BP (Reg SP) : ins

statements :: [Statement] -> State Context [Instruction]
statements sts = case map Statement.def sts of
  [] -> return []
  SD.Empty : _ -> go []
  SD.Block block : _ -> do ins <- statements block; go ins
  SD.Expr e : _ -> do ins <- expr e; go ins
  SD.Var ty name e : _ -> do ins <- var ty name e; go ins
  -- SD.If cond then_ else_ : _ -> do ins <- if_ cond then_ else_; go ins
  -- SD.Switch {eval :: Expr, body :: Statement} ->
  -- SD.While {cond :: Expr, body :: Statement} ->
  -- SD.DoWhile {body :: Statement, cond :: Expr} ->
  -- SD.For {finit :: Maybe Expr, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement} ->
  -- SD.ForVar {fdecl :: Statement, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement} ->
  -- SD.Break ->
  -- SD.Continue ->
  -- SD.Return (Maybe Expr) ->
  -- SD.Case (Constant IntRepr) ->
  -- SD.Goto Id ->
  -- SD.Labeled Id Statement ->
  SD.Invalid str : _ -> error $ "Invalid statement : " ++ str
  _ -> error $ "Statement not implemented yet : " ++ show (head sts)
  where
    go is = do
      iss <- statements (drop 1 sts)
      return $ is ++ iss

var :: Type -> Id -> Maybe Expr -> State Context [Instruction]
var ty name mex = case mex of
  Nothing -> do
    modify $ addVar (ty, name)
    return []
  Just ex -> do
    modify $ addVar (ty, name)
    ins <- expr ex
    let (r0 : _) = trace (show $ getTyRegs ty) $ trace (show ty) getTyRegs ty
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

expr :: Expr -> State Context [Instruction]
expr e = case Expr.def e of
  ED.Id name -> do
    context <- get
    case getVar context name of
      Nothing -> error $ "Undefined identifier : " ++ show name
      Just (idx, ty) ->
        let (r0 : r1 : _) = getTyRegs ty
         in return [SET r0 (Reg BP), ADD r0 (Cst idx), LOAD r0 (Reg r0)]
  ED.IntLiteral (Constant Type.Int int) -> do
    return [SET I0 (Cst int)]
  -- ED.FltLiteral flt -> []
  -- ED.StrLiteral str -> []
  -- ED.ArrayDecl exs -> []
  -- ED.UnopPre op ex -> []
  -- ED.UnopPost op ex -> []
  ED.Binop left op right -> do
    insL <- expr left
    insR <- expr right
    let (r0 : r1 : _) = getTyRegs $ Expr.eval e
    let insOp = case op of
          Op.AddOrPlus -> ADD r0 (Reg r1)
          Op.SubOrNeg -> SUB r0 (Reg r1)
          Op.MultOrIndir -> MUL r0 (Reg r1)
          Op.Div -> DIV r0 (Reg r1)
          _ -> error $ "Operator not implemented : " ++ show op
    return $ insR ++ [PUSH (Reg r0)] ++ insL ++ [POP r1, insOp]
  -- ED.Ternary ter_cond ter_then ter_else -> []
  -- ED.Call ex args -> []
  ED.Parenthese ex -> expr ex
  ED.Invalid str -> error $ "Invalid expression : " ++ str
  _ -> error $ "Expression not implemented yet : " ++ show e

