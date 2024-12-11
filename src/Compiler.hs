module Compiler (compile) where

import Constant (Constant (Constant))
import Context (Context (Context), addVar, getVar)
import Context qualified (new)
import Control.Monad.State.Lazy (State, evalState, get, modify, put)
import Debug.Trace (trace)
import Declaration (Declaration)
import Declaration qualified (Declaration (..))
import Declaration qualified as DD (DeclarationDef (..))
import Expr (Expr)
import Expr qualified (Expr (..), eval)
import Expr qualified as ED (ExprDef (..))
import Identifier (Id)
import Instruction (Instruction (..), Program (..), Register (..))
import Op qualified
import Statement (Statement)
import Statement qualified
import Statement qualified as SD (StatementDef (..))
import Type (Type (Int))

compile :: [Declaration] -> Program
compile decls = Program $ evalState (declarations decls) Context.new

declarations :: [Declaration] -> State Context [Instruction]
declarations decls = do
  context <- get
  let !_ = trace ("in declarations : " ++ show context) ()
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
  put $ Context params (Just context)
  ins <- statements body
  put context
  return $ [CPY BP SP] ++ ins

statements :: [Statement] -> State Context [Instruction]
statements sts = case map Statement.def sts of
  [] -> return []
  SD.Empty : _ -> go []
  SD.Block block : _ -> do ins <- statements block; go ins
  SD.Expr e : _ -> do ins <- expr e; go ins
  SD.Var ty name e : _ -> do ins <- var ty name e; go ins
  -- SD.If {cond :: Expr, then_ :: Statement, else_ :: Maybe Statement} ->
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
      modify (addVar ty name)
      return []
    Just ex -> do
      modify (addVar ty name)
      ins <- expr ex
      return $ case ty of
        Type.Int -> ins ++ [PUSH I0]
        _ -> error $ "Type not implemented in variable assignment : " ++ show ty

expr :: Expr -> State Context [Instruction]
expr e = case Expr.def e of
  ED.Id name -> do
    context <- get
    case getVar context name of
      Nothing -> error $ "Undefined identifier : " ++ show name
      Just (idx, ty) -> 
        return $ case ty of
          Type.Int -> [LOAD I0 idx]
          _ -> error $ "Type not implemented in binop : " ++ show ty
  ED.IntLiteral (Constant Type.Int int) -> do
    context <- get
    let !_ = trace ("in expr : " ++ show context) ()
    return [CONST I0 int]
  -- ED.FltLiteral flt -> []
  -- ED.StrLiteral str -> []
  -- ED.ArrayDecl exs -> []
  -- ED.UnopPre op ex -> []
  -- ED.UnopPost op ex -> []
  ED.Binop left op right -> do
    insL <- expr left
    insR <- expr right
    let insOp = case op of
          Op.AddOrPlus -> ADD I0 I1
          Op.SubOrNeg -> SUB I0 I1
          Op.MultOrIndir -> MUL I0 I1
          Op.Div -> DIV I0 I1
          _ -> error $ "Operator not implemented : " ++ show op
    return $ case Expr.eval left of
      Type.Int -> insR ++ [PUSH I0] ++ insL ++ [POP I0, insOp]
      ty -> error $ "Type not implemented in binop : " ++ show ty
  -- ED.Ternary ter_cond ter_then ter_else -> []
  -- ED.Call ex args -> []
  ED.Parenthese ex -> expr ex
  ED.Invalid str -> error $ "Invalid expression : " ++ str
  _ -> error $ "Expression not implemented yet : " ++ show e
