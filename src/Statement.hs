module Statement (Statement (..), StatementDef (..), errs) where

import Constant (Constant, IntRepr)
import Cursor (Cursor)
import Expr (Expr)
import Expr qualified (errs)
import Identifier (Id)
import Type (Type (..))

data Statement = Statement {crs :: Cursor, def :: StatementDef}
  deriving (Show)

data StatementDef
  = Empty
  | Expr Expr
  | Var Type Id (Maybe Expr)
  | Block [Statement]
  | If {cond :: Expr, then_ :: Statement, else_ :: Maybe Statement}
  | Switch {eval :: Expr, body :: Statement}
  | While {cond :: Expr, body :: Statement}
  | DoWhile {body :: Statement, cond :: Expr}
  | For {finit :: Maybe Expr, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement}
  | ForVar {fdecl :: Statement, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement}
  | Break
  | Continue
  | Return (Maybe Expr)
  | Case (Constant IntRepr)
  | Goto Id
  | Labeled Id Statement
  | Invalid String
  deriving (Show)

errs :: [Statement] -> [String]
errs = concatMap err
  where
    err :: Statement -> [String]
    err statement = case def statement of
      Expr e -> Expr.errs [e]
      Var _ _ (Just e) -> Expr.errs [e]
      If e st0 (Just st1) -> Expr.errs [e] ++ errs [st0, st1]
      Switch e st -> Expr.errs [e] ++ errs [st]
      While e st -> Expr.errs [e] ++ errs [st]
      DoWhile st e -> Expr.errs [e] ++ errs [st]
      For (Just e0) (Just e1) (Just e2) st -> Expr.errs [e0, e1, e2] ++ errs [st]
      ForVar st0 (Just e0) (Just e1) st1 -> Expr.errs [e0, e1, e1] ++ errs [st0, st1]
      Return (Just e) -> Expr.errs [e]
      Block block -> concatMap err block
      Invalid str -> [str ++ " at " ++ show (crs statement)]
      _ -> []