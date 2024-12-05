module Statement (Statement (..)) where

import Expr (Expr)
import Identifier (Id)
import Type (Type (..))
import Constant (Constant, IntRepr)

data Statement
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
  | Goto -- TODO
  | Label -- TODO
  | Invalid String
  deriving (Show)