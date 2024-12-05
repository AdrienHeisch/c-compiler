module Statement (Statement (..)) where

import Expr (Expr)
import Identifier (Id)
import Type (Type (..))

data Statement
  = Empty
  | Expr Expr
  | Var Type Id (Maybe Expr)
  | Block [Statement]
  | If {cond :: Expr, then_ :: Statement, else_ :: Maybe Statement}
  | Switch -- TODO
  | While {cond :: Expr, body :: Statement}
  | DoWhile {body :: Statement, cond :: Expr}
  | For {fdecl :: Maybe Expr, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement}
  | ForVar {decl :: Statement, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement}
  | Break
  | Continue
  | Return (Maybe Expr)
  | Goto -- TODO
  | Label -- TODO
  | Invalid String
  deriving (Show)