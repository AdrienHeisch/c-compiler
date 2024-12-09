module Expr (Expr (..), ExprDef (..), errs) where

import Constant (Constant, FltRepr, IntRepr, StrRepr)
import Cursor (Cursor)
import Identifier (Id)
import Op (Op)
import Utils (genErrs)

data Expr = Expr {crs :: Cursor, def :: ExprDef}
  deriving (Show)

errs :: [Expr] -> [String]
errs = genErrs isInvalid
  where
    isInvalid (Expr _ (Invalid _)) = True
    isInvalid _ = False

data ExprDef
  = Id Id
  | IntLiteral (Constant IntRepr)
  | FltLiteral (Constant FltRepr)
  | StrLiteral (Constant StrRepr)
  | ArrayDecl [Expr]
  | UnopPre {op :: Op, ex :: Expr}
  | UnopPost {op :: Op, ex :: Expr}
  | Binop {left :: Expr, op :: Op, right :: Expr}
  | Ternary {ter_cond :: Expr, ter_then :: Expr, ter_else :: Expr}
  | Call {ex :: Expr, args :: [Expr]}
  | Parenthese Expr
  | Invalid String
  deriving (Show)
