module Expr (Expr (..)) where

import Constant (Constant, FltRepr, IntRepr, StrRepr)
import Identifier (Id)
import Op (Op)

data Expr
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
