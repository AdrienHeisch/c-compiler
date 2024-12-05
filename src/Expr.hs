module Expr (Expr (..)) where

import Identifier (Id)
import Op (Op)

data Expr
  = Id Id
  | NumLiteral Int
  | StrLiteral String
  | ArrayDecl [Expr]
  | UnopPre {op :: Op, expr :: Expr}
  | UnopPost {op :: Op, expr :: Expr}
  | Binop {left :: Expr, op :: Op, right :: Expr}
  | Ternary {ter_cond :: Expr, ter_then :: Expr, ter_else :: Expr}
  | Call {expr :: Expr, args :: [Expr]}
  | Parenthese Expr
  | Invalid String
  deriving (Show)
