module Expr (Expr (..), ExprDef (..), errs) where

import Constant (Constant, FltRepr, IntRepr, StrRepr)
import Identifier (Id)
import Op (Op)
import Utils (genErrs, Display (..))
import Token (Token)

data Expr = Expr {def :: ExprDef, tks :: [Token]}
  deriving (Show)

instance Display Expr where
  display :: Expr -> String
  display = display . def

errs :: [Expr] -> [String]
errs = genErrs isInvalid
  where
    isInvalid (Expr (Invalid _) _)  = True
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

instance Display ExprDef where
  display :: ExprDef -> String
  display expr = case expr of
    ArrayDecl exs -> unwords ["Var", display exs]
    UnopPre op ex -> unwords ["UnopPre", show op, display ex]
    UnopPost op ex -> unwords ["UnopPost", show op, display ex]
    Binop left op right -> unwords ["Binop", display left, show op, display right]
    Ternary ter_cond ter_then ter_else -> unwords ["Ternary", display ter_cond, display ter_then, display ter_else]
    Call ex args -> unwords ["Call", display ex, display args]
    Parenthese ex -> unwords ["Parenthese", display ex]
    _ -> show expr
