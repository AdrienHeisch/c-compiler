module Expr (Expr (..), ExprDef (..), eval, errs) where

import Constant (Constant (..), FltRepr, IntRepr, StrRepr)
import Identifier (Id)
import Op (Op)
import Token (Token)
import Type (Type)
import Type qualified (Type (..))
import Utils (Display (..), genErrs)

data Expr = Expr {def :: ExprDef, tks :: [Token]}
  deriving (Show)

instance Display Expr where
  display :: Expr -> String
  display = display . def

errs :: [Expr] -> [String]
errs = genErrs isInvalid
  where
    isInvalid (Expr (Invalid _) _) = True
    isInvalid _ = False

eval :: Expr -> Type
eval expr = case Expr.def expr of
  Id _ -> Type.Infer
  IntLiteral (Constant ty _) -> ty
  FltLiteral (Constant ty _) -> ty
  StrLiteral (Constant ty _) -> ty
  ArrayDecl [] -> error "Empty arrays evaluation not implemented"
  ArrayDecl arr@(ex : _) -> Type.Array (eval ex) (length arr)
  UnopPre op ex -> error "Unop evaluation not implemented"
  UnopPost op ex -> error "Unop evaluation not implemented"
  Binop left op right -> eval left -- TODO might be wrong ? divisions ?
  Ternary ter_cond ter_then ter_else -> error "Ternary evaluation not implemented"
  Call ex _ -> eval ex
  Parenthese ex -> eval ex
  Invalid str -> error $ "Evaluating invalid expression : " ++ str

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
    ArrayDecl exs -> "Var ('" ++ display exs ++ ")"
    UnopPre op ex -> "UnopPre (" ++ unwords [show op, display ex] ++ ")"
    UnopPost op ex -> "UnopPost (" ++ unwords [show op, display ex] ++ ")"
    Binop left op right -> "Binop (" ++ unwords [display left, show op, display right] ++ ")"
    Ternary ter_cond ter_then ter_else -> "Ternary (" ++ unwords [display ter_cond, display ter_then, display ter_else] ++ ")"
    Call ex args -> "Call (" ++ unwords [display ex, display args] ++ ")"
    Parenthese ex -> "Parenthese (" ++ display ex ++ ")"
    _ -> show expr
