module Expr (Expr (..), ExprDef (..), InitializerKind (..), eval, errs) where

import Constant (Constant (..), FltRepr, IntRepr, StrRepr)
import Data.List (intercalate)
import Identifier (Id)
import Op (Op)
import Token (Token, foldCrs)
import Type (Type)
import Type qualified (Type (..))
import Utils (Display (..))

data Expr = Expr {def :: ExprDef, tks :: [Token]}
  deriving (Show)

instance Display Expr where
  display :: Expr -> String
  display = display . def

eval :: Expr -> Either Type String
eval expr = case Expr.def expr of
  Id _ -> Right "Can't evaluate identifier"
  IntLiteral (Constant ty _) -> Left ty
  FltLiteral (Constant ty _) -> Left ty
  StrLiteral (Constant ty _) -> Left ty
  Initializer _ -> Right "Can't evaluate initializer" -- Type.Struct Nothing (map (second eval) exs) -- FIXME eval whole array
  UnopPre _ ex -> eval ex -- TODO probably wrong
  UnopPost _ ex -> eval ex -- TODO probably wrong
  Binop left _ right -> case eval left of -- TODO probably wrong
    Left Type.Infer -> eval right
    ret -> ret
  Ternary _ ter_then ter_else -> case eval ter_then of -- TODO probably wrong
    Left Type.Infer -> eval ter_else
    ret -> ret
  Call ex _ -> call (eval ex)
  Parenthese ex -> eval ex
  SizeofType _ -> Left Type.Int
  Invalid str -> Right $ "Evaluating invalid expression : " ++ str
  where
    call :: Either Type String -> Either Type String
    call mty = case mty of
      Left (Type.Function ty _) -> Left ty
      Left (Type.Pointer ty) -> call (Left ty)
      Left ty -> Right $ "Tried to call a value of type " ++ show ty
      Right err -> Right err

data ExprDef
  = Id Id
  | IntLiteral (Constant IntRepr)
  | FltLiteral (Constant FltRepr)
  | StrLiteral (Constant StrRepr)
  | Initializer [(InitializerKind, Expr)]
  | UnopPre {op :: Op, ex :: Expr}
  | UnopPost {op :: Op, ex :: Expr}
  | Binop {left :: Expr, op :: Op, right :: Expr}
  | Ternary {ter_cond :: Expr, ter_then :: Expr, ter_else :: Expr}
  | Call {ex :: Expr, args :: [Expr]}
  | Parenthese Expr
  | SizeofType Type
  | Invalid String
  deriving (Show)

instance Display ExprDef where
  display :: ExprDef -> String
  display expr = case expr of
    Initializer exs -> "Initializer (" ++ intercalate ", " (map (\(ik, e) -> show ik ++ ": " ++ display e) exs) ++ ")"
    UnopPre op ex -> "UnopPre (" ++ unwords [show op, display ex] ++ ")"
    UnopPost op ex -> "UnopPost (" ++ unwords [show op, display ex] ++ ")"
    Binop left op right -> "Binop (" ++ unwords [display left, show op, display right] ++ ")"
    Ternary ter_cond ter_then ter_else -> "Ternary (" ++ unwords [display ter_cond, display ter_then, display ter_else] ++ ")"
    Call ex args -> "Call (" ++ unwords [display ex, display args] ++ ")"
    Parenthese ex -> "Parenthese (" ++ display ex ++ ")"
    _ -> show expr

errs :: [Expr] -> [String]
errs = concatMap err
  where
    err :: Expr -> [String]
    err expr = case def expr of
      Initializer exs -> errs (map snd exs)
      UnopPre _ ex -> err ex
      UnopPost _ ex -> err ex
      Binop left _ right -> errs [left, right]
      Ternary ter_cond ter_then ter_else -> errs [ter_cond, ter_then, ter_else]
      Call ex args -> err ex ++ errs args
      Parenthese ex -> err ex
      Invalid str -> [str ++ " at " ++ show (foldCrs $ tks expr)]
      _ -> []

data InitializerKind
  = Simple
  | Field Id
  | Index Expr
  deriving (Show)