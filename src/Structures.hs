module Structures (Id, Declaration (..), Statement (..), Expr (..)) where

import Identifier (Id)
import Op (Op)
import Type (Type (..))

data Declaration
  = Directive
  | FuncDec Type Id [Statement] -- TODO parameters
  | FuncDef
  | Global
  | Static
  | Type
  | InvalidDeclaration String
  deriving (Show)

data Statement
  = Empty
  | Expr Expr
  | Var Type Id Expr
  | Block [Statement]
  | If {cond :: Expr, then_ :: [Statement], else_ :: Maybe Expr}
  | Switch -- TODO
  | For -- TODO
  | While {cond :: Expr, loop :: [Statement]}
  | DoWhile {cond :: Expr, loop :: [Statement]}
  | Break
  | Continue
  | Return (Maybe Expr)
  | Goto -- TODO
  | Label -- TODO
  | InvalidStatement String
  deriving (Show)

data Expr
  = Id Id
  | NumLiteral String
  | StrLiteral String
  | ArrayDecl [Expr]
  | UnopPre {op :: Op, expr :: Expr}
  | UnopPost {op :: Op, expr :: Expr}
  | Binop {left :: Expr, op :: Op, right :: Expr}
  | Ternary {ter_cond :: Expr, ter_then :: [Statement], ter_else :: Maybe Expr}
  | Parentheses Expr
  | InvalidExpr String
  deriving (Show)
