module Structures (Id, Declaration (..), Statement (..), Expr (..)) where

import Identifier (Id)
import Op (Op)
import Type (Type (..))

data Declaration
  = Directive
  | FuncDef Type Id [(Type, Id)]
  | FuncDec Type Id [(Type, Id)] [Statement] -- TODO parameters
  | Global
  | Static
  | Type
  | InvalidDeclaration String
  deriving (Show)

data Statement
  = Empty
  | Expr Expr
  | Var Type Id (Maybe Expr)
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
