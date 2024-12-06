module Declaration (Declaration (..)) where

import Identifier (Id)
import Statement (Statement)
import Type (Type (..))
import Expr (Expr)

data Declaration
  = Directive
  | FuncDef Type Id [(Type, Id)]
  | FuncDec Type Id [(Type, Id)] [Statement]
  | Global
  | Static
  | Struct (Maybe Id) [(Type, Id)]
  | Enum (Maybe Id) Type [(Id, Maybe Expr)]
  | Invalid String
  deriving (Show)