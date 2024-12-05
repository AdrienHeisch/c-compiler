module Declaration (Declaration (..)) where

import Identifier (Id)
import Statement (Statement)
import Type (Type (..))

data Declaration
  = Directive
  | FuncDef Type Id [(Type, Id)]
  | FuncDec Type Id [(Type, Id)] [Statement]
  | Global
  | Static
  | Struct (Maybe Id) [(Type, Id)]
  | Invalid String
  deriving (Show)