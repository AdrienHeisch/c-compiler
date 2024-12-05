module Declaration (Declaration (..)) where

import Identifier (Id)
import Statement (Statement)
import Type (Type (..))

data Declaration
  = Directive
  | FuncDef Type Id [(Type, Id)]
  | FuncDec Type Id [(Type, Id)] [Statement] -- TODO parameters
  | Global
  | Static
  | Type
  | Invalid String
  deriving (Show)