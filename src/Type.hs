module Type (Type (..)) where

import Identifier (Id)

data Type
  = Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Pointer Type
  | Array Type Int
  | ArrayNoHint Type
  | Struct (Maybe Id)
  deriving (Show, Eq)