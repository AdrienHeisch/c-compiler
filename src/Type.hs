module Type (Type (..)) where

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
  deriving (Show, Eq)