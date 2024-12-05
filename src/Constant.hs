module Constant (Constant (..), IntRepr (..), StrRepr (..)) where

import Type (Type)

data Constant t
  = Constant {ty :: Type, val :: t}
  deriving (Show, Eq)

newtype IntRepr = IntRepr Int
  deriving (Show, Eq)

newtype StrRepr = StrRepr String
  deriving (Show, Eq)