module Constant (Constant (..), IntRepr, StrRepr, FltRepr, BoolRepr) where

import Type (Type)

data Constant repr
  = Constant {ty :: Type, val :: repr}
  deriving (Show, Eq)

type IntRepr = Int

type FltRepr = Float

type StrRepr = String

type BoolRepr = Bool