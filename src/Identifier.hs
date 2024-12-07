module Identifier (Id (..), toStr) where

newtype Id = Id String
  deriving (Show, Eq)

toStr :: Id -> String
toStr (Id ident) = ident