module Identifier (Id (..)) where

newtype Id = Id String
  deriving (Show, Eq)
