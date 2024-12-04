module Identifier (Id(Id)) where

newtype Id = Id String
  deriving (Show, Eq)

-- toString :: Id -> String
-- toString identifier = Id( )