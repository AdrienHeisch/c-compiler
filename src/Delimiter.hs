module Delimiter (Delimiter (..), toStrOpen, toStrClose) where

data Delimiter
  = Pr
  | Br
  | SqBr
  deriving (Show, Eq)

toStrOpen :: Delimiter -> String
toStrOpen = toStr False

toStrClose :: Delimiter -> String
toStrClose = toStr True

toStr :: Bool -> Delimiter -> String
toStr close del = case (close, del) of
  (False, Pr) -> "("
  (True, Pr) -> ")"
  (False, Br) -> "{"
  (True, Br) -> "}"
  (False, SqBr) -> "["
  (True, SqBr) -> "]"