module Cursor (Cursor (..), CursorOps (..), end, expand, Cursor.head, Cursor.tail, Cursor.fold) where

data Cursor = Cursor {idx :: Int, len :: Int}
  deriving (Show)

end :: Cursor -> Int
end cursor = idx cursor Prelude.+ len cursor

expand :: Cursor -> Cursor
expand cursor = Cursor {idx = idx cursor, len = len cursor Prelude.+ 1}

head :: Cursor -> Cursor
head (Cursor idx _) = Cursor idx 1

tail :: Cursor -> Cursor
tail (Cursor idx len) = Cursor (idx Prelude.+ 1) (len - 1)

fold :: [Cursor] -> Cursor
fold cursors = go (Prelude.head cursors) (Prelude.tail cursors)
  where
    go :: Cursor -> [Cursor] -> Cursor
    go cursor [] = cursor
    go cursor (next : others) = cursor |+| go next others

class CursorOps a where
  (|+|) :: Cursor -> a -> Cursor

instance CursorOps Cursor where
  (|+|) :: Cursor -> Cursor -> Cursor
  left |+| right = Cursor from to
    where
      from = idx left
      to = end right - from