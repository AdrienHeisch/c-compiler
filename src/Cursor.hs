module Cursor (Cursor (..), (|+|), end, expand, Cursor.head, Cursor.tail, Cursor.fold) where

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

(|+|) :: Cursor -> Cursor -> Cursor
Cursor idxL _ |+| Cursor _ lenR = Cursor idxL lenR