module Cursor (Cursor (..), end, expand) where

data Cursor = Cursor {idx :: Int, len :: Int}

end :: Cursor -> Int
end cursor = idx cursor + len cursor

expand :: Cursor -> Cursor
expand cursor = Cursor {idx = idx cursor, len = len cursor + 1}