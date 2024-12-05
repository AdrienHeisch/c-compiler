module Utils where

listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs