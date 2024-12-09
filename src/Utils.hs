module Utils where

genErrs :: (Show a) => (a -> Bool) -> [a] -> [String]
genErrs f = map show . filter f

listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

mapFst :: (t -> a) -> (t, b) -> (a, b)
mapFst f (a, b) = (f a, b)