module Utils where

genErrs :: (Show a) => (a -> Bool) -> [a] -> [String]
genErrs f = map show . filter f

listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

mapFst :: (t -> a) -> (t, b) -> (a, b)
mapFst f (a, b) = (f a, b)

withSplit :: [a] -> ([a] -> [a] -> t) -> Int -> t
withSplit l f n = let (tk, dp) = splitAt n l in f tk dp

withSplitTpl :: [a] -> ([a] -> t) -> Int -> (t, [a])
withSplitTpl l f n = let (tk, dp) = splitAt n l in (f tk, dp)

class Display a where
    display :: a -> String

instance Display a => Display [a] where
  display :: [a] -> String
  display l = "[" ++ go l ++ "]"
    where
      go [] = ""
      go [x] = display x
      go (x : xs) = display x ++ ", " ++ go xs

instance Display a => Display (Maybe a) where
  display :: Maybe a -> String
  display m = "[" ++ go m ++ "]"
    where
      go Nothing = "_"
      go (Just a) = display a