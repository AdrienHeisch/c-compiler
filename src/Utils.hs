module Utils where

import Control.Monad.State.Lazy (State, StateT, get, put, runState)
import Data.Word (Word8)
import Data.Bits (Bits (shiftR, (.&.)))

mtransform :: State a b -> StateT a IO b
mtransform st = do
  (values, newState) <- runState st <$> get
  put newState
  return values

genErrs :: (Display a) => (a -> Bool) -> [a] -> [String]
genErrs f = map display . filter f

listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

maybeListToList :: Maybe [a] -> [a]
maybeListToList Nothing = []
maybeListToList (Just xs) = xs

mapFst :: (t -> a) -> (t, b) -> (a, b)
mapFst f (a, b) = (f a, b)

withSplit :: [a] -> ([a] -> [a] -> t) -> Int -> t
withSplit l f n = let (tk, dp) = splitAt n l in f tk dp

withSplitTpl :: [a] -> ([a] -> t) -> Int -> (t, [a])
withSplitTpl l f n = let (tk, dp) = splitAt n l in (f tk, dp)

modifyFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
modifyFirst _ _ [] = []
modifyFirst predicate transformer (x : xs)
  | predicate x = transformer x : xs
  | otherwise = x : modifyFirst predicate transformer xs

intoByte :: (Integral a) => a -> Word8
intoByte n = fromIntegral n :: Word8

intoBytes :: (Integral a, Bits a) => Int -> a -> [Word8]
intoBytes len n = [fromIntegral ((n `shiftR` (i * 8)) .&. 0xFF) | i <- [0 .. len - 1]]

unreachable :: a
unreachable = error "Unreachable code, or so I thought"

class Display a where
  display :: a -> String

instance (Display a) => Display [a] where
  display :: [a] -> String
  display l = "[" ++ go l ++ "]"
    where
      go [] = ""
      go [x] = display x
      go (x : xs) = display x ++ ", " ++ go xs

instance (Display a) => Display (Maybe a) where
  display :: Maybe a -> String
  display Nothing = "_"
  display (Just a) = display a