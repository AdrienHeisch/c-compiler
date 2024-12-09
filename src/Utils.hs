module Utils where

import Token (Token, TokenDef)
import Token as Delimiter (Delimiter)
import Token qualified (Token (..))
import Token qualified as TokenDef (TokenDef (..))

listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

mapFst :: (t -> a) -> (t, b) -> (a, b)
mapFst f (a, b) = (f a, b)

collectUntil :: TokenDef -> [Token] -> ([Token], [Token])
collectUntil end tokens = case tokens of
  [] -> ([], [])
  (tk : tks) | Token.def tk == end -> ([], tks)
  (tk : tks) ->
    let (tks', rest) = collectUntil end tks
     in (tk : tks', rest)

collectUntilDelimiter :: Delimiter -> [Token] -> ([Token], [Token])
collectUntilDelimiter del = go 0
  where
    go :: Int -> [Token] -> ([Token], [Token])
    go depth tokens = case tokens of
      [] -> ([], [])
      (tk : tks)
        | Token.def tk == TokenDef.DelimClose del && depth == 0 -> ([], tks)
        | Token.def tk == TokenDef.DelimClose del -> next tk tks (depth - 1)
        | Token.def tk == TokenDef.DelimOpen del -> next tk tks (depth + 1)
        | otherwise -> next tk tks depth

    next :: Token -> [Token] -> Int -> ([Token], [Token])
    next tk rest depth =
      let (tks', rest') = go depth rest
       in (tk : tks', rest')

parseList :: TokenDef -> ([Token] -> (el, [Token])) -> [Token] -> [el]
parseList end parser tokens = case tokens of
  [] -> []
  (tk : tks) | Token.def tk == end -> parseList end parser tks
  _ ->
    let (el, tks) = parser tokens
     in el : parseList end parser tks