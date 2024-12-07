module Utils where

import Token (Token)
import Token qualified (Token (..))
import Token as Delimiter (Delimiter)

listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

collectUntil :: Token -> [Token] -> ([Token], [Token])
collectUntil end tokens = case tokens of
  [] -> ([], [])
  (tk : tks) | tk == end -> ([], tks)
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
        | tk == Token.DelimClose del && depth == 0 -> ([], tks)
        | tk == Token.DelimClose del -> next tk tks (depth - 1)
        | tk == Token.DelimOpen del -> next tk tks (depth + 1)
        | otherwise -> next tk tks depth

    next :: Token -> [Token] -> Int -> ([Token], [Token])
    next tk rest depth =
      let (tks', rest') = go depth rest
       in (tk : tks', rest')

parseList :: Token -> ([Token] -> (el, [Token])) -> [Token] -> [el]
parseList end parser tokens = case tokens of
  [] -> []
  (tk : tks) | tk == end -> parseList end parser tks
  _ ->
    let (el, tks) = parser tokens
     in el : parseList end parser tks