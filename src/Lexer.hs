module Lexer (Lexer.lex) where

import CharClasses qualified as CC
import Cursor (Cursor (Cursor))
import Cursor qualified (end, expand)
import Data.Text as Text (Text)
import Data.Text qualified as Text (drop, index, length, take, unpack)
import Identifier (Id (Id))
import Op qualified
import Token (Token)
import Token qualified

lex :: Text -> [Token]
lex text = reduceTokens $ lexFrom text 0

reduceTokens :: [Token] -> [Token]
reduceTokens tokens = case tokens of
  [] -> []
  (Token.NL : Token.NL : rest) -> reduceTokens (Token.NL : rest)
  (Token.Op Op.Lt : Token.Id (Id str) : Token.Op Op.MemberPtr : Token.Id (Id str') : Token.Op Op.Gt : rest) ->
    reduceTokens (Token.ImplInclude (str ++ "." ++ str') : rest)
  (tk : rest) -> tk : reduceTokens rest

lexFrom :: Text -> Int -> [Token]
lexFrom text from = case takeToken text from of
  (Token.Eof, _) -> [Token.Eof]
  (Token.Nil, from') -> lexFrom text from'
  (token, from') -> token : lexFrom text from'

takeToken :: Text -> Int -> (Token, Int)
takeToken text from
  | from >= Text.length text = (Token.Eof, from)
  | otherwise =
      let cursor = getCursor text from
       in case readCursor text cursor of
            "//" -> (Token.Nil, skipLine text (Cursor.end cursor))
            "/*" -> (Token.Nil, skipBlock text (Cursor.end cursor))
            str -> (Token.make str, Cursor.end cursor)

getCursor :: Text -> Int -> Cursor
getCursor text idx = go (Cursor idx 1)
  where
    go :: Cursor -> Cursor
    go cursor
      | first == ' ' = getWhile cursor (== ' ')
      | first == '\n' = getWhile cursor (== '\n')
      | first == '"' = getUntil cursor '"'
      | first == '\'' = getUntil cursor '\''
      | first `elem` CC.identifierStart = getWhile cursor $ \c -> c `elem` CC.identifier
      | first `elem` CC.digits = getWhile cursor $ \c -> c `elem` CC.float
      | first `elem` CC.punctuators = getWhile cursor $ \c -> c `elem` CC.punctuators
      | otherwise = cursor

    first :: Char
    first = Text.index text idx

    peek :: Cursor -> Char
    peek cursor = Text.index text (Cursor.end cursor)

    getUntil :: Cursor -> Char -> Cursor
    getUntil cursor terminator =
      if peek cursor == terminator
        then Cursor.expand cursor
        else getUntil (Cursor.expand cursor) terminator

    getWhile :: Cursor -> (Char -> Bool) -> Cursor
    getWhile cursor predicate =
      if predicate (peek cursor)
        then getWhile (Cursor.expand cursor) predicate
        else cursor

readCursor :: Text -> Cursor -> String
readCursor text (Cursor idx len) = Text.unpack $ Text.take len $ Text.drop idx text

skipLine :: Text -> Int -> Int
skipLine text = go
  where
    go idx
      | idx >= len || Text.index text idx == '\n' = idx
      | otherwise = go (idx + 1)
    len = Text.length text

skipBlock :: Text -> Int -> Int
skipBlock text = go
  where
    go idx
      | idx + 1 >= len || readCursor text (Cursor idx 2) == "*/" = idx + 2
      | otherwise = skipBlock text (idx + 1)
    len = Text.length text