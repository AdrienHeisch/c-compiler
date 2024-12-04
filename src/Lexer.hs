module Lexer (Lexer.lex) where

import Cursor (Cursor (Cursor))
import Cursor qualified (end, expand, idx, len)
import Data.Function
import Data.Text as Text (Text)
import Data.Text qualified as Text (drop, index, length, take, unpack)
import Op
import Token (Token)
import Token qualified

lex :: Text -> [Token]
lex text = lexFrom text 0

lexFrom :: Text -> Int -> [Token]
lexFrom text from =
  case getToken text from of
    (Token.Eof, _) -> [Token.Eof]
    (token, from') -> token : lexFrom text from' & filter (/= Token.Nil)

getToken :: Text -> Int -> (Token, Int)
getToken text from | from >= Text.length text = (Token.Eof, from)
getToken text from =
  let (first, cursor) = (Text.index text from, Cursor from 1)
   in getCursor text first cursor & \cursor' -> (readCursor text cursor' & Token.make, Cursor.end cursor')

getCursor :: Text -> Char -> Cursor -> Cursor
getCursor text first cursor | first `elem` Token.identBeginChars = case Text.index text (Cursor.end cursor) of
  c | c `elem` Token.identChars -> getCursor text first (Cursor.expand cursor)
  _ -> cursor
getCursor text first cursor | first `elem` opChars = case Text.index text (Cursor.len cursor) of
  c | c `elem` opChars -> getCursor text first (Cursor.expand cursor)
  _ -> cursor
getCursor text first cursor | first == '"' = case Text.index text (Cursor.end cursor) of
  c | c == '"' -> Cursor.expand cursor
  _ -> getCursor text first (Cursor.expand cursor)
getCursor text first cursor | first `elem` Token.whiteChars = case Text.index text (Cursor.len cursor) of
  c | c `elem` Token.whiteChars -> getCursor text first (Cursor.expand cursor)
  _ -> cursor
getCursor _ _ cursor = cursor

readCursor :: Text -> Cursor -> String
readCursor text cursor = Text.unpack (Text.take (Cursor.len cursor) (Text.drop (Cursor.idx cursor) text))