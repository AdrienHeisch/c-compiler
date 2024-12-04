module Lexer (Lexer.lex) where

import Data.Char (isAlpha, isDigit)
import Data.Function
import Data.Text as Text (Text)
import Data.Text qualified as Text (drop, empty, head, index, length, null, tail, take, unpack)
import Op
import Token (Token)
import qualified Token

data Cursor = Cursor {idx :: Int, len :: Int}

end :: Cursor -> Int
end cursor = idx cursor + len cursor

lex :: Text -> [Token]
lex text = lexFrom text 0

lexFrom :: Text -> Int -> [Token]
lexFrom text from =
  case getToken text from of
    (Token.Eof, _) -> [Token.Eof]
    (token, from) -> token : lexFrom text from & filter (/= Token.Nil)

getToken :: Text -> Int -> (Token, Int)
getToken text from | from >= Text.length text = (Token.Eof, from)
getToken text from =
  let (head, cursor) = (Text.index text from, Cursor {idx = from, len = 1})
   in expandCursor text head cursor & \cursor -> (readCursor text cursor & Token.make, end cursor)

expandCursor :: Text -> Char -> Cursor -> Cursor
expandCursor text head cursor | head `elem` Token.identBeginChars = case Text.index text (end cursor) of
  c | c `elem` Token.identChars -> expandCursor text head Cursor {idx = idx cursor, len = len cursor + 1}
  _ -> cursor
expandCursor text head cursor | head `elem` opChars = case Text.index text (len cursor) of
  c | c `elem` opChars -> expandCursor text head Cursor {idx = idx cursor, len = len cursor + 1}
  _ -> cursor
expandCursor text head cursor | head == '"' = case Text.index text (end cursor) of
  c | c == '"' -> Cursor {idx = idx cursor, len = len cursor + 1}
  _ -> expandCursor text head Cursor {idx = idx cursor, len = len cursor + 1}
expandCursor text head cursor | head `elem` Token.whiteChars = case Text.index text (len cursor) of
  c | c `elem` Token.whiteChars -> expandCursor text head Cursor {idx = idx cursor, len = len cursor + 1}
  _ -> cursor
expandCursor text head cursor = cursor

readCursor :: Text -> Cursor -> String
readCursor text cursor = Text.unpack (Text.take (len cursor) (Text.drop (idx cursor) text))