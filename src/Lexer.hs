module Lexer (Lexer.lex) where

import CharClasses qualified as CC
import Cursor (Cursor (Cursor))
import Cursor qualified (end, expand, idx, len)
import Data.Text as Text (Text)
import Data.Text qualified as Text (drop, index, length, take, unpack)
import Identifier (Id (Id))
import Op qualified
import Token (Token)
import Token qualified
import Type qualified

lex :: Text -> [Token]
lex text = reduceTokens (lexFrom text 0)

reduceTokens :: [Token] -> [Token]
reduceTokens [] = []
reduceTokens [tk] = [tk]
reduceTokens (Token.NL : Token.NL : tks) = reduceTokens (Token.NL : tks)
reduceTokens (Token.Type ty : Token.Op Op.MultOrIndir : tks) = reduceTokens (Token.Type (Type.Pointer ty) : tks)
reduceTokens (Token.Op Op.Lt : Token.Id (Id str) : Token.Op Op.StructRef : Token.Id (Id str') : Token.Op Op.Gt : tks) =
  reduceTokens (Token.ImplInclude (str ++ "." ++ str') : tks)
reduceTokens (tk : tks) = tk : reduceTokens tks

lexFrom :: Text -> Int -> [Token]
lexFrom text from =
  case takeToken text from of
    (Token.Eof, _) -> [Token.Eof]
    (Token.Nil, from') -> lexFrom text from'
    (token, from') -> token : lexFrom text from'

takeToken :: Text -> Int -> (Token, Int)
takeToken text from | from >= Text.length text = (Token.Eof, from)
takeToken text from =
  let (first, cursor) = (Text.index text from, Cursor from 1)
   in let cursor' = getCursor text first cursor
       in case readCursor text cursor' of
            "//" -> (Token.Nil, skipLine text (Cursor.end cursor'))
            "/*" -> (Token.Nil, skipBlock text (Cursor.end cursor'))
            str -> (Token.make str, Cursor.end cursor')

getCursor :: Text -> Char -> Cursor -> Cursor
getCursor text first cursor = case first of
  _ | first `elem` CC.identifierStart -> getIdent
  _ | first `elem` CC.digits -> getInt
  _ | first `elem` CC.punctuators -> getPunct
  '"' -> getStrLit
  ' ' -> getWhite
  '\n' -> getNewLine
  _ -> cursor
  where
    getCursorNext = getCursor text first
    getIdent = case Text.index text (Cursor.end cursor) of
      c | c `elem` CC.identifier -> getCursorNext (Cursor.expand cursor)
      _ -> cursor
    getInt = case Text.index text (Cursor.end cursor) of
      c | c `elem` CC.digits -> getCursorNext (Cursor.expand cursor)
      _ -> cursor
    getPunct = case Text.index text (Cursor.end cursor) of
      c | c `elem` CC.punctuators && Cursor.len cursor < 2 -> getCursorNext (Cursor.expand cursor)
      _ -> cursor
    getStrLit = case Text.index text (Cursor.end cursor) of
      c | c == '"' -> Cursor.expand cursor
      _ -> getCursorNext (Cursor.expand cursor)
    getWhite = case Text.index text (Cursor.end cursor) of
      c | c == ' ' -> getCursorNext (Cursor.expand cursor)
      _ -> cursor
    getNewLine = case Text.index text (Cursor.end cursor) of
      c | c == '\n' -> getCursorNext (Cursor.expand cursor)
      _ -> cursor

readCursor :: Text -> Cursor -> String
readCursor text cursor = Text.unpack (Text.take (Cursor.len cursor) (Text.drop (Cursor.idx cursor) text))

skipLine :: Text -> Int -> Int
skipLine text idx | idx >= Text.length text || Text.index text idx == '\n' = idx
skipLine text idx = skipLine text (idx + 1)

skipBlock :: Text -> Int -> Int
skipBlock text idx | idx + 1 >= Text.length text || readCursor text (Cursor idx 2) == "*/" = idx + 2
skipBlock text idx = skipBlock text (idx + 1)