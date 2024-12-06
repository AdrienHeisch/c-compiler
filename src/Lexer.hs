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

lex :: Text -> [Token]
lex text = reduceTokens (lexFrom text 0)

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
takeToken text from =
  if from >= Text.length text
    then (Token.Eof, from)
    else do
      let cursor = getCursor text (Text.index text from) from
      case readCursor text cursor of
        "//" -> (Token.Nil, skipLine text (Cursor.end cursor))
        "/*" -> (Token.Nil, skipBlock text (Cursor.end cursor))
        str -> (Token.make str, Cursor.end cursor)

getCursor :: Text -> Char -> Int -> Cursor
getCursor text first idx = do
  let crsr = makeCursor idx
  case first of
    _ | first `elem` CC.identifierStart -> getIdent crsr
    _ | first `elem` CC.digits -> getNumLit crsr
    _ | first `elem` CC.punctuators -> getPunct crsr
    '"' -> getStrLit crsr
    '\'' -> getCharLit crsr
    ' ' -> getWhite crsr
    '\n' -> getNewLine crsr
    _ -> crsr
  where
    makeCursor start_idx = Cursor start_idx 1
    peek cursor = Text.index text (Cursor.end cursor)
    getIdent cursor = case peek cursor of
      c | c `elem` CC.identifier -> getIdent $ Cursor.expand cursor
      _ -> cursor
    getNumLit cursor = case peek cursor of
      c | c `elem` CC.float -> getNumLit $ Cursor.expand cursor
      _ -> cursor
    getPunct cursor = case peek cursor of
      c | c `elem` CC.punctuators && Cursor.len cursor < 2 -> getPunct $ Cursor.expand cursor
      _ -> cursor
    getStrLit cursor = case peek cursor of
      c | c == '"' -> Cursor.expand cursor
      _ -> getStrLit $ Cursor.expand cursor
    getCharLit cursor = case peek cursor of
      '\'' -> Cursor.expand cursor
      '\\' -> getCharLit $ Cursor.expand $ Cursor.expand cursor
      _ -> getCharLit $ Cursor.expand cursor
    getWhite cursor = case peek cursor of
      c | c == ' ' -> getWhite $ Cursor.expand cursor
      _ -> cursor
    getNewLine cursor = case peek cursor of
      c | c == '\n' -> getNewLine $ Cursor.expand cursor
      _ -> cursor

readCursor :: Text -> Cursor -> String
readCursor text cursor = Text.unpack (Text.take (Cursor.len cursor) (Text.drop (Cursor.idx cursor) text))

skipLine :: Text -> Int -> Int
skipLine text idx =
  if idx >= Text.length text || Text.index text idx == '\n'
    then idx
    else skipLine text (idx + 1)

skipBlock :: Text -> Int -> Int
skipBlock text idx =
  if idx + 1 >= Text.length text || readCursor text (Cursor idx 2) == "*/"
    then idx + 2
    else skipBlock text (idx + 1)