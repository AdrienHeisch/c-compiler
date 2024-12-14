module Lexer (Lexer.lex) where

import CharClasses qualified as CC
import Cursor (Cursor (..), CursorOps (..))
import Cursor qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Identifier (Id (Id))
import Op qualified
import Token (Token (..))
import Token qualified
import Token qualified as TD (TokenDef (..))

lex :: Text -> [Token]
lex text = reduceTokens $ lexFrom text 0

reduceTokens :: [Token] -> [Token]
reduceTokens tokens = case tokens of
  [] -> []
  Token TD.NL cl
    : Token TD.NL cr
    : rest ->
      reduceTokens (Token TD.NL (cl |+| cr) : rest)
  Token (Token.Op Op.Lt) cl
    : Token (Token.Id (Id str)) _
    : Token (Token.Op Op.MemberPtr) _
    : Token (Token.Id (Id str')) _
    : Token (Token.Op Op.Gt) cr
    : rest ->
      reduceTokens (Token (TD.ImplInclude (str ++ "." ++ str')) (cl |+| cr) : rest)
  tk
    : rest ->
      tk : reduceTokens rest

lexFrom :: Text -> Int -> [Token]
lexFrom text from = case def token of
  Token.Eof -> []
  _ -> token : lexFrom text from'
  where
    (token, from') = takeToken text from

takeToken :: Text -> Int -> (Token, Int)
takeToken text from
  | from >= Text.length text = (Token Token.Eof (Cursor from 0), from)
  | otherwise =
      let cursor = getCursor text from
       in case readCursor text cursor of
            "//" -> (Token Token.Nil cursor, skipLine text (Cursor.end cursor))
            "/*" -> (Token Token.Nil cursor, skipBlock text (Cursor.end cursor))
            str -> (Token (Token.makeDef str) cursor, Cursor.end cursor)

getCursor :: Text -> Int -> Cursor
getCursor text idx = go (Cursor idx 1)
  where
    go :: Cursor -> Cursor
    go cursor
      | first == '\n' = getWhile cursor (== '\n')
      | first == '"' = getUntil cursor '"'
      | first == '\'' = getUntil cursor '\''
      | first == '#' = getWhile cursor $ \c -> c `elem` CC.identifier
      | first `elem` CC.whitespace = getWhile cursor $ \c -> c `elem` CC.whitespace
      | first `elem` CC.identifierStart = getWhile cursor $ \c -> c `elem` CC.identifier
      | first `elem` CC.digits = getWhile cursor $ \c -> c `elem` CC.float
      | first `elem` CC.punctuators = makePunctuator cursor
      | otherwise = cursor

    first :: Char
    first = Text.index text idx

    peek :: Cursor -> Maybe Char
    peek cursor =
      let end = Cursor.end cursor
       in if end < len
            then Just $ Text.index text (Cursor.end cursor)
            else Nothing

    len :: Int
    len = Text.length text

    getUntil :: Cursor -> Char -> Cursor
    getUntil cursor terminator =
      case peek cursor of
        Just c | c == terminator -> Cursor.expand cursor
        _ -> getUntil (Cursor.expand cursor) terminator

    getWhile :: Cursor -> (Char -> Bool) -> Cursor
    getWhile cursor predicate =
      case peek cursor of
        Just c | predicate c -> getWhile (Cursor.expand cursor) predicate
        _ -> cursor

    makePunctuator :: Cursor -> Cursor
    makePunctuator cursor =
      case peek cursor of
        Just c
          | let str = readCursor text cursor ++ [c]
             in Op.strIsOperator str || str == "/*" || str == "//" ->
              makePunctuator (Cursor.expand cursor)
        _ -> cursor

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