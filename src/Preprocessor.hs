module Preprocessor (process) where

import Constant (Constant (..))
import Data.Text.IO qualified as TIO
import Delimiter qualified as Dl
import Identifier (Id (..))
import Lexer qualified
import Op qualified
import System.FilePath (combine, normalise, takeDirectory)
import Token (Token)
import Token qualified (filterNil, toStr)
import Token qualified as Tk (Token (..))
import Type qualified as Ty (Type (..))
import Utils (collectUntil, collectUntilDelimiter)

data Directive
  = Include Bool String
  | Define Id [Id] [Token]
  deriving (Show)

process :: FilePath -> IO [Token]
process filePath = do
  (directives, rest) <- addFile filePath
  rest' <- applyDirectives filePath directives rest
  let rest'' = Token.filterNil rest'
   in return rest''

addFile :: FilePath -> IO ([Directive], [Token])
addFile filePath = do
  source <- TIO.readFile filePath
  let tokens = Lexer.lex source
      (directives, rest) = parseDirectives [] tokens
  return (directives, rest)

-- TODO force directive at fist position of line
parseDirectives :: [Directive] -> [Token] -> ([Directive], [Token])
parseDirectives directives tokens = case tokens of
  [] -> (directives, [])
  ( Tk.Directive name
      : tks
    ) ->
      let (tks', rest) = collectDirective tks
       in next rest $ case name of
            "include" -> parseInclude tks'
            "define" -> parseDefine tks'
            _ -> error $ "Unknown directive #" ++ name
  (tk : tks) ->
    let (directives', rest) = parseDirectives directives tks
     in (directives', tk : rest)
  where
    next rest directive =
      let (directives', rest') = parseDirectives directives rest
       in (directive : directives', rest')

collectDirective :: [Token] -> ([Token], [Token])
collectDirective = collectUntil Tk.NL

cleanupTemplate :: [Token] -> [Token]
cleanupTemplate tokens = case tokens of
  [] -> []
  (Tk.Directive name : tks) -> Tk.Stringize : Tk.Id (Id name) : cleanupTemplate tks
  (tk : tks) -> tk : cleanupTemplate tks

applyDirectives :: FilePath -> [Directive] -> [Token] -> IO [Token]
applyDirectives sourcePath directives tokens = case directives of
  [] -> return tokens
  (directive : rest) -> do
    newTokens <- applyDirective sourcePath directive tokens
    applyDirectives sourcePath rest newTokens

applyDirective :: FilePath -> Directive -> [Token] -> IO [Token]
applyDirective sourcePath directive tokens = case directive of
  Include True _ -> error "Standard library not implemented"
  Include False fileName -> applyInclude sourcePath fileName tokens
  Define name params template -> return (applyDefine name params template tokens)

parseInclude :: [Token] -> Directive
parseInclude tokens = case tokens of
  [Tk.Nil, Tk.ImplInclude fileName] -> Include True fileName
  [Tk.Nil, Tk.StrLiteral (Constant _ fileName)] -> Include False fileName
  _ -> error "Invalid include"

parseDefine :: [Token] -> Directive
parseDefine tokens = case tokens of
  (Tk.Nil : Tk.Id name : Tk.DelimOpen Dl.Pr : rest) ->
    let (params, rest') = collectDefineParams rest
     in Define name params (cleanupTemplate rest')
  (Tk.Nil : Tk.Id name : Tk.Nil : rest) -> Define name [] rest
  _ -> error "Define directive expected name"

collectDefineParams :: [Token] -> ([Id], [Token])
collectDefineParams tokens = case Token.filterNil tokens of
  [] -> ([], [])
  (Tk.Id name : Tk.DelimClose Dl.Pr : tks) -> ([name], tks)
  (Tk.Id name : Tk.Op Op.Comma : tks) ->
    let (names, rest) = collectDefineParams tks
     in (name : names, rest)
  (tk : _) -> error $ "Expected identifier, got : " ++ show tk

applyInclude :: FilePath -> FilePath -> [Token] -> IO [Token]
applyInclude source include tokens = do
  (directives, includeTokens) <- addFile includePath
  let newTokens = includeTokens ++ tokens
  applyDirectives includePath directives newTokens
  where
    includePath :: FilePath
    includePath = normalise $ combine (takeDirectory source) include

applyDefine :: Id -> [Id] -> [Token] -> [Token] -> [Token]
applyDefine name params template = apply
  where
    apply :: [Token] -> [Token]
    apply tokens = case tokens of
      [] -> []
      (Tk.Id name_ : Tk.DelimOpen Dl.Pr : tks)
        | name == name_ ->
            let (args, rest) = collectArgs tks
                replacement = applyArgs args template
             in replacement ++ applyDefine name params template rest
      (Tk.Id name_ : tks)
        | name == name_ -> template ++ applyDefine name params template tks
      (tk : tks) -> tk : applyDefine name params template tks

    applyArgs :: [[Token]] -> [Token] -> [Token]
    applyArgs args tokens
      -- FIXME why does this make the program hang
      -- \| length args < length params = error "Not enough arguments"
      -- \| length args > length params = error "Too many arguments"
      | null args || null params = tokens
      | otherwise = go 0 args tokens
      where
        lenParams :: Int
        lenParams = length params

        go :: Int -> [[Token]] -> [Token] -> [Token]
        go idx args' tokens' = case args' of
          [] -> tokens'
          (arg : rest) ->
            let replacement = applyArg (params !! idx) arg tokens'
                newIdx = idx + 1
             in if newIdx < lenParams
                  then go newIdx rest replacement
                  else replacement

    applyArg :: Id -> [Token] -> [Token] -> [Token]
    applyArg param arg = go
      where
        go :: [Token] -> [Token]
        go tokens = case tokens of
          [] -> []
          (Tk.Stringize : Tk.Id param_ : tks) | param == param_ -> stringize arg : go tks
          (Tk.Id param_ : tks) | param == param_ -> arg ++ go tks
          (tk : tks) -> tk : go tks

    stringize :: [Token] -> Token
    stringize arg = Tk.StrLiteral $ Constant (Ty.Array Ty.Char $ length str) str
      where
        str :: String
        str = toStr arg

        toStr :: [Token] -> String
        toStr tokens = case tokens of
          [] -> ""
          (tk : tks) -> Token.toStr tk ++ toStr tks

    collectArgs :: [Token] -> ([[Token]], [Token])
    collectArgs = go
      where
        go :: [Token] -> ([[Token]], [Token])
        go tokens =
          let (args, rest) = collectUntilDelimiter Dl.Pr tokens
           in (parseArgs args, rest)

        parseArgs :: [Token] -> [[Token]]
        parseArgs tokens =
          let (arg, rest) = collectOne tokens
           in trim arg : parseArgs rest

        -- TODO move to utils and replace in other occurences
        collectOne :: [Token] -> ([Token], [Token])
        collectOne tokens = case tokens of
          [] -> ([], [])
          (Tk.Op Op.Comma : tks) -> collectOne tks
          _ -> collectUntil (Tk.Op Op.Comma) tokens

        trim :: [Token] -> [Token]
        trim = reverse . go' . reverse . go'
          where
            go' tokens = case tokens of
              (Tk.Nil : tks) -> go' tks
              (Tk.NL : tks) -> go' tks
              _ -> tokens