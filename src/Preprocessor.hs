module Preprocessor (process) where

import Constant (Constant (..))
import Cursor (Cursor, (|+|))
import Cursor qualified
import Data.List (intercalate)
import Data.Text.IO qualified as TIO
import Delimiter qualified as Dl
import Identifier (Id (..))
import Lexer qualified
import Op qualified
import System.FilePath (combine, normalise, takeDirectory)
import Token (Token (..))
import Token qualified (collectUntil, collectUntilDelimiter, errs, filterNil, defToStr)
import Token qualified as TD (TokenDef (..))
import Type qualified as Ty (Type (..))
import Utils (genErrs)

data Directive
  = Include Bool String
  | Define Id [Id] [Token]
  | Invalid String
  deriving (Show)

errs :: [Directive] -> [String]
errs = genErrs isInvalid
  where
    isInvalid (Invalid _) = True
    isInvalid _ = False

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
      !_ = case Token.errs tokens of
        [] -> ()
        tkErrs -> error $ "Lexer errors :\n" ++ intercalate "\n" tkErrs
      (directives, rest) = parseDirectives [] tokens
      !_ = case errs directives of
        [] -> ()
        directiveErrs -> error $ "Preprocessor errors :\n" ++ intercalate "\n" directiveErrs
  return (directives, rest)

-- TODO force directive at fist position of line
parseDirectives :: [Directive] -> [Token] -> ([Directive], [Token])
parseDirectives directives tokens = case tokens of
  [] -> (directives, [])
  Token cursor (TD.Directive name) : tks ->
    let (tks', rest) = collectDirective tks
     in next rest $ case name of
          "include" -> parseInclude tks'
          "define" -> parseDefine tks'
          _ -> Invalid $ "Unknown directive #" ++ name ++ " at " ++ show cursor
  (tk : tks) ->
    let (directives', rest) = parseDirectives directives tks
     in (directives', tk : rest)
  where
    next rest directive =
      let (directives', rest') = parseDirectives directives rest
       in (directive : directives', rest')

collectDirective :: [Token] -> ([Token], [Token])
collectDirective = Token.collectUntil TD.NL

cleanupTemplate :: [Token] -> [Token]
cleanupTemplate tokens = case tokens of
  [] -> []
  Token crs (TD.Directive name) : tks ->
    Token (Cursor.head crs) TD.Stringize : Token (Cursor.tail crs) (TD.Id (Id name)) : cleanupTemplate tks
  tk : tks -> tk : cleanupTemplate tks

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
  Invalid _ -> error $ "Invalid directive shouldn't be applied : " ++ show directive

parseInclude :: [Token] -> Directive
parseInclude tokens = case map def tokens of
  [TD.Nil, TD.ImplInclude fileName] -> Include True fileName
  [TD.Nil, TD.StrLiteral (Constant _ fileName)] -> Include False fileName
  _ -> Invalid "Invalid include"

parseDefine :: [Token] -> Directive
parseDefine tokens = case map def tokens of
  TD.Nil : TD.Id name : TD.DelimOpen Dl.Pr : _ ->
    case collectDefineParams (drop 3 tokens) of
      Left (params, rest') -> Define name params (cleanupTemplate rest')
      Right err -> Invalid err
  TD.Nil : TD.Id name : TD.Nil : _ -> Define name [] (drop 3 tokens)
  _ -> Invalid "Define directive expected name"

collectDefineParams :: [Token] -> Either ([Id], [Token]) String
collectDefineParams tokens = case map def filtered of
  [] -> Left ([], [])
  TD.Id name : TD.DelimClose Dl.Pr : _ -> Left ([name], drop 2 filtered)
  TD.Id name : TD.Op Op.Comma : _ ->
    case collectDefineParams (drop 2 filtered) of
      Left (names, rest) -> Left (name : names, rest)
      Right err -> Right err
  TD.Id _ : tk : _ -> Right $ "Expected , or ) but got : " ++ show tk ++ " at " ++ show (crs $ filtered !! 1)
  tk : _ -> Right $ "Expected identifier, got : " ++ show tk ++ " at " ++ show (crs $ head filtered)
  where
    filtered = Token.filterNil tokens

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
    apply tokens = case map def tokens of
      [] -> []
      TD.Id name_ : TD.DelimOpen Dl.Pr : _
        | name == name_ ->
            let (args, rest) = collectArgs (drop 2 tokens)
                replacement = applyArgs args template
             in replacement ++ applyDefine name params template rest
      TD.Id name_ : _
        | name == name_ ->
            template ++ applyDefine name params template (tail tokens)
      _ : _ ->
        head tokens : applyDefine name params template (tail tokens)

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
        go tokens = case map def tokens of
          [] -> []
          TD.Stringize : TD.Id param_ : _
            | param == param_ ->
                stringize arg : go (drop 2 tokens)
          TD.Id param_ : _
            | param == param_ ->
                arg ++ go (tail tokens)
          _ : _ -> head tokens : go (tail tokens)

    stringize :: [Token] -> Token
    stringize arg = Token cursor (TD.StrLiteral (Constant (Ty.Array Ty.Char $ length str) str))
      where
        cursor :: Cursor
        cursor = Token.crs (head arg) |+| Token.crs (last arg)

        str :: String
        str = toStr arg

        toStr :: [Token] -> String
        toStr tokens = case tokens of
          [] -> ""
          tk : tks -> (Token.defToStr . Token.def) tk ++ toStr tks

    collectArgs :: [Token] -> ([[Token]], [Token])
    collectArgs = go
      where
        go :: [Token] -> ([[Token]], [Token])
        go tokens =
          let (args, rest) = Token.collectUntilDelimiter Dl.Pr tokens
           in (parseArgs args, rest)

        parseArgs :: [Token] -> [[Token]]
        parseArgs tokens =
          let (arg, rest) = collectOne tokens
           in trim arg : parseArgs rest

        -- TODO move to utils and replace in other occurences
        collectOne :: [Token] -> ([Token], [Token])
        collectOne tokens = case map def tokens of
          [] -> ([], [])
          TD.Op Op.Comma : _ -> collectOne (tail tokens)
          _ -> Token.collectUntil (TD.Op Op.Comma) tokens

        trim :: [Token] -> [Token]
        trim = reverse . go' . reverse . go'
          where
            go' :: [Token] -> [Token]
            go' tokens = case map def tokens of
              TD.Nil : _ -> go' $ tail tokens
              TD.NL : _ -> go' $ tail tokens
              _ -> tokens