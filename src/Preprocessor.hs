module Preprocessor (process) where

import Constant (Constant (..))
import Control.Monad.State.Lazy (State, StateT (runStateT), evalStateT, get, liftIO, modify, put, runState)
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
import Token qualified (collectUntil, collectUntilDelimiter, defToStr, errs, filterNil)
import Token qualified as TD (TokenDef (..))
import Type qualified as Ty (Type (..))
import Utils (genErrs, mtransform)

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
  (directives, tokens) <- runStateT (addFile filePath) []
  tokens' <- evalStateT (applyDirectives filePath directives) tokens
  return $ Token.filterNil tokens'

addFile :: FilePath -> StateT [Token] IO [Directive]
addFile filePath = do
  source <- liftIO $ TIO.readFile filePath
  tokens <- get
  let tokens' = Lexer.lex source ++ tokens
  put tokens'
  let !_ = case Token.errs tokens' of
        [] -> ()
        tkErrs -> error $ "Lexer errors :\n" ++ intercalate "\n" tkErrs
  directives <- mtransform $ parseDirectives []
  let !_ = case errs directives of
        [] -> ()
        directiveErrs -> error $ "Preprocessor errors :\n" ++ intercalate "\n" directiveErrs
  return directives

-- TODO force directive at first position of line
parseDirectives :: [Directive] -> State [Token] [Directive]
parseDirectives directives = do
  tokens <- get
  case tokens of
    [] -> return directives
    Token (TD.Directive name) cursor : _ -> do
      modify $ drop 1
      directiveTks <- collectDirective
      let directive = case name of
            "include" -> parseInclude directiveTks
            "define" -> parseDefine directiveTks
            _ -> Invalid $ "Unknown directive #" ++ name ++ " at " ++ show cursor
      directives' <- parseDirectives directives
      return $ directive : directives'
    (tk : _) -> do
      modify $ drop 1
      directives' <- parseDirectives directives
      tks <- get
      put $ tk : tks
      return directives'

collectDirective :: State [Token] [Token]
collectDirective = Token.collectUntil TD.NL

cleanupTemplate :: [Token] -> [Token]
cleanupTemplate tokens = case tokens of
  [] -> []
  Token (TD.Directive name) crs : tks ->
    Token TD.Stringize (Cursor.head crs) : Token (TD.Id (Id name)) (Cursor.tail crs) : cleanupTemplate tks
  tk : tks -> tk : cleanupTemplate tks

applyDirectives :: FilePath -> [Directive] -> StateT [Token] IO [Token]
applyDirectives sourcePath directives = case directives of
  [] -> get
  (directive : rest) -> do
    tokens <- applyDirective sourcePath directive
    put tokens
    applyDirectives sourcePath rest

applyDirective :: FilePath -> Directive -> StateT [Token] IO [Token]
applyDirective sourcePath directive = case directive of
  Include True _ -> error "Standard library not implemented"
  Include False fileName -> applyInclude sourcePath fileName
  Define name params template -> mtransform $ applyDefine name params template
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

-- FIXME tokens are appended at the top of the file !
applyInclude :: FilePath -> FilePath -> StateT [Token] IO [Token]
applyInclude source include = do
  directives <- addFile includePath
  applyDirectives includePath directives
  where
    includePath :: FilePath
    includePath = normalise $ combine (takeDirectory source) include

applyDefine :: Id -> [Id] -> [Token] -> State [Token] [Token]
applyDefine name params template =
  get >>= \tokens -> case map def tokens of
    [] -> return []
    TD.Id name_ : TD.DelimOpen Dl.Pr : _
      | name == name_ -> do
          modify $ drop 2
          args <- collectArgs
          let replacement = applyArgs args
          rest <- getRest
          return $ replacement ++ rest
    TD.Id name_ : _
      | name == name_ -> do
          modify $ drop 1
          rest <- getRest
          return $ template ++ rest
    _ : _ -> do
      let tk = head tokens
      modify $ drop 1
      rest <- getRest
      return $ tk : rest
  where
    getRest :: State [Token] [Token]
    getRest = do applyDefine name params template

    applyArgs :: [[Token]] -> [Token]
    applyArgs args
      -- FIXME why does this make the program hang
      -- \| length args < length params = error "Not enough arguments"
      -- \| length args > length params = error "Too many arguments"
      | null args || null params = template
      | otherwise = go 0 args template
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
    stringize arg = Token (TD.StrLiteral (Constant (Ty.Array Ty.Char $ length str) str)) cursor
      where
        cursor :: Cursor
        cursor = Token.crs (head arg) |+| Token.crs (last arg)

        str :: String
        str = toStr arg

        toStr :: [Token] -> String
        toStr tokens = case tokens of
          [] -> ""
          tk : tks -> (Token.defToStr . Token.def) tk ++ toStr tks

    collectArgs :: State [Token] [[Token]]
    collectArgs = go
      where
        go :: State [Token] [[Token]]
        go = do
          args <- Token.collectUntilDelimiter Dl.Pr
          return $ parseArgs args

        parseArgs :: [Token] -> [[Token]]
        parseArgs tokens =
          let (arg, rest) = runState collectOne tokens
           in trim arg : parseArgs rest

        -- TODO move to utils and replace in other occurences
        collectOne :: State [Token] [Token]
        collectOne = do
          tks <- get
          case map def tks of
            [] -> return []
            TD.Op Op.Comma : _ -> do
              modify $ drop 1
              collectOne
            _ -> Token.collectUntil (TD.Op Op.Comma)

        trim :: [Token] -> [Token]
        trim = reverse . go' . reverse . go'
          where
            go' :: [Token] -> [Token]
            go' tokens = case map def tokens of
              TD.Nil : _ -> go' $ tail tokens
              TD.NL : _ -> go' $ tail tokens
              _ -> tokens