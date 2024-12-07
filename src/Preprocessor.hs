module Preprocessor (process) where

import Delimiter qualified as Dl
import Identifier (Id (..))
import Op qualified
import Token (Token)
import Token qualified as Tk (Token (..))
import Utils (collectUntil, collectUntilDelimiter)

data Directive
  = Define Id [Id] [Token]
  deriving (Show)

process :: [Token] -> [Token]
process tokens =
  let (directives, rest) = parseDirectives ([], tokens)
      rest' = applyDirectives directives rest
   in rest'

-- TODO force directive at fist position of line
parseDirectives :: ([Directive], [Token]) -> ([Directive], [Token])
parseDirectives (directives, tokens) = case tokens of
  [] -> (directives, [])
  ( Tk.Directive name
      : tks
    ) ->
      let (directive, rest) = collectDirective tks
       in case name of
            "define" -> (parseDefine directive : directives, rest)
            _ -> error $ "Unknown directive #" ++ name
  (tk : tks) ->
    let (new_directives, rest) = parseDirectives (directives, tks)
     in (new_directives, tk : rest)

collectDirective :: [Token] -> ([Token], [Token])
collectDirective = collectUntil Tk.NL

applyDirectives :: [Directive] -> [Token] -> [Token]
applyDirectives directives tokens = case directives of
  [] -> tokens
  (directive : rest) -> applyDirectives rest $ applyDirective directive tokens

applyDirective :: Directive -> [Token] -> [Token]
applyDirective directive tokens = case directive of
  Define name params template -> applyDefine name params template tokens

parseDefine :: [Token] -> Directive
parseDefine tokens = case tokens of
  (Tk.Id name : Tk.DelimOpen Dl.Pr : rest) ->
    let (params, rest') = collectDefineParams rest
     in Define name params rest'
  (Tk.Id name : rest) -> Define name [] rest
  _ -> error "Define directive expected name"

collectDefineParams :: [Token] -> ([Id], [Token])
collectDefineParams tokens = case tokens of
  [] -> ([], [])
  (Tk.Id name : Tk.DelimClose Dl.Pr : tks) -> ([name], tks)
  (Tk.Id name : Tk.Op Op.Comma : tks) ->
    let (names, rest) = collectDefineParams tks
     in (name : names, rest)
  (tk : _) -> error $ "Expected identifier, got : " ++ show tk

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
      -- | length args < length params = error "Not enough arguments"
      -- | length args > length params = error "Too many arguments"
      | null args = tokens
      | otherwise = go 0 args tokens
      where
        go :: Int -> [[Token]] -> [Token] -> [Token]
        go idx args' tokens' = case args' of
          [] -> tokens'
          (arg : rest) ->
            let replacement = applyArg (params !! idx) arg tokens'
                newIdx = idx + 1
             in if newIdx < length params
                  then go newIdx rest replacement
                  else replacement

    applyArg :: Id -> [Token] -> [Token] -> [Token]
    applyArg param arg = go
      where
        go :: [Token] -> [Token]
        go tokens = case tokens of
          [] -> []
          (Tk.Id param_ : tks) | param == param_ -> arg ++ go tks
          (tk: tks) -> tk : go tks

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
           in arg : parseArgs rest

        -- TODO move to utils and replace in other occurences
        collectOne :: [Token] -> ([Token], [Token])
        collectOne tokens = case tokens of
          [] -> ([], [])
          (Tk.Op Op.Comma : tks) -> collectOne tks
          _ -> collectUntil (Tk.Op Op.Comma) tokens