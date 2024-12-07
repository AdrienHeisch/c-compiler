module Preprocessor (process) where

-- import Constant (Constant, IntRepr, FltRepr, StrRepr)

import Debug.Trace (trace)
import Identifier (Id (..))
import Token (Token)
import Token qualified as Tk (Token (..))
import Utils (collectUntil)

data Directive
  = Define Id [Id] [Token]
  deriving (Show)

process :: [Token] -> [Token]
process tokens =
  let (directives, rest) = parseDirectives ([], tokens)
      rest' = trace (show directives) applyDirectives directives rest
   in rest'

-- TODO force directive at fist position of line
parseDirectives :: ([Directive], [Token]) -> ([Directive], [Token])
parseDirectives (directives, tokens) = case tokens of
  [] -> (directives, [])
  ( Tk.Directive
      : tks
    ) ->
      let (directive, rest) = collectDirective tks
       in (parseDirective directive : directives, rest)
  (tk : tks) ->
    let (new_directives, rest) = parseDirectives (directives, tks)
     in (new_directives, tk : rest)

collectDirective :: [Token] -> ([Token], [Token])
collectDirective = collectUntil Tk.NL

parseDirective :: [Token] -> Directive
parseDirective tokens = case tokens of
  (Tk.Id (Id "define") : Tk.Id name : rest) -> Define name [] rest
  _ -> error "Invalid directive"

applyDirectives :: [Directive] -> [Token] -> [Token]
applyDirectives directives tokens = case directives of
  [] -> tokens
  (directive : rest) -> applyDirectives rest $ applyDirective directive tokens

applyDirective :: Directive -> [Token] -> [Token]
applyDirective directive tokens = case directive of
  Define name ids replace -> applyDefine name ids replace tokens
--   _ -> tokens

applyDefine :: Id -> [Id] -> [Token] -> [Token] -> [Token]
applyDefine name ids replace = apply
  where
    apply :: [Token] -> [Token]
    apply tokens = case tokens of
      [] -> tokens
      (Tk.Id name_ : tks) | name == name_ -> replace ++ applyDefine name ids replace tks
      (tk : tks) -> tk : applyDefine name ids replace tks