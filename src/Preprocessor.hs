module Preprocessor (process) where

import Token as Tk (Token (..))

process :: [Token] -> [Token]
process tokens = tokens

-- collectDirective :: [Token] -> ([Token], [Token])
-- collectDirective = collectUntil Tk.NL

-- parseDirective :: [Token] -> Directive
-- parseDirective _ = Declaration.Directive