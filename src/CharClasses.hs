module CharClasses where

whitespace :: [Char]
whitespace = [' ', '\n']

identifierStart :: [Char]
identifierStart = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

identifier :: [Char]
identifier = identifierStart ++ ['0' .. '9']

punctuators :: [Char]
punctuators = ['=', '+', '-', '*', '/', '%', '<', '>', '|', '&', '!', '?', ':']

-- strLitForbiddenChars :: [Char]
-- strLitForbiddenChars = ['\n']