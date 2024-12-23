module CharClasses where

whitespace :: [Char]
whitespace = [' ', '\t']

digits :: [Char]
digits = ['0' .. '9']

float :: [Char]
float = digits ++ ['.']

lowercase :: [Char]
lowercase = ['a' .. 'z']

uppercase :: [Char]
uppercase = ['A' .. 'Z']

alphabet :: [Char]
alphabet = lowercase ++ uppercase

identifierStart :: [Char]
identifierStart = lowercase ++ uppercase ++ ['_']

identifier :: [Char]
identifier = identifierStart ++ digits

punctuators :: [Char]
punctuators = ['=', '+', '-', '*', '/', '%', '<', '>', '|', '&', '!', '?', ':']

escape :: [Char]
escape =
  [ '\n',
    '\r',
    '\t',
    '\v',
    '\b',
    '\a',
    '\'',
    '\"',
    '\\',
    '\0'
  ]