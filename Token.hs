module Token where

import Data.Char (isAlpha, isDigit)
import Op

data Token
  = Type Type
  | Op Op
  | Id String
  | NumLiteral Int
  | StrLiteral String
  | Const
  | If
  | Else
  | Switch
  | Case
  | Do
  | While
  | For
  | Continue
  | Break
  | Goto
  | Return
  | Struct
  | Enum
  | Union
  | Typedef
  | Semicolon
  | DelimOpen Delimiter
  | DelimClose Delimiter
  | Directive
  | Eof
  | Nil
  deriving (Show, Eq)

data Delimiter
  = Pr
  | Br
  | SqBr
  deriving (Show, Eq)

data Type
  = Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  deriving (Show, Eq)

mkToken :: String -> Token
mkToken str = case str of
  ";" -> Semicolon
  "void" -> Type Void
  _ | str == "bool" || str == "_Bool" -> keywordUnimplErr str
  "char" -> Type Char
  "short" -> Type Short
  "int" -> Type Int
  "long" -> Type Long
  "float" -> Type Float
  "double" -> Type Double
  "signed" -> keywordUnimplErr str
  "unsigned" -> keywordUnimplErr str
  "auto" -> keywordUnimplErr str
  "const" -> Const
  "static" -> keywordUnimplErr str
  _ | str == "static_assert" || str == "_Static_assert" -> keywordUnimplErr str
  "constexpr" -> keywordUnimplErr str
  "_Atomic" -> keywordUnimplErr str
  "_BitInt" -> keywordUnimplErr str
  "_Complex" -> keywordUnimplErr str
  "_Decimal32" -> keywordUnimplErr str
  "_Decimal64" -> keywordUnimplErr str
  "_Decimal128" -> keywordUnimplErr str
  "_Generic" -> keywordUnimplErr str
  "_Imaginary" -> keywordUnimplErr str
  "_Noreturn" -> keywordUnimplErr str
  "if" -> If
  "else" -> Else
  "switch" -> Switch
  "case" -> Case
  "do" -> Do
  "while" -> While
  "for" -> For
  "continue" -> Continue
  "break" -> Break
  "goto" -> Goto
  "return" -> Return
  "struct" -> Struct
  "enum" -> Enum
  "union" -> Union
  "typedef" -> Typedef
  "default" -> keywordUnimplErr str
  "extern" -> keywordUnimplErr str
  "register" -> keywordUnimplErr str
  "restrict" -> keywordUnimplErr str
  "volatile" -> keywordUnimplErr str
  "inline" -> keywordUnimplErr str
  "nullptr" -> keywordUnimplErr str
  _ | str == "thread_local" || str == "_Thread_local" -> keywordUnimplErr str
  _ | str == "alignas" || str == "_Alignas" -> keywordUnimplErr str
  _ | str == "alignof" || str == "_Alignof" -> keywordUnimplErr str
  "typeof" -> keywordUnimplErr str
  "typeof_unqual" -> keywordUnimplErr str
  "sizeof" -> Op Sizeof
  "!" -> Op Not
  "==" -> Op Equal
  "!=" -> Op NotEqual
  ">" -> Op Gt
  ">=" -> Op Gte
  "<" -> Op Lt
  "<=" -> Op Lte
  "&&" -> Op BoolAnd
  "||" -> Op BoolOr
  "%" -> Op Mod
  "%=" -> Op ModAssign
  "+" -> Op AddOrPlus
  "+=" -> Op AddAssign
  "-" -> Op SubOrNeg
  "-=" -> Op SubAssign
  "*" -> Op MultOrIndir
  "*=" -> Op MultAssign
  "/" -> Op Div
  "/=" -> Op DivAssign
  "++" -> Op Increment
  "--" -> Op Decrement
  "~" -> Op BitNot
  "&" -> Op BitAndOrAddr
  "&=" -> Op BitAndAssign
  "|" -> Op BitOr
  "|=" -> Op BitOrAssign
  "^" -> Op BitXor
  "^=" -> Op BitXorAssign
  "<<" -> Op LShift
  "<<=" -> Op LShiftAssign
  ">>" -> Op RShift
  ">>=" -> Op RShiftAssign
  "->" -> Op Deref
  "." -> Op StructRef
  "=" -> Op Assign
  "," -> Op Comma
  "?" -> Op TernaryThen
  ":" -> Op TernaryElse
  "(" -> DelimOpen Pr
  ")" -> DelimClose Pr
  "{" -> DelimOpen Br
  "}" -> DelimClose Br
  "[" -> DelimOpen SqBr
  "]" -> DelimClose SqBr
  "#" -> Directive
  "false" -> keywordUnimplErr str
  "true" -> keywordUnimplErr str
  str | isStringLiteral str -> StrLiteral (tail (take (length str - 1) str))
  str | isIdentifier str -> Id str
  str | isNumberLiteral str -> NumLiteral (read str)
  str | isWhitespace str -> Nil
  str -> error ("Unexpected character sequence \"" ++ str ++ "\"")

keywordUnimplErr :: String -> a
keywordUnimplErr keyword = error ("Unimplemented keyword : " ++ keyword)

isNumberLiteral :: String -> Bool
isNumberLiteral "" = False
isNumberLiteral (c : "") = isDigit c
isNumberLiteral (c : cs) = isDigit c && isNumberLiteral cs

strLitForbiddenChars = ['\n']

isStringLiteral :: String -> Bool
isStringLiteral ('"' : cs) = case cs of
  "" -> False
  _ -> last cs == '"'
isStringLiteral _ = False

-- isImplImport :: String -> Bool
-- isImplImport ('<' : cs) = case cs of
--   "" -> False
--   _ -> last cs == '>'
-- isImplImport _ = False

identBeginChars :: [Char]
identBeginChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

identChars :: [Char]
identChars = identBeginChars ++ ['0' .. '9']

isIdentifier :: String -> Bool
isIdentifier "" = False
isIdentifier (c : "") = c `elem` identChars
isIdentifier (c : cs) = c `elem` identChars && isIdentifier cs

whiteChars :: [Char]
whiteChars = [' ', '\n']

isWhitespace :: String -> Bool
isWhitespace "" = True
isWhitespace (c : cs) | c `elem` whiteChars = isWhitespace cs
isWhitespace str = False
