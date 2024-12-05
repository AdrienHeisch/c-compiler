module Token (Token (..), Delimiter (..), make) where

import CharClasses qualified as CC
import Data.Char (isDigit)
import Identifier qualified
import Op (Op)
import Op qualified (Op (..))
import Type (Type)
import Type qualified (Type (..))
import Constant (Constant(..), IntRepr(IntRepr), StrRepr(StrRepr))

data Token
  = Type Type
  | Op Op
  | Id Identifier.Id
  | NumLiteral (Constant IntRepr) -- TODO float
  | StrLiteral (Constant StrRepr)
  | ImplInclude String
  | Const
  | If
  | Else
  | Switch
  | Case
  | Default
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
  | NL
  | Nil
  | Eof
  deriving (Show, Eq)

data Delimiter
  = Pr
  | Br
  | SqBr
  deriving (Show, Eq)

make :: String -> Token
make str = case str of
  ";" -> Semicolon
  "void" -> Type Type.Void
  "bool" -> keywordUnimplErr str
  "char" -> Type Type.Char
  "short" -> Type Type.Short
  "int" -> Type Type.Int
  "long" -> Type Type.Long
  "float" -> Type Type.Float
  "double" -> Type Type.Double
  "signed" -> keywordUnimplErr str
  "unsigned" -> keywordUnimplErr str
  "auto" -> keywordUnimplErr str
  "const" -> Const
  "static" -> keywordUnimplErr str
  "static_assert" -> keywordUnimplErr str
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
  "default" -> Default
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
  "extern" -> keywordUnimplErr str
  "register" -> keywordUnimplErr str
  "restrict" -> keywordUnimplErr str
  "volatile" -> keywordUnimplErr str
  "inline" -> keywordUnimplErr str
  "nullptr" -> keywordUnimplErr str
  "thread_local" -> keywordUnimplErr str
  "alignas" -> keywordUnimplErr str
  "alignof" -> keywordUnimplErr str
  "typeof" -> keywordUnimplErr str
  "typeof_unqual" -> keywordUnimplErr str
  "sizeof" -> Op Op.Sizeof
  "!" -> Op Op.Not
  "==" -> Op Op.Equal
  "!=" -> Op Op.NotEqual
  ">" -> Op Op.Gt
  ">=" -> Op Op.Gte
  "<" -> Op Op.Lt
  "<=" -> Op Op.Lte
  "&&" -> Op Op.BoolAnd
  "||" -> Op Op.BoolOr
  "%" -> Op Op.Mod
  "%=" -> Op Op.ModAssign
  "+" -> Op Op.AddOrPlus
  "+=" -> Op Op.AddAssign
  "-" -> Op Op.SubOrNeg
  "-=" -> Op Op.SubAssign
  "*" -> Op Op.MultOrIndir
  "*=" -> Op Op.MultAssign
  "/" -> Op Op.Div
  "/=" -> Op Op.DivAssign
  "++" -> Op Op.Increment
  "--" -> Op Op.Decrement
  "~" -> Op Op.BitNot
  "&" -> Op Op.BitAndOrAddr
  "&=" -> Op Op.BitAndAssign
  "|" -> Op Op.BitOr
  "|=" -> Op Op.BitOrAssign
  "^" -> Op Op.BitXor
  "^=" -> Op Op.BitXorAssign
  "<<" -> Op Op.LShift
  "<<=" -> Op Op.LShiftAssign
  ">>" -> Op Op.RShift
  ">>=" -> Op Op.RShiftAssign
  "->" -> Op Op.Member
  "." -> Op Op.MemberPtr
  "=" -> Op Op.Assign
  "," -> Op Op.Comma
  "?" -> Op Op.TernaryThen
  ":" -> Op Op.TernaryElse
  "(" -> DelimOpen Pr
  ")" -> DelimClose Pr
  "{" -> DelimOpen Br
  "}" -> DelimClose Br
  "[" -> DelimOpen SqBr
  "]" -> DelimClose SqBr
  "#" -> Directive
  "false" -> keywordUnimplErr str
  "true" -> keywordUnimplErr str
  _ | isStringLiteral str -> StrLiteral $ Constant (Type.Array Type.Char $ length str - 2) (StrRepr $ tail (take (length str - 1) str))
  _ | isNumberLiteral str -> NumLiteral $ Constant Type.Int (IntRepr $ read str)
  _ | isIdentifier str -> Id (Identifier.Id str)
  _ | isNewLine str -> NL
  _ | isWhitespace str -> Nil
  _ -> error ("Unexpected character sequence \"" ++ str ++ "\"")

keywordUnimplErr :: String -> a
keywordUnimplErr keyword = error ("Unimplemented keyword : " ++ keyword)

isNumberLiteral :: String -> Bool
isNumberLiteral "" = False
isNumberLiteral (c : "") = isDigit c
isNumberLiteral (c : cs) = isDigit c && isNumberLiteral cs

isStringLiteral :: String -> Bool
isStringLiteral ('"' : cs) = case cs of
  "" -> False
  _ -> last cs == '"'
isStringLiteral _ = False

-- isImplHeader :: String -> Bool
-- isImplHeader ('<' : cs) = case cs of
--   "" -> False
--   _ -> last cs == '>'
-- isImplHeader _ = False

isIdentifier :: String -> Bool
isIdentifier "" = False
isIdentifier (c : "") = c `elem` CC.identifier
isIdentifier (c : cs) = c `elem` CC.identifier && isIdentifier cs

isWhitespace :: String -> Bool
isWhitespace "" = True
isWhitespace (' ' : cs) = isWhitespace cs
isWhitespace _ = False

isNewLine :: String -> Bool
isNewLine "" = False
isNewLine ('\n' : "") = True
isNewLine ('\n' : cs) = isNewLine cs
isNewLine _ = False
