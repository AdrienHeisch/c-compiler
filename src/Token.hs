module Token (Token (..), Delimiter (..), make, filterNL) where

import CharClasses qualified as CC
import Constant (Constant (..), FltRepr, IntRepr, StrRepr)
import Data.Char (ord)
import Identifier qualified
import Op (Op)
import Op qualified (Op (..))
import Type (Type)
import Type qualified (Type (..))

data Token
  = Type Type
  | Signed
  | Unsigned
  | Op Op
  | Id Identifier.Id
  | IntLiteral (Constant IntRepr)
  | FltLiteral (Constant FltRepr)
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
  "bool" -> Type Type.Bool
  "char" -> Type Type.Char
  "short" -> Type Type.Short
  "int" -> Type Type.Int
  "long" -> Type Type.Long
  "float" -> Type Type.Float
  "double" -> Type Type.Double
  "signed" -> Signed
  "unsigned" -> Unsigned
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
  "?" -> Op Op.Ternary
  ":" -> Op Op.Colon
  "(" -> DelimOpen Pr
  ")" -> DelimClose Pr
  "{" -> DelimOpen Br
  "}" -> DelimClose Br
  "[" -> DelimOpen SqBr
  "]" -> DelimClose SqBr
  "#" -> Directive
  "false" -> IntLiteral $ Constant Type.Bool 0
  "true" -> IntLiteral $ Constant Type.Bool 1
  _ | isStringLiteral str -> StrLiteral $ Constant (Type.Array Type.Char $ length str - 2) (tail (take (length str - 1) str))
  _ | isCharLiteral str -> IntLiteral $ Constant Type.Char (ord $ read str)
  _ | isIntLiteral str -> IntLiteral $ Constant Type.Int (read str)
  _ | isFltLiteral str -> FltLiteral $ Constant Type.Float (read str)
  _ | isIdentifier str -> Id (Identifier.Id str)
  _ | isNewLine str -> NL
  _ | isWhitespace str -> Nil
  _ -> error ("Unexpected character sequence \"" ++ str ++ "\"")

filterNL :: [Token] -> [Token]
filterNL = filter (/= Token.NL)

keywordUnimplErr :: String -> a
keywordUnimplErr keyword = error ("Unimplemented keyword : " ++ keyword)

isIntLiteral :: String -> Bool
isIntLiteral "" = False
isIntLiteral (c : "") = c `elem` CC.digits
isIntLiteral (c : cs) = c `elem` CC.digits && isIntLiteral cs

isFltLiteral :: String -> Bool
isFltLiteral "" = False
isFltLiteral (c : cs) = c `elem` CC.digits && continue cs
  where
    continue "" = False
    continue (c' : "") = c' `elem` CC.float
    continue (c' : cs') = c' `elem` CC.float && continue cs'

isStringLiteral :: String -> Bool
isStringLiteral ('"' : cs) = case cs of
  "" -> False
  _ -> last cs == '"'
isStringLiteral _ = False

isCharLiteral :: String -> Bool
isCharLiteral "\'" = False
isCharLiteral "\'\'" = False
isCharLiteral ('\'' : cs) = last cs == '\'' {-  && continue cs -}
-- where
--   continue "" = False
--   continue ('\\' : cs') = esc cs'
--   continue (c' : cs') = False
--   esc "" = False
--   -- esc (c' : cs') =
isCharLiteral _ = False

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
