module Token (Token (..), TokenDef (..), Delimiter (..), errs, foldCrs, makeDef, defToStr, filterNil, filterNL, collectUntil, collectUntilDelimiter, parseList) where

import CharClasses qualified as CC
import Constant (Constant (..), FltRepr, IntRepr, StrRepr)
import Cursor (Cursor, fold)
import Data.Char (ord, toLower)
import Delimiter (Delimiter)
import Delimiter qualified
import Identifier (Id)
import Identifier qualified
import Op (Op)
import Op qualified
import Type (Type)
import Type qualified
import Utils (genErrs, Display (..))

data Token = Token {def :: TokenDef, crs :: Cursor}
  deriving (Show)

instance Display Token where
  display :: Token -> String
  display = show . def

errs :: [Token] -> [String]
errs = genErrs isInvalid
  where
    isInvalid (Token (Invalid _) _) = True
    isInvalid _ = False

foldCrs :: [Token] -> Cursor
foldCrs = Cursor.fold . map Token.crs

data TokenDef
  = Type Type
  | Signed
  | Unsigned
  | Op Op
  | Id Id
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
  | Directive String
  | Stringize
  | TokenPaste
  | NL
  | Nil
  | Eof
  | Invalid String
  deriving (Show, Eq)

makeDef :: String -> TokenDef
makeDef str = case str of
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
  "(" -> DelimOpen Delimiter.Pr
  ")" -> DelimClose Delimiter.Pr
  "{" -> DelimOpen Delimiter.Br
  "}" -> DelimClose Delimiter.Br
  "[" -> DelimOpen Delimiter.SqBr
  "]" -> DelimClose Delimiter.SqBr
  "false" -> IntLiteral $ Constant Type.Bool 0
  "true" -> IntLiteral $ Constant Type.Bool 1
  _ | isStringLiteral str -> StrLiteral $ Constant (Type.Array Type.Char $ length str - 2) (tail (take (length str - 1) str))
  _ | isCharLiteral str -> IntLiteral $ Constant Type.Char (ord $ read str)
  _ | isIntLiteral str -> IntLiteral $ Constant Type.Int (read str)
  _ | isFltLiteral str -> FltLiteral $ Constant Type.Float (read str)
  _ | isIdentifier str -> Id (Identifier.Id str)
  _ | isNewLine str -> NL
  _ | isWhitespace str -> Nil
  ('#' : directive) | isIdentifier directive -> Directive directive
  "#" -> Stringize
  "##" -> TokenPaste
  _ -> Invalid ("Unexpected character sequence \"" ++ str ++ "\"")

filterNil :: [Token] -> [Token]
filterNil = filter ((/= Nil) . def)

filterNL :: [Token] -> [Token]
filterNL = filter ((/= NL) . def)

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
isCharLiteral ('\'' : cs) = last cs == '\''
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

defToStr :: TokenDef -> String
defToStr tkDef = case tkDef of
  Type ty -> Type.toStr ty
  Op op -> Op.toStr op
  Id name -> Identifier.toStr name
  DelimOpen dl -> Delimiter.toStrOpen dl
  DelimClose dl -> Delimiter.toStrClose dl
  IntLiteral (Constant _ int) -> show int
  FltLiteral (Constant _ flt) -> show flt
  StrLiteral (Constant _ str) -> str
  ImplInclude str -> "<" ++ str ++ ">"
  Semicolon -> ";"
  Directive str -> "#" ++ str
  NL -> "\n"
  Nil -> " "
  Eof -> ""
  Invalid str -> str
  _ -> map toLower $ show tkDef

collectUntil :: TokenDef -> [Token] -> ([Token], [Token])
collectUntil end tokens = case tokens of
  [] -> ([], [])
  (tk : tks) | Token.def tk == end -> ([], tks)
  (tk : tks) ->
    let (tks', rest) = collectUntil end tks
     in (tk : tks', rest)

collectUntilDelimiter :: Delimiter -> [Token] -> ([Token], [Token])
collectUntilDelimiter del = go 0
  where
    go :: Int -> [Token] -> ([Token], [Token])
    go depth tokens = case tokens of
      [] -> ([], [])
      (tk : tks)
        | Token.def tk == DelimClose del && depth == 0 -> ([], tks)
        | Token.def tk == DelimClose del -> next tk tks (depth - 1)
        | Token.def tk == DelimOpen del -> next tk tks (depth + 1)
        | otherwise -> next tk tks depth

    next :: Token -> [Token] -> Int -> ([Token], [Token])
    next tk rest depth =
      let (tks', rest') = go depth rest
       in (tk : tks', rest')

parseList :: TokenDef -> ([Token] -> (el, [Token])) -> [Token] -> [el]
parseList end parser tokens = case tokens of
  [] -> []
  (tk : tks) | Token.def tk == end -> parseList end parser tks
  _ ->
    let (el, tks) = parser tokens
     in el : parseList end parser tks