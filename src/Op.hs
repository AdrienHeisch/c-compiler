module Op (Op (..), isUnaryPre, isUnaryPost, isBinary, isTernary, isBinaryAssign, getBinaryAssignOp, precedence, strIsOperator, toStr, isRightAssociative, isAddressing) where

import Type (Type)
import Type qualified (toStr)

data Op
  = Not
  | AddOrPlus
  | AddAssign
  | MultOrIndir
  | MultAssign
  | Div
  | DivAssign
  | SubOrNeg
  | SubAssign
  | Mod
  | ModAssign
  | Equal
  | NotEqual
  | Gt
  | Gte
  | Lt
  | Lte
  | BoolAnd
  | BoolOr
  | BitNot
  | BitOr
  | BitOrAssign
  | BitAndOrAddr
  | BitAndAssign
  | BitXor
  | BitXorAssign
  | LShift
  | LShiftAssign
  | RShift
  | RShiftAssign
  | Increment
  | Decrement
  | Assign
  | Member
  | MemberPtr
  | Comma
  | Ternary
  | Colon
  | Subscript
  | Sizeof
  | Cast Type
  deriving (Show, Eq)

unaryPre :: [Op]
unaryPre = [Not, AddOrPlus, MultOrIndir, SubOrNeg, BitNot, BitAndOrAddr, Increment, Decrement, Sizeof]

isUnaryPre :: Op -> Bool
isUnaryPre op = op `elem` unaryPre

unaryPost :: [Op]
unaryPost = [Subscript, Increment, Decrement]

isUnaryPost :: Op -> Bool
isUnaryPost op = op `elem` unaryPost

binary :: [Op]
binary =
  [ AddOrPlus,
    AddAssign,
    MultOrIndir,
    MultAssign,
    Div,
    DivAssign,
    SubOrNeg,
    SubAssign,
    Mod,
    ModAssign,
    Equal,
    NotEqual,
    Gt,
    Gte,
    Lt,
    Lte,
    BoolAnd,
    BoolOr,
    BitOr,
    BitOrAssign,
    BitAndOrAddr,
    BitAndAssign,
    BitXor,
    BitXorAssign,
    LShift,
    LShiftAssign,
    RShift,
    RShiftAssign,
    Assign,
    Member,
    MemberPtr,
    Comma
  ]

isBinary :: Op -> Bool
isBinary op = op `elem` binary

ternary :: [Op]
ternary = [Ternary, Colon]

isTernary :: Op -> Bool
isTernary op = op `elem` ternary

binaryAssign :: [Op]
binaryAssign =
  [ AddAssign,
    MultAssign,
    DivAssign,
    SubAssign,
    ModAssign,
    BitOrAssign,
    BitAndAssign,
    BitXorAssign,
    LShiftAssign,
    RShiftAssign,
    Increment,
    Decrement,
    Assign
  ]

isBinaryAssign :: Op -> Bool
isBinaryAssign op = op `elem` binaryAssign

getBinaryAssignOp :: Op -> Maybe Op
getBinaryAssignOp op = case op of
  AddAssign -> Just AddOrPlus
  SubAssign -> Just SubOrNeg
  MultAssign -> Just MultOrIndir
  DivAssign -> Just Div
  ModAssign -> Just Mod
  BitOrAssign -> Just BitOr
  BitAndAssign -> Just BitAndOrAddr
  BitXorAssign -> Just BitXor
  LShiftAssign -> Just LShift
  RShiftAssign -> Just RShift
  _ -> Nothing

rightAssociative :: [Op]
rightAssociative =
  binaryAssign
    ++ ternary
    ++ [ Assign,
         Member,
         MemberPtr
       ]

isRightAssociative :: Op -> Bool
isRightAssociative op = op `elem` rightAssociative

addressing :: [Op]
addressing =
  [ Assign,
    Member,
    MemberPtr
  ]

isAddressing :: Op -> Bool
isAddressing op = op `elem` addressing

precedence :: Op -> Int
precedence op = case op of
  Subscript -> 0
  Member -> 0
  MemberPtr -> 0
  MultOrIndir -> 1
  Div -> 1
  Mod -> 1
  AddOrPlus -> 2
  SubOrNeg -> 2
  LShift -> 3
  RShift -> 3
  Lt -> 4
  Lte -> 4
  Gt -> 4
  Gte -> 4
  Equal -> 5
  NotEqual -> 5
  BitAndOrAddr -> 6
  BitXor -> 7
  BitOr -> 8
  BoolAnd -> 9
  BoolOr -> 10
  Assign -> 11
  AddAssign -> 11
  SubAssign -> 11
  MultAssign -> 11
  DivAssign -> 11
  ModAssign -> 11
  LShiftAssign -> 11
  RShiftAssign -> 11
  BitAndAssign -> 11
  BitXorAssign -> 11
  BitOrAssign -> 11
  Comma -> 12
  _ -> 99

strIsOperator :: String -> Bool
strIsOperator str = str `elem` allowed
  where
    allowed =
      [ "!",
        "==",
        "!=",
        ">",
        ">=",
        "<",
        "<=",
        "&&",
        "||",
        "%",
        "%=",
        "+",
        "+=",
        "-",
        "-=",
        "*",
        "*=",
        "/",
        "/=",
        "++",
        "--",
        "~",
        "&",
        "&=",
        "|",
        "|=",
        "^",
        "^=",
        "<<",
        "<<=",
        ">>",
        ">>=",
        "->",
        ".",
        "=",
        ",",
        "?",
        ":"
      ]

toStr :: Op -> String
toStr op = case op of
  Not -> "!"
  AddOrPlus -> "+"
  AddAssign -> "+="
  MultOrIndir -> "*"
  MultAssign -> "*="
  Div -> "/"
  DivAssign -> "/="
  SubOrNeg -> "-"
  SubAssign -> "-="
  Mod -> "%"
  ModAssign -> "%="
  Equal -> "=="
  NotEqual -> "!="
  Gt -> ">"
  Gte -> ">="
  Lt -> "<"
  Lte -> "<="
  BoolAnd -> "&&"
  BoolOr -> "||"
  BitNot -> "~"
  BitOr -> "|"
  BitOrAssign -> "|="
  BitAndOrAddr -> "&"
  BitAndAssign -> "&="
  BitXor -> "^"
  BitXorAssign -> "^="
  LShift -> "<<"
  LShiftAssign -> "<<="
  RShift -> ">>"
  RShiftAssign -> ">>="
  Increment -> "++"
  Decrement -> "--"
  Assign -> "="
  Member -> "."
  MemberPtr -> "->"
  Comma -> ","
  Ternary -> "?"
  Colon -> ":"
  Subscript -> "[]"
  Sizeof -> "sizeof"
  Cast ty -> "(" ++ Type.toStr ty ++ ")"