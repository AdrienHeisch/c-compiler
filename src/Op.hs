module Op (Op (..), isUnaryPre, isUnaryPost, isBinary, isTernary) where

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
  | Deref
  | StructRef
  | Call
  | Comma
  | TernaryThen
  | TernaryElse
  | Index
  | Sizeof
  deriving (Show, Eq)

unaryPre :: [Op]
unaryPre = [Not, AddOrPlus, MultOrIndir, SubOrNeg, BitNot, Increment, Decrement, Sizeof]

isUnaryPre :: Op -> Bool
isUnaryPre op = op `elem` unaryPre

unaryPost :: [Op]
unaryPost = [Call, Index, Increment, Decrement]

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
    Deref,
    StructRef,
    Call,
    Comma
  ]

isBinary :: Op -> Bool
isBinary op = op `elem` binary

ternary :: [Op]
ternary = [TernaryThen, TernaryElse]

isTernary :: Op -> Bool
isTernary op = op `elem` ternary

-- opPrecedence :: Op -> Int
-- opPrecedence op = case op of
--   AddrOrBitAnd -> 0
--   Not -> 0
--   Index -> 1
--   Mod -> 1
--   MultOrDeref -> 2
--   Div -> 2
--   AddOrPlus -> 3
--   SubOrNeg -> 3
--   Equal -> 4
--   NotEqual -> 4
--   Gt -> 4
--   Gte -> 4
--   Lt -> 4
--   Lte -> 4
--   BoolAnd -> 5
--   BoolOr -> 6
--   Assign -> 7
--   AddAssign -> 7
--   SubAssign -> 7
--   MultAssign -> 7
--   DivAssign -> 7
--   ModAssign -> 7