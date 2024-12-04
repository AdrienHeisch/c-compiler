module Op where

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

opChars = ['=', '+', '-', '*', '/', '%', '<', '>', '|', '&', '!', '?', ':']

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