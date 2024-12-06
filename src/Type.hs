module Type (Type (..), signed, unsigned, isInteger, isFloating) where

import Identifier (Id)

data Type
  = Void
  | Bool
  | Char
  | UChar
  | Short
  | UShort
  | Int
  | UInt
  | Long
  | ULong
  | LLong
  | ULLong
  | Float
  | Double
  | LDouble
  | Pointer Type
  | Array Type Int
  | ArrayNoHint Type
  | Struct (Maybe Id)
  | Enum (Maybe Id) Type
  deriving (Show, Eq)

signed :: Type -> Type
signed ty = case ty of
  Char -> Char
  UChar -> Char
  Short -> Short
  UShort -> Short
  Int -> Int
  UInt -> Int
  Long -> Long
  ULong -> Long
  LLong -> LLong
  ULLong -> LLong
  _ -> ty

unsigned :: Type -> Type
unsigned ty = case ty of
  Char -> UChar
  UChar -> UChar
  Short -> UShort
  UShort -> UShort
  Int -> UInt
  UInt -> UInt
  Long -> ULong
  ULong -> ULong
  LLong -> ULLong
  ULLong -> ULLong
  _ -> ty

integer :: [Type]
integer =
  [ Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LLong,
    ULLong
  ]

isInteger :: Type -> Bool
isInteger ty = ty `elem` integer

floating :: [Type]
floating =
  [ Float,
    Double,
    LDouble
  ]

isFloating :: Type -> Bool
isFloating ty = ty `elem` floating