module Type (Type (..), isInteger, isFloating) where

import Identifier (Id)

data Type
  = Void
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
  deriving (Show, Eq)

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