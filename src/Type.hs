module Type (Type (..), signed, unsigned, isInteger, isFloating, toStr) where

import Identifier (Id)
import Identifier qualified

data Type
  = Infer
  | Void
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
  [ Bool,
    Char,
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

toStr :: Type -> String
toStr ty = case ty of
  Infer -> error "Infer type should not be stringified"
  Void -> "void"
  Bool -> "bool"
  Char -> "char"
  UChar -> "unsigned char"
  Short -> "short"
  UShort -> "unsigned short"
  Int -> "int"
  UInt -> "unsigned int"
  Long -> "long"
  ULong -> "usigned long"
  LLong -> "long long"
  ULLong -> "unsigned long long"
  Float -> "float"
  Double -> "double"
  LDouble -> "long double"
  Pointer ty' -> toStr ty' ++ " *"
  Array ty' len -> toStr ty' ++ " [" ++ show len ++ "]"
  ArrayNoHint ty' -> toStr ty' ++ " []"
  Struct Nothing -> "struct"
  Struct (Just name) -> "struct" ++ Identifier.toStr name
  Enum Nothing ty' -> "enum : " ++ toStr ty'
  Enum (Just name) ty' -> "enum " ++ Identifier.toStr name ++ " : " ++ toStr ty'