module Type (Type (..), signed, unsigned, isInteger, isFloating, len, toStr) where

import Identifier (Id)
import Identifier qualified
import Data.List (intercalate)

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
  | Struct (Maybe Id) [(Type, Id)] -- TODO merge with statement constructor ? or external data ?
  | Union (Maybe Id) [(Type, Id)] -- TODO merge with statement constructor ? or external data ?
  | Enum (Maybe Id) Type -- TODO replace with underlying type ?
  | Typedef Id
  | Function Type [Type]
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

len :: Type -> Int -- TODO duplicate of register len
len ty = case ty of
  Bool -> 1
  Char -> 1
  UChar -> 1
  Short -> 2
  UShort -> 2
  Int -> 4
  UInt -> 4
  Long -> 8
  ULong -> 8
  LLong -> 8
  ULLong -> 8
  Float -> 4
  Double -> 8
  LDouble -> 8
  Pointer _ -> 8
  Array ty' len' -> len ty' * len'
  _ -> error "Unsized type"


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
  Array ty' arrlen -> toStr ty' ++ " [" ++ show arrlen ++ "]"
  ArrayNoHint ty' -> toStr ty' ++ " []"
  Struct Nothing fields -> "struct { " ++ show fields ++ " }"
  Struct (Just name) fields -> "struct " ++ Identifier.toStr name ++ " { " ++ show fields ++ " }"
  Union Nothing fields -> "union { " ++ show fields ++ " }"
  Union (Just name) fields -> "union " ++ Identifier.toStr name ++ " { " ++ show fields ++ " }"
  Enum Nothing ty' -> "enum : " ++ toStr ty'
  Enum (Just name) ty' -> "enum " ++ Identifier.toStr name ++ " : " ++ toStr ty'
  Typedef name -> Identifier.toStr name
  Function ret tys -> toStr ret ++ "*(" ++ intercalate ", " (map toStr tys) ++ ")"
  Pointer (Function ret tys) -> toStr ret ++ "(*)(" ++ intercalate ", " (map toStr tys) ++ ")"
  Pointer ty' -> toStr ty' ++ " *"