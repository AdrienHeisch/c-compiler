module Type (Type (..), signed, unsigned, isInteger, isFloating, sizeof, toStr, paddedSizeof, mask, isComplex, sizeofWithPointer, canCast, getName) where

import Data.List (intercalate)
import Identifier (Id)
import Identifier qualified
import Instruction (regLen)

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
  | Struct (Maybe Id) (Maybe [(Type, Id)]) -- TODO merge with statement constructor ? or external data ?
  | Union (Maybe Id) (Maybe [(Type, Id)]) -- TODO merge with statement constructor ? or external data ?
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

sizeof :: Type -> Int
sizeof ty = case ty of
  Void -> 0
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
  Pointer _ -> regLen
  Array ty' len' -> sizeof ty' * len'
  Struct _ (Just fields) -> sum $ map (sizeof . fst) fields
  Union _ (Just fields) -> maximum $ map (sizeof . fst) fields
  Enum _ ty' -> sizeof ty'
  _ -> error $ "Not a concrete type: " ++ show ty

paddedSizeof :: Type -> Int
paddedSizeof ty = (((sizeof ty - 1) `div` regLen) + 1) * regLen

isComplex :: Type -> Bool
isComplex ty = case ty of
  Type.Array _ _ -> True
  Type.ArrayNoHint _ -> True
  Type.Struct _ _ -> True
  Type.Union _ _ -> True
  _ -> False

getName :: Type -> Maybe Id
getName ty = case ty of
  Type.Struct name _ -> name
  Type.Union name _ -> name
  Type.Enum name _ -> name
  Type.Typedef name -> Just name
  _ -> Nothing

sizeofWithPointer :: Type -> Int
sizeofWithPointer ty = paddedSizeof ty + if isComplex ty then regLen else 0

mask :: Type -> Int
-- FIXME 64 bit system -> replace Int with Word everywhere ?
mask ty
  | isComplex ty = 0x7FFFFFFFFFFFFFFF
  | otherwise = case sizeof ty of
      0 -> 0
      1 -> 0xFF
      2 -> 0xFFFF
      4 -> 0xFFFFFFFF
      8 -> 0x7FFFFFFFFFFFFFFF
      _ -> error $ "Invalid type size: " ++ show ty

canCast :: Type -> Type -> Bool
canCast from to
  | isInteger from && isInteger to && sizeof from < sizeof to = True
  | otherwise = case (from, to) of
      (_, Pointer _) | isInteger from -> True
      (Array fromTy _, Pointer toTy) | fromTy == toTy -> True
      _ -> False

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
  Array ty' arrlen -> toStr ty' ++ " [" ++ show arrlen ++ "]" -- TODO use maybe and remove arraynohint
  ArrayNoHint ty' -> toStr ty' ++ " []"
  Struct Nothing fields -> "struct { " ++ show fields ++ " }"
  Struct (Just name) fields -> "struct " ++ Identifier.toStr name ++ " { " ++ show fields ++ " }"
  Union Nothing fields -> "union { " ++ show fields ++ " }"
  Union (Just name) fields -> "union " ++ Identifier.toStr name ++ " { " ++ show fields ++ " }"
  Enum Nothing ty' -> "enum : " ++ toStr ty'
  Enum (Just name) ty' -> "enum " ++ Identifier.toStr name ++ " : " ++ toStr ty'
  Typedef name -> Identifier.toStr name
  Function ret tys -> toStr ret ++ "*(" ++ intercalate ", " (map toStr tys) ++ ")"
  Pointer (Function ret tys) -> toStr ret ++ " (*)(" ++ intercalate ", " (map toStr tys) ++ ")"
  Pointer (Array ty' arrlen) -> toStr ty' ++ " (*)[" ++ show arrlen ++ "]"
  Pointer ty' -> toStr ty' ++ " *"
