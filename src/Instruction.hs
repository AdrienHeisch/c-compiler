module Instruction (Program (..), Instruction (..), Register (..), Cst, Value (..), regLen, getTyRegs) where

import Type (Type)
import Type qualified (Type (..))
import Utils (Display (display))
import Text.Printf (printf)

newtype Program = Program [Instruction]
  deriving (Show)

instance Utils.Display Program where
  display :: Program -> String
  -- display (Program ins) = intercalate "\n" . map show $ ins
  display (Program ins) = go 0 ins
    where
      go (pc :: Int) ins' = case ins' of
        [] -> ""
        LABEL l : rest -> show l ++ ":\n" ++ go pc rest
        instr : rest -> "  " ++ printf "%04d" pc ++ "  " ++ show instr ++ "\n" ++ go (pc + 1) rest

data Instruction
  = LABEL Int
  | NOP
  | HALT Value
  | SYS_CALL Value
  | CLEAR Register
  | SET Register Value
  | LOAD Register Value
  | STORE Register Value
  | SWAP Register Register
  | CMP Register Value
  | NEG Register
  | INC Register
  | DEC Register
  | ADD Register Value
  | SUB Register Value
  | MUL Register Value
  | DIV Register Value
  | NOT Register
  | AND Register Value
  | OR Register Value
  | XOR Register Value
  | NAND Register Value
  | NOR Register Value
  | NXOR Register Value
  | SHL Register Value
  | SHR Register Value
  | RCL Register Value
  | RCR Register Value
  | BSWAP Register
  | PUSH Value
  | DUP Value
  | POP Register
  | DROP
  | CALL Value
  | RET
  | JMP Value
  | JEQ Register Value
  | JNE Register Value
  | JGT Register Value
  | JGE Register Value
  | JLT Register Value
  | JLE Register Value
  | PRINT Value
  | EPRINT Value
  | DUMP
  deriving (Show)

type Cst = Int

data Value
  = Reg Register
  | Cst Cst
  | Lbl Int
  deriving (Show)

data Register
  = PC
  | SP
  | BP
  | RR
  | SR
  | FR
  | PR
  | C0
  | C1
  | S0
  | S1
  | I0
  | I1
  | L0
  | L1
  | F0
  | F1
  | D0
  | D1
  | P0
  | P1
  deriving (Show, Enum)

getTyRegs :: Type -> (Register, Register)
getTyRegs ty
  | ty `elem` [Type.Bool, Type.Char, Type.UChar] = (C0, C1)
  | ty `elem` [Type.Short, Type.UShort] = (S0, S1)
  | ty `elem` [Type.Int, Type.UInt] = (I0, I1)
  | ty `elem` [Type.Long, Type.ULong] = (L0, L1)
  -- | ty `elem` [Type.LLong, Type.ULLong] = (L0, L1)
  | ty == Type.Float = (F0, F1)
  | ty == Type.Double = (D0, D1)
  -- | ty == Type.LDouble = (D0, D1)
  | otherwise = error $ "No general use register for type : " ++ show ty

regLen :: (Num a) => Register -> a
regLen reg = case reg of
  PC -> 8
  SP -> 8
  BP -> 8
  RR -> 8
  SR -> 8
  FR -> 1
  PR -> 8
  C0 -> 1
  C1 -> 1
  S0 -> 2
  S1 -> 2
  I0 -> 4
  I1 -> 4
  L0 -> 8
  L1 -> 8
  F0 -> 4
  F1 -> 4
  D0 -> 8
  D1 -> 8
  P0 -> 8
  P1 -> 8
