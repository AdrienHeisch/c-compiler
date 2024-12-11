module Instruction (Program (..), Instruction (..), Register (..), Cst, Value (..)) where

import Data.List (intercalate)
import Utils (Display (display))

newtype Program = Program [Instruction]
  deriving (Show)

instance Utils.Display Program where
  display :: Program -> String
  display (Program ins) = intercalate "\n" . map show $ ins

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
