module Instruction (Program (..), Instruction (..), Register (..), Cst, Address) where

import Data.List (intercalate)
import Utils (Display (display))

newtype Program = Program [Instruction]
  deriving (Show)

instance Utils.Display Program where
  display :: Program -> String
  display (Program ins) = intercalate "\n" . map show $ ins

data Instruction
  = NOP
  | HALT Register
  | SYS_CALL Cst
  | CLEAR Register
  | CONST Register Cst
  | CPY Register Register
  | LOAD Register Address
  | STORE Register Address
  | SWAP Register Register
  | CMP Register Register
  | NEG Register
  | INC Register
  | DEC Register
  | ADDC Register Cst
  | SUBC Register Cst
  | MULC Register Cst
  | DIVC Register Cst
  | ADD Register Register
  | SUB Register Register
  | MUL Register Register
  | DIV Register Register
  | NOT Register
  | AND Register Register
  | OR Register Register
  | XOR Register Register
  | NAND Register Register
  | NOR Register Register
  | NXOR Register Register
  | SHL Register Register
  | SHR Register Register
  | RCL Register Register
  | RCR Register Register
  | BSWAP Register Register
  | PUSHC Cst
  | POPC Address
  | PUSH Register
  | POP Register
  | DROP
  | DUP Register
  | CALL Address
  | RET
  | JMP Address
  | JEQ Register Address
  | JNE Register Address
  | JGT Register Address
  | JGE Register Address
  | JLT Register Address
  | JLE Register Address
  | PRINT Register
  | EPRINT Register
  | DUMP
  deriving (Show)

type Cst = Int

type Address = Int

data Register
  = BP
  | SP
  | RR
  | SR
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
  deriving (Show, Eq)
