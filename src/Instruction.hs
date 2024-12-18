module Instruction (Program (..), Instruction (..), Register (..), Cst, Value (..), regLen) where

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
        LABEL lbl : rest -> show lbl ++ ":\n" ++ go pc rest
        instr : rest -> "  " ++ printf "%04d" pc ++ "  " ++ show instr ++ "\n" ++ go (pc + 1) rest

data Instruction
  = LABEL String -- TODO put this in a separate symbol type ?
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
  | Lbl String
  deriving (Show)

data Register
  = PC
  | SP
  | BP
  | RR
  | SR
  | FR
  | PR
  | R0
  | R1
  | R3
  | R4
  | R5
  | R6
  | R7
  deriving (Show, Enum)

regLen :: Int
regLen = 8
