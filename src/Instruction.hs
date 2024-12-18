module Instruction (Program (..), Instruction (..), Register (..), Cst, Value (..), regLen, toOpCode) where

import Text.Printf (printf)
import Utils (Display (display))

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
  | SYCALL Value
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
  | MOD Register Value
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
  | RET Value
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
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  deriving (Show, Enum)

regLen :: (Num a) => a
regLen = 8

toOpCode :: (Num a) => Instruction -> a
toOpCode ins = case ins of
  NOP -> 0x00
  HALT _ -> 0x01
  SYCALL _ -> 0x02
  CLEAR _ -> 0x03
  SET _ _ -> 0x04
  LOAD _ _ -> 0x05
  STORE _ _ -> 0x06
  SWAP _ _ -> 0x07
  CMP _ _ -> 0x08
  NEG _ -> 0x09
  INC _ -> 0x0A
  DEC _ -> 0x0B
  ADD _ _ -> 0x0C
  SUB _ _ -> 0x0D
  MUL _ _ -> 0x0E
  DIV _ _ -> 0x0F
  MOD _ _ -> 0x10
  NOT _ -> 0x11
  AND _ _ -> 0x12
  OR _ _ -> 0x13
  XOR _ _ -> 0x14
  NAND _ _ -> 0x15
  NOR _ _ -> 0x16
  NXOR _ _ -> 0x17
  SHL _ _ -> 0x18
  SHR _ _ -> 0x19
  RCL _ _ -> 0x1A
  RCR _ _ -> 0x1B
  BSWAP _ -> 0x1C
  PUSH _ -> 0x1D
  DUP _ -> 0x1E
  POP _ -> 0x1F
  DROP -> 0x20
  CALL _ -> 0x21
  RET _ -> 0x22
  JMP _ -> 0x23
  JEQ _ _ -> 0x24
  JNE _ _ -> 0x25
  JGT _ _ -> 0x26
  JGE _ _ -> 0x27
  JLT _ _ -> 0x28
  JLE _ _ -> 0x29
  PRINT _ -> 0x2A
  EPRINT _ -> 0x2B
  DUMP -> 0x2C
  _ -> error "Not opcode"