module Instruction (Program (..), Instruction (..), Register (..), Cst, Value (..), regLen, len, toOpCode) where

import Data.Word (Word8)
import Text.Printf (printf)
import Utils (Display (display))

newtype Program = Program [Instruction]
  deriving (Show)

instance Utils.Display Program where
  display :: Program -> String
  display (Program ins) = go 0 ins
    where
      go (pc :: Int) ins' = case ins' of
        [] -> ""
        LABEL lbl : rest -> show lbl ++ ":\n" ++ go pc rest
        instr : rest -> "  " ++ printf "%04d" pc ++ "  " ++ show instr ++ "\n" ++ go (pc + len instr) rest

data Instruction
  = LABEL String
  | STATIC String [Word8]
  | NOP
  | HALT Value
  | SYCALL Value
  | CLEAR Register
  | SET Register Value
  | LOAD Register Value
  | STRB Register Value
  | STRH Register Value
  | STRW Register Value
  | STRD Register Value
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
  | LR
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
  STRB _ _ -> 0x06
  STRH _ _ -> 0x07
  STRW _ _ -> 0x08
  STRD _ _ -> 0x09
  SWAP _ _ -> 0x0A
  CMP _ _ -> 0x0B
  NEG _ -> 0x0C
  INC _ -> 0x0D
  DEC _ -> 0x0E
  ADD _ _ -> 0x0F
  SUB _ _ -> 0x10
  MUL _ _ -> 0x11
  DIV _ _ -> 0x12
  MOD _ _ -> 0x13
  NOT _ -> 0x14
  AND _ _ -> 0x15
  OR _ _ -> 0x16
  XOR _ _ -> 0x17
  NAND _ _ -> 0x18
  NOR _ _ -> 0x19
  NXOR _ _ -> 0x1A
  SHL _ _ -> 0x1B
  SHR _ _ -> 0x1C
  RCL _ _ -> 0x1D
  RCR _ _ -> 0x1E
  BSWAP _ -> 0x1F
  PUSH _ -> 0x20
  DUP _ -> 0x21
  POP _ -> 0x22
  DROP -> 0x23
  CALL _ -> 0x24
  RET _ -> 0x25
  JMP _ -> 0x26
  JEQ _ _ -> 0x27
  JNE _ _ -> 0x28
  JGT _ _ -> 0x29
  JGE _ _ -> 0x2A
  JLT _ _ -> 0x2B
  JLE _ _ -> 0x2C
  PRINT _ -> 0x2D
  EPRINT _ -> 0x2E
  DUMP -> 0x2F
  _ -> error "Not opcode"

len :: (Num a) => Instruction -> a
len ins = case ins of
  LABEL _ -> 0
  STATIC _ bytes -> fromIntegral $ length bytes
  NOP -> 3
  HALT v -> 2 + valueLen v
  SYCALL v -> 2 + valueLen v
  CLEAR _ -> 3
  SET _ v -> 2 + valueLen v
  LOAD _ v -> 2 + valueLen v
  STRB _ v -> 2 + valueLen v
  STRH _ v -> 2 + valueLen v
  STRW _ v -> 2 + valueLen v
  STRD _ v -> 2 + valueLen v
  SWAP _ _ -> 3
  CMP _ v -> 2 + valueLen v
  NEG _ -> 3
  INC _ -> 3
  DEC _ -> 3
  ADD _ v -> 2 + valueLen v
  SUB _ v -> 2 + valueLen v
  MUL _ v -> 2 + valueLen v
  DIV _ v -> 2 + valueLen v
  MOD _ v -> 2 + valueLen v
  NOT _ -> 3
  AND _ v -> 2 + valueLen v
  OR _ v -> 2 + valueLen v
  XOR _ v -> 2 + valueLen v
  NAND _ v -> 2 + valueLen v
  NOR _ v -> 2 + valueLen v
  NXOR _ v -> 2 + valueLen v
  SHL _ v -> 2 + valueLen v
  SHR _ v -> 2 + valueLen v
  RCL _ v -> 2 + valueLen v
  RCR _ v -> 2 + valueLen v
  BSWAP _ -> 3
  PUSH v -> 2 + valueLen v
  DUP v -> 2 + valueLen v
  POP _ -> 3
  DROP -> 3
  CALL v -> 2 + valueLen v
  RET v -> 2 + valueLen v
  JMP v -> 2 + valueLen v
  JEQ _ v -> 2 + valueLen v
  JNE _ v -> 2 + valueLen v
  JGT _ v -> 2 + valueLen v
  JGE _ v -> 2 + valueLen v
  JLT _ v -> 2 + valueLen v
  JLE _ v -> 2 + valueLen v
  PRINT v -> 2 + valueLen v
  EPRINT v -> 2 + valueLen v
  DUMP -> 3
  where
    valueLen (Reg _) = 1
    valueLen (Cst _) = regLen
    valueLen (Lbl _) = regLen