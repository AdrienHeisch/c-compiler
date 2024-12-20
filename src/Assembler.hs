module Assembler (assemble) where

import Data.Bits (Bits (..))
import Data.Word (Word8)
import Instruction (Instruction (..), Program (..), Register, Value (..), regLen, toOpCode)

assemble :: Program -> [Word8]
assemble (Program ins) = secondPass ins

secondPass :: [Instruction] -> [Word8]
secondPass ins = case ins of
  [] -> []
  (instr : rest) -> bytecode instr ++ secondPass rest

bytecode :: Instruction -> [Word8]
bytecode instr = case instr of
  NOP -> make
  HALT (Reg r) -> make_R r
  HALT (Cst c) -> make_C c
  SYCALL (Reg r) -> make_R r
  SYCALL (Cst c) -> make_C c
  CLEAR r -> makeR_ r
  SET r (Reg r') -> makeRR r r'
  SET r (Cst c) -> makeRC r c
  LOAD r (Reg r') -> makeRR r r'
  LOAD r (Cst c) -> makeRC r c
  STORE r (Reg r') -> makeRR r r'
  STORE r (Cst c) -> makeRC r c
  SWAP r r' -> makeRR r r'
  CMP r (Reg r') -> makeRR r r'
  CMP r (Cst c) -> makeRC r c
  NEG r -> makeR_ r
  INC r -> makeR_ r
  DEC r -> makeR_ r
  ADD r (Reg r') -> makeRR r r'
  ADD r (Cst c) -> makeRC r c
  SUB r (Reg r') -> makeRR r r'
  SUB r (Cst c) -> makeRC r c
  MUL r (Reg r') -> makeRR r r'
  MUL r (Cst c) -> makeRC r c
  DIV r (Reg r') -> makeRR r r'
  DIV r (Cst c) -> makeRC r c
  MOD r (Reg r') -> makeRR r r'
  MOD r (Cst c) -> makeRC r c
  NOT r -> makeR_ r
  AND r (Reg r') -> makeRR r r'
  AND r (Cst c) -> makeRC r c
  OR r (Reg r') -> makeRR r r'
  OR r (Cst c) -> makeRC r c
  XOR r (Reg r') -> makeRR r r'
  XOR r (Cst c) -> makeRC r c
  NAND r (Reg r') -> makeRR r r'
  NAND r (Cst c) -> makeRC r c
  NOR r (Reg r') -> makeRR r r'
  NOR r (Cst c) -> makeRC r c
  NXOR r (Reg r') -> makeRR r r'
  NXOR r (Cst c) -> makeRC r c
  SHL r (Reg r') -> makeRR r r'
  SHL r (Cst c) -> makeRC r c
  SHR r (Reg r') -> makeRR r r'
  SHR r (Cst c) -> makeRC r c
  RCL r (Reg r') -> makeRR r r'
  RCL r (Cst c) -> makeRC r c
  RCR r (Reg r') -> makeRR r r'
  RCR r (Cst c) -> makeRC r c
  BSWAP r -> makeR_ r
  PUSH (Reg r) -> make_R r
  PUSH (Cst c) -> make_C c
  DUP (Reg r) -> make_R r
  DUP (Cst c) -> make_C c
  POP r -> makeR_ r
  DROP -> make
  CALL (Reg r) -> make_R r
  CALL (Cst c) -> make_C c
  RET (Reg r) -> make_R r
  RET (Cst c) -> make_C c
  JMP (Reg r) -> make_R r
  JMP (Cst c) -> make_C c
  JEQ r (Reg r') -> makeRR r r'
  JEQ r (Cst c) -> makeRC r c
  JNE r (Reg r') -> makeRR r r'
  JNE r (Cst c) -> makeRC r c
  JGT r (Reg r') -> makeRR r r'
  JGT r (Cst c) -> makeRC r c
  JGE r (Reg r') -> makeRR r r'
  JGE r (Cst c) -> makeRC r c
  JLT r (Reg r') -> makeRR r r'
  JLT r (Cst c) -> makeRC r c
  JLE r (Reg r') -> makeRR r r'
  JLE r (Cst c) -> makeRC r c
  PRINT (Reg r) -> make_R r
  PRINT (Cst c) -> make_C c
  EPRINT (Reg r) -> make_R r
  EPRINT (Cst c) -> make_C c
  DUMP -> make
  LABEL _ -> error "Label instructions should not appear at this point"
  _ -> error $ "Invalid operation : " ++ show instr
  where
    op = Instruction.toOpCode instr
    make = asmRR op Nothing (0 :: Int)
    makeR_ r = asmRR op (Just r) (0 :: Int)
    make_R r = asmRR op Nothing (regToInt r)
    make_C = asmRC op Nothing
    makeRR r r' = asmRR op (Just r) (regToInt r')
    makeRC r = asmRC op (Just r)

asmRR :: Word8 -> Maybe Register -> Int -> [Word8]
asmRR op Nothing val = [regFlag op, 0, intoByte val]
asmRR op (Just reg) val = [regFlag op, intoByte (regToInt reg), intoByte val]

asmRC :: Word8 -> Maybe Register -> Int -> [Word8]
asmRC op Nothing val = op : 0 : intoBytes val
asmRC op (Just reg) val = op : intoByte (regToInt reg) : intoBytes val

regFlag :: Word8 -> Word8
regFlag = (.|.) (0b10000000 :: Word8)

intoByte :: (Integral a) => a -> Word8
intoByte n = fromIntegral n :: Word8

intoBytes :: (Integral a1, Bits a1, Num a2) => a1 -> [a2]
intoBytes n = [fromIntegral ((n `shiftR` (i * 8)) .&. 0xFF) | i <- [0 .. regLen - 1]]

regToInt :: Register -> Int
regToInt = (.&. ((1 `shiftL` 64) - 1)) . fromEnum