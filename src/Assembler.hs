module Assembler where

import Data.Bits (Bits (..))
import Data.Word (Word8)
import Instruction (Instruction (..), Program (..), Register, Value (..))

assemble :: Program -> [Word8]
assemble (Program ins) = go ins
  where
    go ins' = case ins' of
      [] -> []
      (instr : rest) -> bytecode instr ++ go rest

bytecode :: Instruction -> [Word8]
bytecode instr = case instr of
  NOP -> make 0x00
  HALT (Reg r) -> makeR 0x01 r
  HALT (Cst c) -> makeC 0x02 c
  SYS_CALL (Reg r) -> makeR 0x03 r
  SYS_CALL (Cst c) -> makeC 0x04 c
  CLEAR r -> makeR 0x05 r
  SET r (Reg r') -> makeRR 0x06 r r'
  SET r (Cst c) -> makeRC 0x07 r c
  LOAD r (Reg r') -> makeRR 0x08 r r'
  LOAD r (Cst c) -> makeRC 0x09 r c
  STORE r (Reg r') -> makeRR 0x0A r r'
  STORE r (Cst c) -> makeRC 0x0B r c
  SWAP r r' -> makeRR 0x0C r r'
  CMP r (Reg r') -> makeRR 0x0D r r'
  CMP r (Cst c) -> makeRC 0x0E r c
  NEG r -> makeR 0x0F r
  INC r -> makeR 0x10 r
  DEC r -> makeR 0x11 r
  ADD r (Reg r') -> makeRR 0x12 r r'
  ADD _ (Cst 0) -> []
  ADD r (Cst c) -> makeRC 0x13 r c
  SUB r (Reg r') -> makeRR 0x14 r r'
  SUB r (Cst c) -> makeRC 0x15 r c
  MUL r (Reg r') -> makeRR 0x16 r r'
  MUL r (Cst c) -> makeRC 0x17 r c
  DIV r (Reg r') -> makeRR 0x18 r r'
  DIV r (Cst c) -> makeRC 0x19 r c
  NOT r -> makeR 0x1A r
  AND r (Reg r') -> makeRR 0x1B r r'
  AND r (Cst c) -> makeRC 0x1C r c
  OR r (Reg r') -> makeRR 0x1D r r'
  OR r (Cst c) -> makeRC 0x1E r c
  XOR r (Reg r') -> makeRR 0x1F r r'
  XOR r (Cst c) -> makeRC 0x20 r c
  NAND r (Reg r') -> makeRR 0x21 r r'
  NAND r (Cst c) -> makeRC 0x22 r c
  NOR r (Reg r') -> makeRR 0x23 r r'
  NOR r (Cst c) -> makeRC 0x24 r c
  NXOR r (Reg r') -> makeRR 0x25 r r'
  NXOR r (Cst c) -> makeRC 0x26 r c
  SHL r (Reg r') -> makeRR 0x27 r r'
  SHL r (Cst c) -> makeRC 0x28 r c
  SHR r (Reg r') -> makeRR 0x29 r r'
  SHR r (Cst c) -> makeRC 0x2A r c
  RCL r (Reg r') -> makeRR 0x2B r r'
  RCL r (Cst c) -> makeRC 0x2C r c
  RCR r (Reg r') -> makeRR 0x2D r r'
  RCR r (Cst c) -> makeRC 0x2E r c
  BSWAP r -> makeR 0x2F r
  PUSH (Reg r) -> makeR 0x30 r
  PUSH (Cst c) -> makeC 0x31 c
  DUP (Reg r) -> makeR 0x32 r
  DUP (Cst c) -> makeC 0x33 c
  POP r -> makeR 0x34 r
  DROP -> make 0x35
  CALL (Reg r) -> makeR 0x36 r
  CALL (Cst c) -> makeC 0x37 c
  RET -> make 0x38
  JMP (Reg r) -> makeR 0x39 r
  JMP (Cst c) -> makeC 0x3A c
  -- JMP (Lbl l) -> makeL 0x3B l
  JEQ r (Reg r') -> makeRR 0x3C r r'
  JEQ r (Cst c) -> makeRC 0x3D r c
  -- JEQ r (Lbl l) -> makeRL 0x3E r l
  JNE r (Reg r') -> makeRR 0x3F r r'
  JNE r (Cst c) -> makeRC 0x40 r c
  -- JNE r (Lbl l) -> makeRL 0x41 r l
  JGT r (Reg r') -> makeRR 0x42 r r'
  JGT r (Cst c) -> makeRC 0x43 r c
  -- JGT r (Lbl l) -> makeRL 0x44 r l
  JGE r (Reg r') -> makeRR 0x45 r r'
  JGE r (Cst c) -> makeRC 0x46 r c
  -- JGE r (Lbl l) -> makeRL 0x47 r l
  JLT r (Reg r') -> makeRR 0x48 r r'
  JLT r (Cst c) -> makeRC 0x49 r c
  -- JLT r (Lbl l) -> makeRL 0x4A r l
  JLE r (Reg r') -> makeRR 0x4B r r'
  JLE r (Cst c) -> makeRC 0x4C r c
  -- JLE r (Lbl l) -> makeRL 0x4D r l
  PRINT (Reg r) -> makeR 0x4E r
  PRINT (Cst c) -> makeC 0x4F c
  EPRINT (Reg r) -> makeR 0x50 r
  EPRINT (Cst c) -> makeC 0x51 c
  DUMP -> make 0x53
  LABEL _ -> error "Can't transalte label into opcode"
  _ -> error $ "Invalid operation : " ++ show instr
  where
    asm :: Word8 -> Int -> Int -> [Word8]
    asm code left right = code : intToByteArray left ++ intToByteArray right

    make op = asm op 0 0
    makeR op r = asm op (reg r) 0
    makeC op c = asm op c 0
    -- makeL op l = asm op $ error "Labels not implemented"
    makeRR op r r' = asm op (reg r) (reg r')
    makeRC op r = asm op (reg r)
    -- makeRL op r l = asm op (reg r) $ error "Labels not implemented"

intToByteArray :: Int -> [Word8]
intToByteArray n = [fromIntegral ((n `shiftR` (i * 8)) .&. 0xFF) | i <- [0 .. 7]]

reg :: Register -> Int
reg = (.&. ((1 `shiftL` 64) - 1)) . fromEnum
