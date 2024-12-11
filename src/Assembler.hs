module Assembler (assemble) where

import Data.Bits (Bits (..))
import Data.Word (Word8)
import Instruction (Instruction (..), Program (..), Register, Value (..), regLen)

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
  HALT (Cst c) -> makeC 0x01 c
  SYS_CALL (Reg r) -> makeR 0x02 r
  SYS_CALL (Cst c) -> makeC 0x02 c
  CLEAR r -> makeR 0x03 r
  SET r (Reg r') -> makeRR 0x04 r r'
  SET r (Cst c) -> makeRC 0x04 r c
  LOAD r (Reg r') -> makeRR 0x05 r r'
  LOAD r (Cst c) -> makeRC 0x05 r c
  STORE r (Reg r') -> makeRR 0x06 r r'
  STORE r (Cst c) -> makeRC 0x06 r c
  SWAP r r' -> makeRR 0x07 r r'
  CMP r (Reg r') -> makeRR 0x08 r r'
  CMP r (Cst c) -> makeRC 0x08 r c
  NEG r -> makeR 0x07 r
  INC r -> makeR 0x0A r
  DEC r -> makeR 0x0B r
  ADD r (Reg r') -> makeRR 0x0C r r'
  ADD _ (Cst 0) -> []
  ADD r (Cst c) -> makeRC 0x0C r c
  SUB r (Reg r') -> makeRR 0x0D r r'
  SUB r (Cst c) -> makeRC 0x0D r c
  MUL r (Reg r') -> makeRR 0x0E r r'
  MUL r (Cst c) -> makeRC 0x0E r c
  DIV r (Reg r') -> makeRR 0x0F r r'
  DIV r (Cst c) -> makeRC 0x0F r c
  NOT r -> makeR 0x10 r
  AND r (Reg r') -> makeRR 0x11 r r'
  AND r (Cst c) -> makeRC 0x11 r c
  OR r (Reg r') -> makeRR 0x12 r r'
  OR r (Cst c) -> makeRC 0x12 r c
  XOR r (Reg r') -> makeRR 0x13 r r'
  XOR r (Cst c) -> makeRC 0x13 r c
  NAND r (Reg r') -> makeRR 0x14 r r'
  NAND r (Cst c) -> makeRC 0x14 r c
  NOR r (Reg r') -> makeRR 0x15 r r'
  NOR r (Cst c) -> makeRC 0x15 r c
  NXOR r (Reg r') -> makeRR 0x16 r r'
  NXOR r (Cst c) -> makeRC 0x16 r c
  SHL r (Reg r') -> makeRR 0x17 r r'
  SHL r (Cst c) -> makeRC 0x17 r c
  SHR r (Reg r') -> makeRR 0x18 r r'
  SHR r (Cst c) -> makeRC 0x18 r c
  RCL r (Reg r') -> makeRR 0x19 r r'
  RCL r (Cst c) -> makeRC 0x19 r c
  RCR r (Reg r') -> makeRR 0x1A r r'
  RCR r (Cst c) -> makeRC 0x1A r c
  BSWAP r -> makeR 0x1B r
  PUSH (Reg r) -> makeR 0x1C r
  PUSH (Cst c) -> makeC 0x1C c
  DUP (Reg r) -> makeR 0x1D r
  DUP (Cst c) -> makeC 0x1D c
  POP r -> makeR 0x1E r
  DROP -> make 0x1F
  CALL (Reg r) -> makeR 0x20 r
  CALL (Cst c) -> makeC 0x20 c
  RET -> make 0x21
  JMP (Reg r) -> makeR 0x22 r
  JMP (Cst c) -> makeC 0x22 c
  -- JMP (Lbl l) -> makeL 0x22 l
  JEQ r (Reg r') -> makeRR 0x23 r r'
  JEQ r (Cst c) -> makeRC 0x23 r c
  -- JEQ r (Lbl l) -> makeRL 0x23 r l
  JNE r (Reg r') -> makeRR 0x24 r r'
  JNE r (Cst c) -> makeRC 0x24 r c
  -- JNE r (Lbl l) -> makeRL 0x24 r l
  JGT r (Reg r') -> makeRR 0x25 r r'
  JGT r (Cst c) -> makeRC 0x25 r c
  -- JGT r (Lbl l) -> makeRL 0x25 r l
  JGE r (Reg r') -> makeRR 0x26 r r'
  JGE r (Cst c) -> makeRC 0x26 r c
  -- JGE r (Lbl l) -> makeRL 0x26 r l
  JLT r (Reg r') -> makeRR 0x27 r r'
  JLT r (Cst c) -> makeRC 0x27 r c
  -- JLT r (Lbl l) -> makeRL 0x27 r l
  JLE r (Reg r') -> makeRR 0x28 r r'
  JLE r (Cst c) -> makeRC 0x28 r c
  -- JLE r (Lbl l) -> makeRL 0x4D r l
  PRINT (Reg r) -> makeR 0x29 r
  PRINT (Cst c) -> makeC 0x29 c
  EPRINT (Reg r) -> makeR 0x2A r
  EPRINT (Cst c) -> makeC 0x2A c
  DUMP -> make 0x2B
  LABEL _ -> error "Can't transalte label into opcode"
  _ -> error $ "Invalid operation : " ++ show instr
  where
    regFlag = (.|.) (0b10000000 :: Word8)

    asmRR :: Word8 -> Maybe Register -> Int -> [Word8]
    asmRR op Nothing val = [regFlag op, 0, intoByte val]
    asmRR op (Just reg) val = [regFlag op, intoByte (regToInt reg), intoByte val]

    asmRC :: Word8 -> Maybe Register -> Int -> [Word8]
    asmRC op Nothing val = op : 0 : intoByteArray val 8
    asmRC op (Just reg) val = op : intoByte (regToInt reg) : intoByteArray val (regLen reg)
    
    make op = asmRR op Nothing (0 :: Int)
    makeR op r = asmRR op (Just r) (0 :: Int)
    makeC op = asmRC op Nothing
    -- makeL op l = asm op Nothing $ error "Labels not implemented"
    makeRR op r r' = asmRR op (Just r) (regToInt r')
    makeRC op r = asmRC op (Just r)
    -- makeRL op r l = asmRC op (reg r) $ error "Labels not implemented"

    intoByte n = fromIntegral n :: Word8
    intoByteArray n len = [fromIntegral ((n `shiftR` (i * 8)) .&. 0xFF) | i <- [0 .. len - 1]]

    regToInt = (.&. ((1 `shiftL` 64) - 1)) . fromEnum
