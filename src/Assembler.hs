module Assembler (assemble) where

import Control.Monad.State.Lazy (State, evalState, runState, get, modify, put)
import Data.Bits (Bits (..))
import Data.Word (Word8)
import Instruction (Instruction (..), Program (..), Register, Value (..), regLen)
import Debug.Trace (trace)

type AsmState = State (Int, [Int])

type PartialJmp = (Word8, Maybe Register, Int)

assemble :: Program -> [Word8]
assemble (Program ins) = evalState (firstPass ins >>= secondPass) (0, [])

pcAdd :: Num a => a -> (a, b) -> (a, b)
pcAdd n st = let (pc, lbls) = st in (pc + n, lbls)

secondPass :: [Either [Word8] PartialJmp] -> AsmState [Word8]
secondPass ins = case ins of
  [] -> return []
  (Left bytes : rest) -> do
    rest' <- secondPass rest
    return $ bytes ++ rest'
  (Right (op, reg, lbl) : rest) -> do
    rest' <- secondPass rest
    modify $ pcAdd 1
    addr <- getLabel lbl
    let !_ = trace ("Jump: op " ++ show op ++ " reg " ++ show reg ++ " lbl " ++ show lbl ++ " -> addr " ++ show addr) ()
    return $ asmRC op reg addr ++ rest'

firstPass :: [Instruction] -> AsmState [Either [Word8] PartialJmp]
firstPass ins = case ins of
  [] -> return []
  (instr : rest) -> do
    mbytes <- bytecode instr
    case mbytes of
      Nothing -> firstPass rest
      Just bytes -> do
        modify $ pcAdd 1
        rest' <- firstPass rest
        return $ bytes : rest'
        -- return $ transform bytes ++ rest'
  -- where
    -- transform (Left bytes) = map Left bytes
    -- transform (Right jmp) = [Right jmp]

bytecode :: Instruction -> AsmState (Maybe (Either [Word8] PartialJmp))
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
  JMP (Lbl l) -> makeL 0x22 l
  JEQ r (Reg r') -> makeRR 0x23 r r'
  JEQ r (Cst c) -> makeRC 0x23 r c
  JEQ r (Lbl l) -> makeRL 0x23 r l
  JNE r (Reg r') -> makeRR 0x24 r r'
  JNE r (Cst c) -> makeRC 0x24 r c
  JNE r (Lbl l) -> makeRL 0x24 r l
  JGT r (Reg r') -> makeRR 0x25 r r'
  JGT r (Cst c) -> makeRC 0x25 r c
  JGT r (Lbl l) -> makeRL 0x25 r l
  JGE r (Reg r') -> makeRR 0x26 r r'
  JGE r (Cst c) -> makeRC 0x26 r c
  JGE r (Lbl l) -> makeRL 0x26 r l
  JLT r (Reg r') -> makeRR 0x27 r r'
  JLT r (Cst c) -> makeRC 0x27 r c
  JLT r (Lbl l) -> makeRL 0x27 r l
  JLE r (Reg r') -> makeRR 0x28 r r'
  JLE r (Cst c) -> makeRC 0x28 r c
  JLE r (Lbl l) -> makeRL 0x4D r l
  PRINT (Reg r) -> makeR 0x29 r
  PRINT (Cst c) -> makeC 0x29 c
  EPRINT (Reg r) -> makeR 0x2A r
  EPRINT (Cst c) -> makeC 0x2A c
  DUMP -> make 0x2B
  LABEL l -> do
    newLabel l
    return Nothing
  _ -> error $ "Invalid operation : " ++ show instr
  where
    make op = return $ Just $ Left $ asmRR op Nothing (0 :: Int)
    makeR op r = return $ Just $ Left $ asmRR op (Just r) (0 :: Int)
    makeRR op r r' = return $ Just $ Left $ asmRR op (Just r) (regToInt r')
    makeC op c = return $ Just $ Left $ asmRC op Nothing c
    makeRC op r c = return $ Just $ Left $ asmRC op (Just r) c

    makeL op l = return $ Just $ Right (op, Nothing, l)
    makeRL op r l = return $ Just $ Right (op, Just r, l)

asmRR :: Word8 -> Maybe Register -> Int -> [Word8]
asmRR op Nothing val = [regFlag op, 0, intoByte val]
asmRR op (Just reg) val = [regFlag op, intoByte (regToInt reg), intoByte val]

asmRC :: Word8 -> Maybe Register -> Int -> [Word8]
asmRC op Nothing val = op : 0 : intoByteArray val 8
asmRC op (Just reg) val = op : intoByte (regToInt reg) : intoByteArray val (regLen reg)

regFlag :: Word8 -> Word8
regFlag = (.|.) (0b10000000 :: Word8)

intoByte :: Integral a => a -> Word8
intoByte n = fromIntegral n :: Word8

intoByteArray :: (Integral a1, Bits a1, Num a2) => a1 -> Int -> [a2]
intoByteArray n len = [fromIntegral ((n `shiftR` (i * 8)) .&. 0xFF) | i <- [0 .. len - 1]]

regToInt :: Register -> Int
regToInt = (.&. ((1 `shiftL` 64) - 1)) . fromEnum

-- withLabel :: (Int -> [Word8]) -> Int -> State Status [Word8]
-- withLabel f lbl = do
--   addr <- getLabel lbl
--   return $ f addr

getLabel :: Int -> AsmState Int
getLabel lbl = do
  (_, labels) <- get
  -- let !_ = trace ("get: " ++ show labels) ()
  return $ labels !! lbl

newLabel :: Int -> AsmState ()
newLabel lbl = do -- TODO remove this parameter and related code in compiler
  (pc, labels) <- get
  -- let newLabels = case length labels of
  --       len | len + 1 == lbl -> labels ++ [pc]
  --       len | len < lbl -> take lbl labels ++ [pc]
  --       _ -> error "Can't declare label"
  -- let !_ = trace ("set: " ++ show newLabels) ()
  let newLabels = labels ++ [pc]
  put (pc, newLabels)