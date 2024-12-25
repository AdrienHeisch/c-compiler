module Linker (link) where

import Control.Monad.State.Lazy (State, evalState, get, modify, put)
import Data.List (find)
import Data.Word (Word8)
import Instruction (Instruction (..), Program (..), Register (RR), Value (..))
import Instruction qualified

type AsmState = State (Int, [(String, Int)], [(String, [Word8])])

startProcedure :: [Instruction]
startProcedure = [CALL (Lbl "main"), HALT (Reg RR)]

link :: [Program] -> Program
link programs =
  let ins = startProcedure ++ concatMap (\(Program ins') -> ins') programs
   in Program $ evalState (firstPass ins >>= secondPass >>= thirdPass) (0, [], [])

pcAdd :: (Num a) => a -> (a, b, c) -> (a, b, c)
pcAdd n st = let (pc, lbls, d) = st in (pc + n, lbls, d)

firstPass :: [Instruction] -> AsmState [Instruction]
firstPass ins = case ins of
  [] -> return []
  LABEL lbl : rest -> do
    newLabel lbl
    firstPass rest
  STATIC name bytes : rest -> do
    addStatic name bytes
    firstPass rest
  instr : rest -> do
    modify $ pcAdd $ Instruction.len instr
    rest' <- firstPass rest
    return $ instr : rest'

newLabel :: String -> AsmState ()
newLabel lbl = do
  (pc, labels, d) <- get
  let labels' = labels ++ [(lbl, pc)]
  put (pc, labels', d)

addStatic :: String -> [Word8] -> AsmState ()
addStatic name bytes = do
  (p, l, dts) <- get
  let dts' = dts ++ [(name, bytes)]
  put (p, l, dts')

secondPass :: [Instruction] -> AsmState [Instruction]
secondPass ins = do
  (_, _, static) <- get
  static' <- go static
  return $ ins ++ static'
  where
    go :: [(String, [Word8])] -> AsmState [Instruction]
    go dts = case dts of
      [] -> return []
      (name, bytes) : rest -> do
        newLabel name
        modify $ pcAdd (length bytes)
        rest' <- go rest
        return $ STATIC name bytes : rest'

thirdPass :: [Instruction] -> AsmState [Instruction]
thirdPass ins = case ins of
  [] -> return []
  (instr : rest) -> do
    instr' <- replaceLabels instr
    rest' <- thirdPass rest
    return $ instr' : rest'

replaceLabels :: Instruction -> AsmState Instruction
replaceLabels instr = case instr of
  HALT (Lbl lbl) -> mk HALT lbl
  SYCALL (Lbl lbl) -> mk SYCALL lbl
  SET r (Lbl lbl) -> mk (SET r) lbl
  LOAD r (Lbl lbl) -> mk (LOAD r) lbl
  STRB r (Lbl lbl) -> mk (STRW r) lbl
  STRH r (Lbl lbl) -> mk (STRW r) lbl
  STRW r (Lbl lbl) -> mk (STRW r) lbl
  STRD r (Lbl lbl) -> mk (STRW r) lbl
  CMP r (Lbl lbl) -> mk (CMP r) lbl
  ADD r (Lbl lbl) -> mk (ADD r) lbl
  SUB r (Lbl lbl) -> mk (SUB r) lbl
  MUL r (Lbl lbl) -> mk (MUL r) lbl
  DIV r (Lbl lbl) -> mk (DIV r) lbl
  MOD r (Lbl lbl) -> mk (MOD r) lbl
  AND r (Lbl lbl) -> mk (AND r) lbl
  OR r (Lbl lbl) -> mk (OR r) lbl
  XOR r (Lbl lbl) -> mk (XOR r) lbl
  NAND r (Lbl lbl) -> mk (NAND r) lbl
  NOR r (Lbl lbl) -> mk (NOR r) lbl
  NXOR r (Lbl lbl) -> mk (NXOR r) lbl
  SHL r (Lbl lbl) -> mk (SHL r) lbl
  SHR r (Lbl lbl) -> mk (SHR r) lbl
  RCL r (Lbl lbl) -> mk (RCL r) lbl
  RCR r (Lbl lbl) -> mk (RCR r) lbl
  PUSH (Lbl lbl) -> mk PUSH lbl
  DUP (Lbl lbl) -> mk DUP lbl
  CALL (Lbl lbl) -> mk CALL lbl
  RET (Lbl lbl) -> mk RET lbl
  JMP (Lbl lbl) -> mk JMP lbl
  JEQ r (Lbl lbl) -> mk (JEQ r) lbl
  JNE r (Lbl lbl) -> mk (JNE r) lbl
  JGT r (Lbl lbl) -> mk (JGT r) lbl
  JGE r (Lbl lbl) -> mk (JGE r) lbl
  JLT r (Lbl lbl) -> mk (JLT r) lbl
  JLE r (Lbl lbl) -> mk (JLE r) lbl
  PRINT (Lbl lbl) -> mk PRINT lbl
  EPRINT (Lbl lbl) -> mk EPRINT lbl
  LABEL _ -> error "Label instructions should not appear at this point"
  _ -> return instr
  where
    mk :: (Value -> Instruction) -> String -> AsmState Instruction
    mk f lbl = do cst <- replace lbl; return $ f cst

    replace :: String -> AsmState Value
    replace lbl = do
      (_, labels, _) <- get
      case find ((lbl ==) . fst) labels of
        Just (_, addr) -> return $ Cst addr
        Nothing -> error $ "Label not found: " ++ show lbl
