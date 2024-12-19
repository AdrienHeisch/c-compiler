module Linker (link) where

import Control.Monad.State.Lazy (State, evalState, get, modify, put)
import Data.List (find)
import Instruction (Instruction (..), Program (..), Register (RR), Value (..))

type AsmState = State (Int, [(String, Int)])

startProcedure :: [Instruction]
startProcedure = [CALL (Lbl "main"), HALT (Reg RR)]

link :: [Program] -> Program
link programs =
  let ins = startProcedure ++ concatMap (\(Program ins') -> ins') programs
   in Program $ evalState (firstPass ins >>= secondPass) (0, [])

pcAdd :: (Num a) => a -> (a, b) -> (a, b)
pcAdd n st = let (pc, lbls) = st in (pc + n, lbls)

firstPass :: [Instruction] -> AsmState [Instruction]
firstPass ins = case ins of
  [] -> return []
  (LABEL lbl : rest) -> do
    newLabel lbl
    firstPass rest
  (instr : rest) -> do
    modify $ pcAdd 1
    rest' <- firstPass rest
    return $ instr : rest'

newLabel :: String -> AsmState ()
newLabel lbl = do
  (pc, labels) <- get
  let labels' = labels ++ [(lbl, pc)]
  put (pc, labels')

secondPass :: [Instruction] -> AsmState [Instruction]
secondPass ins = case ins of
  [] -> return []
  (instr : rest) -> do
    instr' <- replaceLabels instr
    rest' <- secondPass rest
    return $ instr' : rest'

replaceLabels :: Instruction -> AsmState Instruction
replaceLabels instr = case instr of
  CALL (Lbl lbl) -> do cst <- replace lbl; return $ CALL cst
  JMP (Lbl lbl) -> do cst <- replace lbl; return $ JMP cst
  JEQ r (Lbl lbl) -> do cst <- replace lbl; return $ JEQ r cst
  JNE r (Lbl lbl) -> do cst <- replace lbl; return $ JNE r cst
  JGT r (Lbl lbl) -> do cst <- replace lbl; return $ JGT r cst
  JGE r (Lbl lbl) -> do cst <- replace lbl; return $ JGE r cst
  JLT r (Lbl lbl) -> do cst <- replace lbl; return $ JLT r cst
  JLE r (Lbl lbl) -> do cst <- replace lbl; return $ JLE r cst
  LABEL _ -> error "Label instructions should not appear at this point"
  _ -> return instr
  where
    replace :: String -> AsmState Value
    replace lbl = do
      (_, labels) <- get
      case find ((lbl ==) . fst) labels of
        Just (_, addr) -> return $ Cst addr
        Nothing -> error $ "Label not found: " ++ show lbl
