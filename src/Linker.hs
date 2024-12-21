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
  SET r (Lbl lbl) -> mk (SET r) lbl
  CALL (Lbl lbl) -> mk CALL lbl
  JMP (Lbl lbl) -> mk JMP lbl
  JEQ r (Lbl lbl) -> mk (JEQ r) lbl
  JNE r (Lbl lbl) -> mk (JNE r) lbl
  JGT r (Lbl lbl) -> mk (JGT r) lbl
  JGE r (Lbl lbl) -> mk (JGE r) lbl
  JLT r (Lbl lbl) -> mk (JLT r) lbl
  JLE r (Lbl lbl) -> mk (JLE r) lbl
  LABEL _ -> error "Label instructions should not appear at this point"
  _ -> return instr
  where
    mk :: (Value -> Instruction) -> String -> AsmState Instruction
    mk f lbl = do cst <- replace lbl; return $ f cst

    replace :: String -> AsmState Value
    replace lbl = do
      (_, labels) <- get
      case find ((lbl ==) . fst) labels of
        Just (_, addr) -> return $ Cst addr
        Nothing -> error $ "Label not found: " ++ show lbl
