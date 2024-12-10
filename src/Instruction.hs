module Instruction (Instruction (..), Register (..), Cst, Address) where

data Instruction
  = NOP
  | HALT Register
  | SYS_CALL Cst
  | CLEAR Register
  | CONST Register Cst
  | LOAD Register Address
  | STORE Register Address
  | SWAP Register Register
  | CMP Register Register
  | NEG Register
  | INC Register
  | DEC Register
  | ADDC Register Cst
  | SUBC Register Cst
  | MULC Register Cst
  | DIVC Register Cst
  | ADD Register Register
  | SUB Register Register
  | MUL Register Register
  | DIV Register Register
  | NOT Register
  | AND Register Register
  | OR Register Register
  | XOR Register Register
  | NAND Register Register
  | NOR Register Register
  | NXOR Register Register
  | SHL Register Register
  | SHR Register Register
  | RCL Register Register
  | RCR Register Register
  | BSWAP Register Register
  | PUSH Register
  | POP Register
  | DROP
  | DUP Register
  | CALL Address
  | RET
  | JMP Address
  | JEQ Register Address
  | JNE Register Address
  | JGT Register Address
  | JGE Register Address
  | JLT Register Address
  | JLE Register Address
  | PRINT Register
  | EPRINT Register
  | DUMP
  deriving (Show)

type Cst = Int

type Address = Int

data Register
  = C0
  | C1
  | S0
  | S1
  | I0
  | I1
  | L0
  | L1
  | F0
  | F1
  | D0
  | D1
  deriving (Show, Eq)