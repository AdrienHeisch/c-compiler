module Compiler where

-- import Instruction
import Data.Word (Word8)
import Declaration (Declaration)

compile :: [Declaration] -> [Word8]
compile _ = [0x00, 0x01, 0xFF]