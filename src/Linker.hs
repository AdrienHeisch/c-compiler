module Linker (link) where

import Instruction (Program)

link :: [Program] -> Program
link = head