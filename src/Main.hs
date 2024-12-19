import Assembler (assemble)
import Compiler (compile)
import Data.ByteString qualified as BS
import Instruction (Program)
import Linker (link)
import Parser qualified (parse)
import Preprocessor qualified (process)
import System.Environment (getArgs)
import Utils (Display (..))

main :: IO ()
main = do
  args <- getArgs
  files <- mapM compileFile args
  let linkedProgram = Linker.link files
  let bytecode = assemble linkedProgram
  print bytecode
  let bytes = BS.pack bytecode
  BS.writeFile "output.bin" bytes

compileFile :: FilePath -> IO Program
compileFile filePath = do
  tokens <- Preprocessor.process filePath
  putStrLn $ "Preprocessor " ++ filePath ++ " :"
  putStrLn $ display tokens
  let topLevel = Parser.parse tokens
  putStrLn $ "Parser " ++ filePath ++ " :"
  putStrLn $ display topLevel
  let asm = compile topLevel
  putStrLn $ "Assembly " ++ filePath ++ " :"
  putStrLn $ display asm
  return asm