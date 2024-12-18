import Assembler (assemble)
import Compiler (compile)
import Data.ByteString qualified as BS
import Linker (link)
import Parser qualified (parse)
import Preprocessor qualified (process)
import System.Environment (getArgs)
import Utils (Display (..))

main :: IO ()
main = do
  args <- getArgs
  mapM_ compileFile args

compileFile :: FilePath -> IO ()
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
  let linkedProgram = Linker.link asm
  let bytecode = assemble linkedProgram
  print bytecode
  let bytes = BS.pack bytecode
  BS.writeFile "output.bin" bytes