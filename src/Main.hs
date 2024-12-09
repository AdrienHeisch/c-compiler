import Assembler (assemble)
import Compiler (compile)
import Data.ByteString qualified as BS
import Parser qualified (parse)
import Preprocessor qualified (process)
import System.Environment (getArgs)
import Utils (Display (..))
import Validator (validate)

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
  if validate topLevel
    then do
      let asm = compile topLevel
      putStrLn $ "Assembly " ++ filePath ++ " :"
      putStrLn $ display asm
      let bytecode = assemble asm
      print bytecode
      let bytes = BS.pack bytecode
      BS.writeFile "output.bin" bytes
    else error "Invalid program"