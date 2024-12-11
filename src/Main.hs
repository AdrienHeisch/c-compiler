import Parser qualified (parse)
import Preprocessor qualified (process)
import System.Environment (getArgs)
import Validator (validate)
import Utils (Display(..))
import Compiler (compile)

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
      putStrLn $ "Bytecode " ++ filePath ++ " :"
      putStrLn $ display $ compile topLevel
    else error "Invalid program"