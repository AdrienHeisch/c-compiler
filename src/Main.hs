import Declaration (Declaration)
import Parser qualified (parse)
import Preprocessor qualified (process)
import System.Environment (getArgs)
import Validator (validate)
import Utils (Display(..))

main :: IO ()
main = do
  args <- getArgs
  mapM_ compileFile args

compileFile :: FilePath -> IO [Declaration]
compileFile filePath = do
  tokens <- Preprocessor.process filePath
  putStrLn $ "Preprocessor " ++ filePath ++ " :"
  putStrLn $ display tokens
  let topLevel = Parser.parse tokens
  putStrLn $ "Parser " ++ filePath ++ " :"
  putStrLn $ display topLevel
  if validate topLevel
    then return topLevel
    else error "Invalid program"