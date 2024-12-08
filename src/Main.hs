import Declaration (Declaration)
import Parser qualified (parse)
import Preprocessor qualified (process)
import System.Environment (getArgs)
import Token (Token)

main :: IO ()
main = do
  args <- getArgs
  mapM_ compileFile args

compileFile :: FilePath -> IO [Declaration]
compileFile filePath = do
  tokens <- addFile filePath
  let topLevel = Parser.parse tokens
  putStrLn $ "Parser " ++ filePath ++ " :"
  print topLevel
  return topLevel

addFile :: FilePath -> IO [Token]
addFile filePath = do
  tokens <- Preprocessor.process filePath
  putStrLn $ "Preprocessor " ++ filePath ++ " :"
  print tokens
  return tokens