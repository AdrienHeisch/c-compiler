import Data.Text.IO qualified as TIO
import Lexer qualified (lex)
import Preprocessor qualified (process)
import Parser qualified (parse)

main :: IO ()
main = do
  source <- TIO.readFile "code.c"
  let tokens = Lexer.lex source
  putStrLn "Lexer : "
  print tokens
  let preprocessed = Preprocessor.process tokens
  putStrLn "Preprocessor : "
  print preprocessed
  let topLevel = Parser.parse preprocessed
  putStrLn "Parser : "
  print topLevel
