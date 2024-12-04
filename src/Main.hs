import Data.Text.IO qualified as TIO
import Lexer qualified (lex)
import Parser qualified (parse)

main :: IO ()
main = do
  source <- TIO.readFile "bigcode.c"
  let tokens = Lexer.lex source
  print ("Lexer : " ++ show tokens)
  let topLevel = Parser.parse tokens
  print ("Parser : " ++ show topLevel)
