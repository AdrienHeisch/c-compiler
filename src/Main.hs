import Data.Text.IO qualified as TIO
import Lexer

main :: IO ()
main = do
  source <- TIO.readFile "bigcode.c"
  let tokens = Lexer.lex source
  print tokens
