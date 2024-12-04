module Parser (parse) where

import Identifier (Id)
import Op qualified
import Structures as Declaration (Declaration (..))
import Structures as Expr (Expr (..))
import Structures as Statement (Statement (..))
import Token (Token (..))
import Token as Delimiter (Delimiter (..))
import Type (Type (..))

parse :: [Token] -> [Declaration]
parse [] = []
parse [Eof] = []
parse (Token.Directive : tks) =
  let (dirTks, rest) = collectDirective tks
   in parseDirective dirTks : parse rest
parse (Token.Struct : Token.Id _ : Token.DelimOpen Delimiter.Br : tks) =
  let (structTks, rest) = collectStruct tks
   in parseStruct structTks : parse rest
parse (Token.Type ty : Token.Id name : Token.DelimOpen Delimiter.Pr : Token.DelimClose Delimiter.Pr : Token.DelimOpen Delimiter.Br : tks) =
  let (funcDecTks, rest) = collectFuncDec tks
   in parseFuncDec ty name funcDecTks : parse rest
parse (Token.NL : tks) = parse tks
parse (tk : _) = error ("Unexpected token " ++ show tk)

collectUntil :: Token -> [Token] -> ([Token], [Token])
collectUntil _ [] = ([], [])
collectUntil end (tk : rest) | tk == end = ([], rest)
collectUntil end (tk : rest) =
  let (tokens, rest') = collectUntil end rest
   in (tk : tokens, rest')

collectUntilDelimiter :: Delimiter -> [Token] -> ([Token], [Token])
collectUntilDelimiter del tokens = collect tokens 0
  where
    collect :: [Token] -> Int -> ([Token], [Token])
    collect [] _ = ([], [])
    collect (tk : rest) depth | tk == Token.DelimClose del && depth == 0 = ([], rest)
    collect (tk : rest) depth
      | tk == Token.DelimClose del = collectNext tk rest (depth - 1)
    collect (tk : rest) depth
      | tk == Token.DelimOpen del = collectNext tk rest (depth + 1)
    collect (tk : rest) depth = collectNext tk rest depth

    collectNext :: Token -> [Token] -> Int -> ([Token], [Token])
    collectNext tk rest depth =
      let (tokens', rest') = collect rest depth
       in (tk : tokens', rest')

parseList :: Token -> ([Token] -> a) -> [Token] -> [a]
parseList _ _ [] = []
parseList end parser (tk : rest) | tk == end = parseList end parser rest
parseList end parser tokens =
  let (statement, rest) = collectStatement tokens
   in parser statement : parseList end parser rest

parseStatementList :: [Token] -> [Statement]
parseStatementList = parseList Token.NL parseStatement

collectStatement :: [Token] -> ([Token], [Token])
collectStatement = collectUntil Token.Semicolon

parseStatement :: [Token] -> Statement
parseStatement [] = Statement.Empty
parseStatement (Token.Struct : Token.Id name : tks) = parseStatement (Token.Type (Type.Struct name) : tks)
parseStatement [Token.Type ty, Token.Id name] = Statement.Var ty name Nothing
parseStatement (Token.Type ty : Token.Id name : Token.Op Op.Assign : exprTks) = Statement.Var ty name (Just (parseExpr exprTks))
parseStatement [Token.Type ty, Token.Id name, Token.DelimOpen Delimiter.SqBr, Token.NumLiteral len, Token.DelimClose Delimiter.SqBr] =
  Statement.Var (Type.Array ty (read len)) name Nothing
parseStatement (Token.Type ty : Token.Id name : Token.DelimOpen Delimiter.SqBr : Token.NumLiteral len : Token.DelimClose Delimiter.SqBr : Token.Op Op.Assign : exprTks) =
  Statement.Var (Type.Array ty (read len)) name (Just (parseExpr exprTks))
parseStatement [Token.Return] = Statement.Return Nothing
parseStatement (Token.Return : exprTks) = Statement.Return (Just (parseExpr exprTks))
parseStatement tokens = Statement.Expr (parseExpr tokens)

parseExprList :: [Token] -> [Expr]
parseExprList [] = []
parseExprList (Token.Op Op.Comma : rest) = parseExprList rest
parseExprList tokens = let (expr, rest) = collectUntil (Token.Op Op.Comma) tokens in parseExpr expr : parseExprList rest

parseExpr :: [Token] -> Expr
parseExpr [] = error "Empty expression"
parseExpr (Token.NumLiteral num : rest) = parseExprNext (Expr.NumLiteral num) rest
parseExpr (Token.StrLiteral str : rest) = parseExprNext (Expr.StrLiteral str) rest
parseExpr (Token.Id identifier : rest) = parseExprNext (Expr.Id identifier) rest
parseExpr (Token.Op op : rest) | Op.isUnaryPre op = Expr.UnopPre op (parseExpr rest)
parseExpr (Token.DelimOpen Delimiter.Br : rest) = Expr.ArrayDecl (parseExprList (collectArrayDecl rest))
parseExpr tokens = error ("Invalid expression : " ++ show tokens)

parseExprNext :: Expr -> [Token] -> Expr
parseExprNext expr [] = expr
parseExprNext expr [Token.Op op] | Op.isUnaryPost op = Expr.UnopPost op expr
parseExprNext expr (Token.Op op : rest) | Op.isBinary op = Expr.Binop expr op (parseExpr rest)
parseExprNext expr tokens = error ("Invalid follow expression : " ++ show expr ++ ", " ++ show tokens)

collectArrayDecl :: [Token] -> [Token]
collectArrayDecl tokens = let (tokens', _) = collectUntilDelimiter Delimiter.Br tokens in tokens'

collectDirective :: [Token] -> ([Token], [Token])
collectDirective = collectUntil Token.NL

parseDirective :: [Token] -> Declaration
parseDirective _ = Declaration.Directive

collectFuncDec :: [Token] -> ([Token], [Token])
collectFuncDec = collectUntilDelimiter Delimiter.Br

parseFuncDec :: Type -> Identifier.Id -> [Token] -> Declaration
parseFuncDec ty name body = Declaration.FuncDec ty name (parseStatementList body)

collectStruct :: [Token] -> ([Token], [Token])
collectStruct = collectUntilDelimiter Delimiter.Br

parseStruct :: [Token] -> Declaration
parseStruct _ = Declaration.Type