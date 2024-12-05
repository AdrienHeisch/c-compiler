module Parser (parse) where

import Debug.Trace (trace)
import Declaration (Declaration (..))
import Expr (Expr (..))
import Identifier (Id)
import Op (Op (..), isBinary, isUnaryPost, isUnaryPre, precedence)
import Statement (Statement (..))
import Token (Token (..))
import Token as Delimiter (Delimiter (..))
import Type (Type (..))

parse :: [Token] -> [Declaration]
parse tokens = parseDeclarations (staticParse tokens)

staticParse :: [Token] -> [Token]
staticParse [] = []
staticParse [tk] = [tk]
staticParse (Token.Type ty : Token.Op Op.MultOrIndir : rest) = staticParse (Token.Type (Type.Pointer ty) : rest)
staticParse (Token.Type ty : name@(Token.Id _) : Token.DelimOpen Delimiter.SqBr : Token.NumLiteral len : Token.DelimClose Delimiter.SqBr : rest) =
  staticParse (Token.Type (Type.Array ty (read len)) : name : rest)
staticParse (Token.Type ty : name@(Token.Id _) : Token.DelimOpen Delimiter.SqBr : Token.DelimClose Delimiter.SqBr : rest) =
  staticParse (Token.Type (Type.ArrayNoHint ty) : name : rest)
staticParse (Token.Struct : Token.Id name : rest) = staticParse (Token.Type (Type.Struct (Just name)) : rest)
staticParse (Token.Struct : rest) = staticParse (Token.Type (Type.Struct Nothing) : rest)
staticParse (tk : rest) = tk : staticParse rest

parseDeclarations :: [Token] -> [Declaration]
parseDeclarations [] = []
parseDeclarations [Eof] = []
parseDeclarations (Token.Directive : rest) =
  let (tokens, rest') = collectDirective rest
   in parseDirective tokens : parseDeclarations rest'
-- TODO struct name
parseDeclarations (Token.Type (Type.Struct _) : Token.DelimOpen Delimiter.Br : rest) =
  let (tokens, rest') = collectStruct rest
   in parseStruct tokens : parseDeclarations rest'
parseDeclarations (Token.Type ty : Token.Id name : Token.DelimOpen Delimiter.Pr : rest) =
  case collectParameters rest of
    (params, Token.Semicolon : rest') -> parseFuncDef ty name params : parseDeclarations rest'
    (params, Token.DelimOpen Delimiter.Br : rest') ->
      let (body, rest'') = collectFuncBody rest'
       in parseFuncDec ty name params body : parseDeclarations rest''
    (_, rest') -> Declaration.Invalid "Expected semicolon" : parseDeclarations rest'
parseDeclarations (Token.NL : rest) = parseDeclarations rest
parseDeclarations (tk : _) = [Declaration.Invalid ("Unexpected token " ++ show tk)]

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

parseList :: Token -> ([Token] -> (a, [Token])) -> [Token] -> [a]
parseList _ _ [] = []
parseList end parser (tk : rest) | tk == end = parseList end parser rest
parseList end parser tokens =
  let (statement, rest) = parser tokens
   in statement : parseList end parser rest

parseStatementList :: [Token] -> [Statement]
parseStatementList = parseList Token.NL parseStatement

parseStatement :: [Token] -> (Statement, [Token])
parseStatement [] = (Statement.Empty, [])
parseStatement (Token.If : Token.DelimOpen Delimiter.Pr : rest) =
  let (condition, rest') = collectUntilDelimiter Delimiter.Pr rest
   in let (then_, rest'') = parseStatement rest'
       in case rest'' of
        (Token.Else : rest''') ->
          let (else_, rest'''') = parseStatement rest'''
           in (Statement.If (parseExpr condition) then_ (Just else_), rest'''')
        _ -> (Statement.If (parseExpr condition) then_ Nothing, rest'')
parseStatement (Token.DelimOpen Delimiter.Br : rest) =
  let (tokens, rest') = collectUntilDelimiter Delimiter.Br rest
   in (Statement.Block (parseStatementList tokens), rest')
parseStatement (Token.Return : rest) =
  case collectUntil Token.Semicolon rest of
    ([], rest') -> (Statement.Return Nothing, rest')
    (tokens, rest') -> (Statement.Return (Just (parseExpr tokens)), rest')
parseStatement (Token.Type ty : Token.Id name : tokens) = simpleStatement (parseVarStatement ty name) tokens
parseStatement tokens = simpleStatement (Statement.Expr . parseExpr) tokens

simpleStatement :: ([Token] -> Statement) -> [Token] -> (Statement, [Token])
simpleStatement parser tokens =
  let (tokens', rest') = collectUntil Token.Semicolon tokens
   in (parser tokens', rest')

parseVarStatement :: Type -> Id -> [Token] -> Statement
parseVarStatement ty name [] = Statement.Var ty name Nothing
parseVarStatement ty name (Token.Op Op.Assign : tokens) = Statement.Var ty name (Just (parseExpr tokens))
parseVarStatement _ _ tokens = Statement.Invalid ("Invalid assignment : " ++ show tokens)

parseExprList :: [Token] -> [Expr]
parseExprList [] = []
parseExprList (Token.Op Op.Comma : rest) = parseExprList rest
parseExprList tokens = let (expr, rest) = collectUntil (Token.Op Op.Comma) tokens in parseExpr expr : parseExprList rest

parseExpr :: [Token] -> Expr
parseExpr [] = Expr.Invalid "Empty expression"
parseExpr (Token.NumLiteral num : rest) = parseExprNext (Expr.NumLiteral num) rest
parseExpr (Token.StrLiteral str : rest) = parseExprNext (Expr.StrLiteral str) rest
parseExpr (Token.Id identifier : rest) = parseExprNext (Expr.Id identifier) rest
parseExpr (Token.Op op : rest) | Op.isUnaryPre op = Expr.UnopPre op (parseExpr rest)
parseExpr (Token.DelimOpen Delimiter.Br : rest) = Expr.ArrayDecl (parseExprList (collectArrayDecl rest))
parseExpr (Token.DelimOpen Delimiter.Pr : rest) = Expr.Parenthese (parseExpr (collectParenthese rest))
parseExpr tokens = Expr.Invalid ("Invalid expression : " ++ show tokens)

parseExprNext :: Expr -> [Token] -> Expr
parseExprNext expr [] = expr
parseExprNext expr [Token.Op op] | Op.isUnaryPost op = Expr.UnopPost op expr
parseExprNext expr (Token.Op op : rest) | Op.isBinary op = makeBinop expr op (parseExpr rest)
parseExprNext expr (Token.DelimOpen Delimiter.SqBr : rest) = makeBinop expr Op.Subscript (parseExpr (collectIndex rest))
parseExprNext expr (Token.DelimOpen Delimiter.Pr : rest) = Expr.Call expr (parseExprList (collectArguments rest))
parseExprNext expr tokens = Expr.Invalid ("Invalid follow expression : " ++ show expr ++ ", " ++ show tokens)

collectArrayDecl :: [Token] -> [Token]
collectArrayDecl tokens = let (tokens', _) = collectUntilDelimiter Delimiter.Br tokens in tokens'

collectParenthese :: [Token] -> [Token]
collectParenthese tokens = let (tokens', _) = collectUntilDelimiter Delimiter.Pr tokens in tokens'

collectArguments :: [Token] -> [Token]
collectArguments tokens = let (tokens', _) = collectUntilDelimiter Delimiter.Pr tokens in tokens'

collectIndex :: [Token] -> [Token]
collectIndex tokens = let (tokens', _) = collectUntilDelimiter Delimiter.SqBr tokens in tokens'

collectDirective :: [Token] -> ([Token], [Token])
collectDirective = collectUntil Token.NL

makeBinop :: Expr -> Op -> Expr -> Expr
makeBinop left op Expr.Binop {left = r_left, op = r_op, right = r_right}
  | precedence op <= precedence r_op =
      Expr.Binop (makeBinop left op r_left) r_op r_right
makeBinop left op right = Expr.Binop left op right

parseDirective :: [Token] -> Declaration
parseDirective _ = Declaration.Directive

collectParameters :: [Token] -> ([(Type, Id)], [Token])
collectParameters [] = ([], [])
collectParameters tokens =
  let (tokens', rest) = collectUntil (Token.DelimClose Delimiter.Pr) tokens
   in (makeList tokens', rest)
  where
    makeList :: [Token] -> [(Type, Id)]
    makeList [] = []
    makeList (Token.Type ty : Token.Id name : Token.Op Op.Comma : rest) = (ty, name) : makeList rest
    makeList (Token.Type ty : Token.Id name : _) = [(ty, name)]
    makeList tokens'' = error ("Invalid parameters : " ++ show tokens'')

parseFuncDef :: Type -> Id -> [(Type, Id)] -> Declaration
parseFuncDef = Declaration.FuncDef

collectFuncBody :: [Token] -> ([Token], [Token])
collectFuncBody = collectUntilDelimiter Delimiter.Br

parseFuncDec :: Type -> Id -> [(Type, Id)] -> [Token] -> Declaration
parseFuncDec ty name params body = Declaration.FuncDec ty name params (parseStatementList body)

collectStruct :: [Token] -> ([Token], [Token])
collectStruct = collectUntilDelimiter Delimiter.Br

parseStruct :: [Token] -> Declaration
parseStruct _ = Declaration.Invalid "Structs not implemented"