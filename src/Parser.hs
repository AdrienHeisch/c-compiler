module Parser (parse) where

import Constant (Constant (..), IntRepr)
import Declaration (Declaration (..))
import Expr (Expr (..))
import Identifier (Id)
import Op (Op (..), isBinary, isUnaryPost, isUnaryPre, precedence)
import Statement (Statement (..))
import Token (Token (..), filterNL)
import Token as Delimiter (Delimiter (..))
import Type (Type, isFloating, isInteger, signed, unsigned)
import Type qualified (Type (..))
import Utils (listToMaybeList)
import Debug.Trace (trace)

parse :: [Token] -> [Declaration]
parse tokens = parseDeclarations (staticParse tokens)

staticParse :: [Token] -> [Token]
staticParse [] = []
staticParse (Token.Signed : Token.Type ty : rest) = staticParse (Token.Type (signed ty) : rest)
staticParse (Token.Unsigned : Token.Type ty : rest) = staticParse (Token.Type (unsigned ty) : rest)
staticParse (Token.Type ty : Token.Op Op.MultOrIndir : rest) = staticParse (Token.Type (Type.Pointer ty) : rest)
staticParse (Token.Type ty : name@(Token.Id _) : Token.DelimOpen Delimiter.SqBr : Token.IntLiteral (Constant len_ty len) : Token.DelimClose Delimiter.SqBr : rest)
  | isInteger len_ty =
      -- TODO any integer type as length
      staticParse (Token.Type (Type.Array ty len) : name : rest)
staticParse (Token.Type ty : name@(Token.Id _) : Token.DelimOpen Delimiter.SqBr : Token.DelimClose Delimiter.SqBr : rest) =
  staticParse (Token.Type (Type.ArrayNoHint ty) : name : rest)
staticParse (Token.Struct : Token.Id name : rest) = staticParse (Token.Type (Type.Struct (Just name)) : rest)
staticParse (Token.Struct : rest) = staticParse (Token.Type (Type.Struct Nothing) : rest)
staticParse (tk : rest) = tk : staticParse rest

parseDeclarations :: [Token] -> [Declaration]
parseDeclarations [] = []
parseDeclarations [Eof] = []
-- parseDeclarations (Token.Directive : rest) =
--   let (tokens, rest') = collectDirective rest
--    in parseDirective tokens : parseDeclarations rest'
parseDeclarations (Token.Enum : Token.Id name : Token.Op Op.TernaryElse : Token.Type ty : Token.DelimOpen Delimiter.Br : rest) = do
  let (enum, rest') = parseEnum (Just name) ty rest
  enum : parseDeclarations rest'
parseDeclarations (Token.Enum : Token.Op Op.TernaryElse : Token.Type ty : Token.DelimOpen Delimiter.Br : rest) = do
  let (enum, rest') = parseEnum Nothing ty rest
  enum : parseDeclarations rest'
parseDeclarations (Token.Enum : Token.Id name : Token.DelimOpen Delimiter.Br : rest) = do
  let (enum, rest') = parseEnum (Just name) Type.Int rest
  enum : parseDeclarations rest'
parseDeclarations (Token.Enum : Token.DelimOpen Delimiter.Br : rest) = do
  let (enum, rest') = parseEnum Nothing Type.Int rest
  enum : parseDeclarations rest'
parseDeclarations (Token.Type (Type.Struct name) : Token.DelimOpen Delimiter.Br : rest) =
  let (tokens, rest') = collectStructFields rest
   in case rest' of
        (Token.Semicolon : rest'') -> Declaration.Struct name (parseStructFields tokens) : parseDeclarations rest''
        (tk : rest'') -> Declaration.Invalid ("Expected semicolon, got " ++ show tk) : parseDeclarations rest''
        [] -> [Declaration.Invalid "Expected semicolon"]
parseDeclarations (Token.Type ty : Token.Id name : Token.DelimOpen Delimiter.Pr : rest) =
  case collectParameters rest of
    (params, Token.Semicolon : rest') -> parseFuncDef ty name params : parseDeclarations rest'
    (params, Token.DelimOpen Delimiter.Br : rest') ->
      let (body, rest'') = collectFuncBody rest'
       in parseFuncDec ty name params body : parseDeclarations rest''
    (_, rest') -> Declaration.Invalid "Expected semicolon" : parseDeclarations rest'
parseDeclarations (Token.NL : rest) = parseDeclarations rest
parseDeclarations (tk : rest) = Declaration.Invalid ("Unexpected token " ++ show tk) : parseDeclarations rest

-- collectDirective :: [Token] -> ([Token], [Token])
-- collectDirective = collectUntil Token.NL

-- parseDirective :: [Token] -> Declaration
-- parseDirective _ = Declaration.Directive

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
parseStatement (Token.Semicolon : rest) = (Statement.Empty, rest)
parseStatement (Token.If : Token.DelimOpen Delimiter.Pr : rest) =
  let (cond, rest') = collectUntilDelimiter Delimiter.Pr rest
   in let (then_, rest'') = parseStatement rest'
       in case rest'' of
            (Token.Else : rest''') ->
              let (else_, rest'''') = parseStatement rest'''
               in (Statement.If (parseExpr cond) then_ (Just else_), rest'''')
            _ -> (Statement.If (parseExpr cond) then_ Nothing, rest'')
parseStatement (Token.Switch : Token.DelimOpen Delimiter.Pr : rest) =
  let (eval, rest') = collectUntilDelimiter Delimiter.Pr rest
   in let (body, rest'') = parseStatement rest'
       in (Statement.Switch (parseExpr eval) body, rest'')
parseStatement (Token.While : Token.DelimOpen Delimiter.Pr : rest) =
  let (cond, rest') = collectUntilDelimiter Delimiter.Pr rest
   in let (body, rest'') = parseStatement rest'
       in (Statement.While (parseExpr cond) body, rest'')
parseStatement (Token.Do : rest) =
  let (body, rest') = parseStatement rest
   in case rest' of
        (Token.While : Token.DelimOpen Delimiter.Pr : rest'') ->
          let (cond, rest''') = collectUntilDelimiter Delimiter.Pr rest''
           in case rest''' of
                (Token.Semicolon : rest'''') -> (Statement.DoWhile body $ parseExpr cond, rest'''')
                _ -> (Statement.Invalid "Expected semicolon", rest')
        _ -> (Statement.Invalid "Expected while (", rest')
parseStatement (Token.For : Token.DelimOpen Delimiter.Pr : rest) =
  let (decl, rest') = collectUntil Semicolon rest
   in let (cond, rest'') = collectUntil Semicolon rest'
       in let (incr, rest''') = collectUntilDelimiter Delimiter.Pr rest''
           in let (body, rest'''') = parseStatement rest'''
               in case decl of
                    (Token.Type ty : Token.Id name : assign) -> (Statement.ForVar (parseVarStatement ty name assign) (parseExpr <$> listToMaybeList cond) (parseExpr <$> listToMaybeList incr) body, rest'''')
                    _ -> (Statement.For (parseExpr <$> listToMaybeList decl) (parseExpr <$> listToMaybeList cond) (parseExpr <$> listToMaybeList incr) body, rest'''')
parseStatement (Token.DelimOpen Delimiter.Br : rest) =
  let (tokens, rest') = collectUntilDelimiter Delimiter.Br rest
   in (Statement.Block (parseStatementList tokens), rest')
parseStatement (Token.Return : rest) =
  case collectUntil Token.Semicolon rest of
    ([], rest') -> (Statement.Return Nothing, rest')
    (tokens, rest') -> (Statement.Return (Just (parseExpr tokens)), rest')
parseStatement (Token.Goto : Token.Id label : Token.Semicolon : rest) = (Statement.Goto label, rest)
parseStatement (Token.Break : Token.Semicolon : rest) = (Statement.Break, rest)
parseStatement (Token.Continue : Token.Semicolon : rest) = (Statement.Continue, rest)
parseStatement (Token.Case : Token.IntLiteral constant@(Constant Type.Int _) : Token.Op Op.TernaryElse : rest) = (Statement.Case constant, rest) -- TODO any integer type for case
parseStatement (Token.Id label : Token.Op Op.TernaryElse : rest) =
  case rest of
    (Token.NL : Token.Id _ : Token.Op Op.TernaryElse : _) -> (Statement.Labeled label Statement.Empty, rest)
    (Token.Id _ : Token.Op Op.TernaryElse : _) -> (Statement.Labeled label Statement.Empty, rest)
    _ ->
      let (statement, rest') = parseStatement rest
       in (Statement.Labeled label statement, rest')
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
parseExpr (Token.NL : rest) = parseExpr rest
parseExpr (Token.BoolLiteral constant@(Constant Type.Bool _) : rest) = parseExprNext (Expr.BoolLiteral constant) rest
parseExpr (Token.IntLiteral constant : rest) | Type.isInteger $ Constant.ty constant = parseExprNext (Expr.IntLiteral constant) rest
parseExpr (Token.FltLiteral constant : rest) | Type.isFloating $ Constant.ty constant = parseExprNext (Expr.FltLiteral constant) rest
parseExpr (Token.StrLiteral constant@(Constant (Type.Array Type.Char _) _) : rest) = parseExprNext (Expr.StrLiteral constant) rest
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

makeBinop :: Expr -> Op -> Expr -> Expr
makeBinop left op Expr.Binop {left = r_left, op = r_op, right = r_right}
  | precedence op <= precedence r_op =
      Expr.Binop (makeBinop left op r_left) r_op r_right
makeBinop left op right = Expr.Binop left op right

collectParameters :: [Token] -> ([(Type, Id)], [Token])
collectParameters [] = ([], [])
collectParameters tokens =
  let (tokens', rest) = collectUntilDelimiter Delimiter.Pr tokens
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

collectStructFields :: [Token] -> ([Token], [Token])
collectStructFields = collectUntilDelimiter Delimiter.Br

parseStructFields :: [Token] -> [(Type, Id)]
parseStructFields [] = []
parseStructFields (Token.NL : rest) = parseStructFields rest
parseStructFields tokens = do
  let (field, rest) = collectUntil Token.Semicolon tokens
  case field of
    [Token.Type ty, Token.Id name] -> (ty, name) : parseStructFields rest
    (tk : _) -> error $ "Unexpected token : " ++ show tk
    [] -> error "Empty field"

parseEnum :: Maybe Id -> Type -> [Token] -> (Declaration, [Token])
parseEnum name ty tokens =
  let (tokens', rest') = collectEnumVariants tokens
   in case rest' of
        (Token.Semicolon : rest'') -> (Declaration.Enum name ty (parseEnumVariants $ filterNL tokens'), rest'')
        (tk : rest'') -> (Declaration.Invalid ("Expected semicolon, got " ++ show tk), rest'')
        [] -> (Declaration.Invalid "Expected semicolon", rest')

collectEnumVariants :: [Token] -> ([Token], [Token])
collectEnumVariants = collectUntilDelimiter Delimiter.Br

parseEnumVariants :: [Token] -> [(Id, Maybe Expr)]
parseEnumVariants [] = []
parseEnumVariants (Token.NL : rest) = parseEnumVariants rest
parseEnumVariants tokens = do
  let (field, rest) = collectUntil (Token.Op Op.Comma) tokens
  case field of
    (Token.Id name : Token.Op Op.Assign : expr) -> (name, Just $ parseExpr expr) : parseEnumVariants rest
    [Token.Id name] -> (name, Nothing) : parseEnumVariants rest
    (tk : _) -> error $ "Unexpected token : " ++ show tk
    [] -> error "Empty field"