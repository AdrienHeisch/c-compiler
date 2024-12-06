module Parser (parse) where

import Constant (Constant (..))
import Declaration (Declaration (..))
import Expr as Ex (Expr (..))
import Identifier (Id)
import Op (Op (..), isBinary, isUnaryPost, isUnaryPre, precedence)
import Statement as St (Statement (..))
import Token (filterNL)
import Token as Dl (Delimiter (..))
import Token as Tk (Token (..))
import Type qualified (isFloating, isInteger, signed, unsigned)
import Type as Ty (Type (..))
import Utils (listToMaybeList)

parse :: [Token] -> [Declaration]
parse tokens = parseDeclarations (staticParse tokens)

staticParse :: [Token] -> [Token]
staticParse [] = []
staticParse (Tk.Signed : Tk.Type ty : rest) = staticParse (Tk.Type (Type.signed ty) : rest)
staticParse (Tk.Unsigned : Tk.Type ty : rest) = staticParse (Tk.Type (Type.unsigned ty) : rest)
staticParse (Tk.Type ty : Tk.Op Op.MultOrIndir : rest) = staticParse (Tk.Type (Ty.Pointer ty) : rest)
staticParse (Tk.Type ty : name@(Tk.Id _) : Tk.DelimOpen Dl.SqBr : Tk.IntLiteral (Constant len_ty len) : Tk.DelimClose Dl.SqBr : rest)
  | Type.isInteger len_ty =
      staticParse (Tk.Type (Ty.Array ty len) : name : rest)
staticParse (Tk.Type ty : name@(Tk.Id _) : Tk.DelimOpen Dl.SqBr : Tk.DelimClose Dl.SqBr : rest) =
  staticParse (Tk.Type (Ty.ArrayNoHint ty) : name : rest)
staticParse (Tk.Struct : Tk.Id name : rest) = staticParse (Tk.Type (Ty.Struct (Just name)) : rest)
staticParse (Tk.Struct : rest) = staticParse (Tk.Type (Ty.Struct Nothing) : rest)
staticParse (tk : rest) = tk : staticParse rest

parseDeclarations :: [Token] -> [Declaration]
parseDeclarations [] = []
parseDeclarations [Eof] = []
-- parseDeclarations (Tk.Directive : rest) =
--   let (tokens, rest') = collectDirective rest
--    in parseDirective tokens : parseDeclarations rest'
parseDeclarations (Tk.Enum : Tk.Id name : Tk.Op Op.TernaryElse : Tk.Type ty : Tk.DelimOpen Dl.Br : rest) = do
  let (enum, rest') = parseEnum (Just name) ty rest
  enum : parseDeclarations rest'
parseDeclarations (Tk.Enum : Tk.Op Op.TernaryElse : Tk.Type ty : Tk.DelimOpen Dl.Br : rest) = do
  let (enum, rest') = parseEnum Nothing ty rest
  enum : parseDeclarations rest'
parseDeclarations (Tk.Enum : Tk.Id name : Tk.DelimOpen Dl.Br : rest) = do
  let (enum, rest') = parseEnum (Just name) Ty.Int rest
  enum : parseDeclarations rest'
parseDeclarations (Tk.Enum : Tk.DelimOpen Dl.Br : rest) = do
  let (enum, rest') = parseEnum Nothing Ty.Int rest
  enum : parseDeclarations rest'
parseDeclarations (Tk.Type (Ty.Struct name) : Tk.DelimOpen Dl.Br : rest) =
  let (tokens, rest') = collectStructFields rest
   in case rest' of
        (Tk.Semicolon : rest'') -> Declaration.Struct name (parseStructFields tokens) : parseDeclarations rest''
        (tk : rest'') -> Declaration.Invalid ("Expected semicolon, got " ++ show tk) : parseDeclarations rest''
        [] -> [Declaration.Invalid "Expected semicolon"]
parseDeclarations (Tk.Type ty : Tk.Id name : Tk.DelimOpen Dl.Pr : rest) =
  case collectParameters rest of
    (params, Tk.Semicolon : rest') -> parseFuncDef ty name params : parseDeclarations rest'
    (params, Tk.DelimOpen Dl.Br : rest') ->
      let (body, rest'') = collectFuncBody rest'
       in parseFuncDec ty name params body : parseDeclarations rest''
    (_, rest') -> Declaration.Invalid "Expected semicolon" : parseDeclarations rest'
parseDeclarations (Tk.NL : rest) = parseDeclarations rest
parseDeclarations (tk : rest) = Declaration.Invalid ("Unexpected token " ++ show tk) : parseDeclarations rest

-- collectDirective :: [Token] -> ([Token], [Token])
-- collectDirective = collectUntil Tk.NL

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
    collect (tk : rest) depth | tk == Tk.DelimClose del && depth == 0 = ([], rest)
    collect (tk : rest) depth
      | tk == Tk.DelimClose del = collectNext tk rest (depth - 1)
    collect (tk : rest) depth
      | tk == Tk.DelimOpen del = collectNext tk rest (depth + 1)
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
parseStatementList = parseList Tk.NL parseStatement

parseStatement :: [Token] -> (Statement, [Token])
parseStatement [] = (St.Empty, [])
parseStatement (Tk.Semicolon : rest) = (St.Empty, rest)
parseStatement (Tk.If : Tk.DelimOpen Dl.Pr : rest) = do
  let (cond, rest') = collectUntilDelimiter Dl.Pr rest
  let (then_, rest'') = parseStatement rest'
  case rest'' of
    (Tk.Else : rest''') ->
      let (else_, rest'''') = parseStatement rest'''
       in (St.If (parseExpr cond) then_ (Just else_), rest'''')
    _ -> (St.If (parseExpr cond) then_ Nothing, rest'')
parseStatement (Tk.Switch : Tk.DelimOpen Dl.Pr : rest) = do
  let (eval, rest') = collectUntilDelimiter Dl.Pr rest
  let (body, rest'') = parseStatement rest'
   in (St.Switch (parseExpr eval) body, rest'')
parseStatement (Tk.While : Tk.DelimOpen Dl.Pr : rest) = do
  let (cond, rest') = collectUntilDelimiter Dl.Pr rest
  let (body, rest'') = parseStatement rest'
   in (St.While (parseExpr cond) body, rest'')
parseStatement (Tk.Do : rest) = do
  let (body, rest') = parseStatement rest
  case rest' of
    (Tk.While : Tk.DelimOpen Dl.Pr : rest'') -> do
      let (cond, rest''') = collectUntilDelimiter Dl.Pr rest''
      case rest''' of
        (Tk.Semicolon : rest'''') -> (St.DoWhile body $ parseExpr cond, rest'''')
        _ -> (St.Invalid "Expected semicolon", rest')
    _ -> (St.Invalid "Expected while (", rest')
parseStatement (Tk.For : Tk.DelimOpen Dl.Pr : rest) = do
  let (decl, rest') = collectUntil Semicolon rest
  let (cond, rest'') = collectUntil Semicolon rest'
  let (incr, rest''') = collectUntilDelimiter Dl.Pr rest''
  let (body, rest'''') = parseStatement rest'''
  case decl of
    (Tk.Type ty : Tk.Id name : assign) ->
      (St.ForVar (parseVarStatement ty name assign) (parseExpr <$> listToMaybeList cond) (parseExpr <$> listToMaybeList incr) body, rest'''')
    _ -> (St.For (parseExpr <$> listToMaybeList decl) (parseExpr <$> listToMaybeList cond) (parseExpr <$> listToMaybeList incr) body, rest'''')
parseStatement (Tk.DelimOpen Dl.Br : rest) =
  let (tokens, rest') = collectUntilDelimiter Dl.Br rest
   in (St.Block (parseStatementList tokens), rest')
parseStatement (Tk.Return : rest) =
  case collectUntil Tk.Semicolon rest of
    ([], rest') -> (St.Return Nothing, rest')
    (tokens, rest') -> (St.Return (Just (parseExpr tokens)), rest')
parseStatement (Tk.Goto : Tk.Id label : Tk.Semicolon : rest) = (St.Goto label, rest)
parseStatement (Tk.Break : Tk.Semicolon : rest) = (St.Break, rest)
parseStatement (Tk.Continue : Tk.Semicolon : rest) = (St.Continue, rest)
parseStatement (Tk.Case : Tk.IntLiteral constant@(Constant Ty.Int _) : Tk.Op Op.TernaryElse : rest) = (St.Case constant, rest) -- TODO any integer type for case
parseStatement (Tk.Id label : Tk.Op Op.TernaryElse : rest) =
  case rest of
    (Tk.NL : Tk.Id _ : Tk.Op Op.TernaryElse : _) -> (St.Labeled label St.Empty, rest)
    (Tk.Id _ : Tk.Op Op.TernaryElse : _) -> (St.Labeled label St.Empty, rest)
    _ ->
      let (statement, rest') = parseStatement rest
       in (St.Labeled label statement, rest')
parseStatement (Tk.Type ty : Tk.Id name : tokens) = simpleStatement (parseVarStatement ty name) tokens
parseStatement tokens = simpleStatement (St.Expr . parseExpr) tokens

simpleStatement :: ([Token] -> Statement) -> [Token] -> (Statement, [Token])
simpleStatement parser tokens =
  let (tokens', rest') = collectUntil Tk.Semicolon tokens
   in (parser tokens', rest')

parseVarStatement :: Type -> Id -> [Token] -> Statement
parseVarStatement ty name [] = St.Var ty name Nothing
parseVarStatement ty name (Tk.Op Op.Assign : tokens) = St.Var ty name (Just (parseExpr tokens))
parseVarStatement _ _ tokens = St.Invalid ("Invalid assignment : " ++ show tokens)

parseExprList :: [Token] -> [Expr]
parseExprList [] = []
parseExprList (Tk.Op Op.Comma : rest) = parseExprList rest
parseExprList tokens = let (expr, rest) = collectUntil (Tk.Op Op.Comma) tokens in parseExpr expr : parseExprList rest

parseExpr :: [Token] -> Expr
parseExpr [] = Ex.Invalid "Empty expression"
parseExpr (Tk.NL : rest) = parseExpr rest
parseExpr (Tk.BoolLiteral constant@(Constant Ty.Bool _) : rest) = parseExprNext (Ex.BoolLiteral constant) rest
parseExpr (Tk.IntLiteral constant : rest) | Type.isInteger $ Constant.ty constant = parseExprNext (Ex.IntLiteral constant) rest
parseExpr (Tk.FltLiteral constant : rest) | Type.isFloating $ Constant.ty constant = parseExprNext (Ex.FltLiteral constant) rest
parseExpr (Tk.StrLiteral constant@(Constant (Ty.Array Ty.Char _) _) : rest) = parseExprNext (Ex.StrLiteral constant) rest
parseExpr (Tk.Id identifier : rest) = parseExprNext (Ex.Id identifier) rest
parseExpr (Tk.Op op : rest) | Op.isUnaryPre op = Ex.UnopPre op (parseExpr rest)
parseExpr (Tk.DelimOpen Dl.Br : rest) = Ex.ArrayDecl (parseExprList (collectArrayDecl rest))
parseExpr (Tk.DelimOpen Dl.Pr : rest) = Ex.Parenthese (parseExpr (collectParenthese rest))
parseExpr tokens = Ex.Invalid ("Invalid expression : " ++ show tokens)

parseExprNext :: Expr -> [Token] -> Expr
parseExprNext expr [] = expr
parseExprNext expr [Tk.Op op] | Op.isUnaryPost op = Ex.UnopPost op expr
parseExprNext expr (Tk.Op op : rest) | Op.isBinary op = makeBinop expr op (parseExpr rest)
parseExprNext expr (Tk.DelimOpen Dl.SqBr : rest) = makeBinop expr Op.Subscript (parseExpr (collectIndex rest))
parseExprNext expr (Tk.DelimOpen Dl.Pr : rest) = Ex.Call expr (parseExprList (collectArguments rest))
parseExprNext expr tokens = Ex.Invalid ("Invalid follow expression : " ++ show expr ++ ", " ++ show tokens)

collectArrayDecl :: [Token] -> [Token]
collectArrayDecl tokens = let (tokens', _) = collectUntilDelimiter Dl.Br tokens in tokens'

collectParenthese :: [Token] -> [Token]
collectParenthese tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectArguments :: [Token] -> [Token]
collectArguments tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectIndex :: [Token] -> [Token]
collectIndex tokens = let (tokens', _) = collectUntilDelimiter Dl.SqBr tokens in tokens'

makeBinop :: Expr -> Op -> Expr -> Expr
makeBinop left op Ex.Binop {left = r_left, op = r_op, right = r_right}
  | precedence op <= precedence r_op =
      Ex.Binop (makeBinop left op r_left) r_op r_right
makeBinop left op right = Ex.Binop left op right

collectParameters :: [Token] -> ([(Type, Id)], [Token])
collectParameters [] = ([], [])
collectParameters tokens =
  let (tokens', rest) = collectUntilDelimiter Dl.Pr tokens
   in (makeList tokens', rest)
  where
    makeList :: [Token] -> [(Type, Id)]
    makeList [] = []
    makeList (Tk.Type ty : Tk.Id name : Tk.Op Op.Comma : rest) = (ty, name) : makeList rest
    makeList (Tk.Type ty : Tk.Id name : _) = [(ty, name)]
    makeList tokens'' = error ("Invalid parameters : " ++ show tokens'')

parseFuncDef :: Type -> Id -> [(Type, Id)] -> Declaration
parseFuncDef = Declaration.FuncDef

collectFuncBody :: [Token] -> ([Token], [Token])
collectFuncBody = collectUntilDelimiter Dl.Br

parseFuncDec :: Type -> Id -> [(Type, Id)] -> [Token] -> Declaration
parseFuncDec ty name params body = Declaration.FuncDec ty name params (parseStatementList body)

collectStructFields :: [Token] -> ([Token], [Token])
collectStructFields = collectUntilDelimiter Dl.Br

parseStructFields :: [Token] -> [(Type, Id)]
parseStructFields [] = []
parseStructFields (Tk.NL : rest) = parseStructFields rest
parseStructFields tokens = do
  let (field, rest) = collectUntil Tk.Semicolon tokens
  case field of
    [Tk.Type ty, Tk.Id name] -> (ty, name) : parseStructFields rest
    (tk : _) -> error $ "Unexpected token : " ++ show tk
    [] -> error "Empty field"

parseEnum :: Maybe Id -> Type -> [Token] -> (Declaration, [Token])
parseEnum name ty tokens = do
  let (tokens', rest') = collectEnumVariants tokens
  case rest' of
    (Tk.Semicolon : rest'') -> (Declaration.Enum name ty (parseEnumVariants $ filterNL tokens'), rest'')
    (tk : rest'') -> (Declaration.Invalid ("Expected semicolon, got " ++ show tk), rest'')
    [] -> (Declaration.Invalid "Expected semicolon", rest')

collectEnumVariants :: [Token] -> ([Token], [Token])
collectEnumVariants = collectUntilDelimiter Dl.Br

parseEnumVariants :: [Token] -> [(Id, Maybe Expr)]
parseEnumVariants [] = []
parseEnumVariants (Tk.NL : rest) = parseEnumVariants rest
parseEnumVariants tokens = do
  let (field, rest) = collectUntil (Tk.Op Op.Comma) tokens
  case field of
    (Tk.Id name : Tk.Op Op.Assign : expr) -> (name, Just $ parseExpr expr) : parseEnumVariants rest
    [Tk.Id name] -> (name, Nothing) : parseEnumVariants rest
    (tk : _) -> error $ "Unexpected token : " ++ show tk
    [] -> error "Empty field"