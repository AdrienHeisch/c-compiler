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
import Type as Ty (Type (..))
import Type qualified (isFloating, isInteger, signed, unsigned)
import Utils (listToMaybeList)

parse :: [Token] -> [Declaration]
parse tokens = declarations (static tokens)

static :: [Token] -> [Token]
static [] = []
static (Tk.Signed : Tk.Type ty : rest) = static (Tk.Type (Type.signed ty) : rest)
static (Tk.Unsigned : Tk.Type ty : rest) = static (Tk.Type (Type.unsigned ty) : rest)
static (Tk.Type ty : Tk.Op Op.MultOrIndir : rest) = static (Tk.Type (Ty.Pointer ty) : rest)
static (Tk.Type ty : name@(Tk.Id _) : Tk.DelimOpen Dl.SqBr : Tk.IntLiteral (Constant len_ty len) : Tk.DelimClose Dl.SqBr : rest)
  | Type.isInteger len_ty =
      static (Tk.Type (Ty.Array ty len) : name : rest)
static (Tk.Type ty : name@(Tk.Id _) : Tk.DelimOpen Dl.SqBr : Tk.DelimClose Dl.SqBr : rest) =
  static (Tk.Type (Ty.ArrayNoHint ty) : name : rest)
static (Tk.Struct : Tk.Id name : rest) = static (Tk.Type (Ty.Struct (Just name)) : rest)
static (Tk.Struct : rest) = static (Tk.Type (Ty.Struct Nothing) : rest)
static (tk : rest) = tk : static rest

declarations :: [Token] -> [Declaration]
declarations [] = []
declarations [Eof] = []
-- parseDeclarations (Tk.Directive : rest) =
--   let (tokens, rest') = collectDirective rest
--    in parseDirective tokens : parseDeclarations rest'
declarations (Tk.Enum : Tk.Id name : Tk.Op Op.TernaryElse : Tk.Type ty : Tk.DelimOpen Dl.Br : rest) = do
  let (en, rest') = enum (Just name) ty rest
  en : declarations rest'
declarations (Tk.Enum : Tk.Op Op.TernaryElse : Tk.Type ty : Tk.DelimOpen Dl.Br : rest) = do
  let (en, rest') = enum Nothing ty rest
  en : declarations rest'
declarations (Tk.Enum : Tk.Id name : Tk.DelimOpen Dl.Br : rest) = do
  let (en, rest') = enum (Just name) Ty.Int rest
  en : declarations rest'
declarations (Tk.Enum : Tk.DelimOpen Dl.Br : rest) = do
  let (en, rest') = enum Nothing Ty.Int rest
  en : declarations rest'
declarations (Tk.Type (Ty.Struct name) : Tk.DelimOpen Dl.Br : rest) =
  let (tokens, rest') = collectStructFields rest
   in case rest' of
        (Tk.Semicolon : rest'') -> Declaration.Struct name (structFields tokens) : declarations rest''
        (tk : rest'') -> Declaration.Invalid ("Expected semicolon, got " ++ show tk) : declarations rest''
        [] -> [Declaration.Invalid "Expected semicolon"]
declarations (Tk.Type ty : Tk.Id name : Tk.DelimOpen Dl.Pr : rest) =
  case collectParameters rest of
    (params, Tk.Semicolon : rest') -> funcDef ty name params : declarations rest'
    (params, Tk.DelimOpen Dl.Br : rest') ->
      let (body, rest'') = collectFuncBody rest'
       in funcDec ty name params body : declarations rest''
    (_, rest') -> Declaration.Invalid "Expected semicolon" : declarations rest'
declarations (Tk.NL : rest) = declarations rest
declarations (tk : rest) = Declaration.Invalid ("Unexpected token " ++ show tk) : declarations rest

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
  let (st, rest) = parser tokens
   in st : parseList end parser rest

statementList :: [Token] -> [Statement]
statementList = parseList Tk.NL statement

statement :: [Token] -> (Statement, [Token])
statement [] = (St.Empty, [])
statement (Tk.Semicolon : rest) = (St.Empty, rest)
statement (Tk.If : Tk.DelimOpen Dl.Pr : rest) = do
  let (cond, rest') = collectUntilDelimiter Dl.Pr rest
  let (then_, rest'') = statement rest'
  case rest'' of
    (Tk.Else : rest''') ->
      let (else_, rest'''') = statement rest'''
       in (St.If (expr cond) then_ (Just else_), rest'''')
    _ -> (St.If (expr cond) then_ Nothing, rest'')
statement (Tk.Switch : Tk.DelimOpen Dl.Pr : rest) = do
  let (eval, rest') = collectUntilDelimiter Dl.Pr rest
  let (body, rest'') = statement rest'
   in (St.Switch (expr eval) body, rest'')
statement (Tk.While : Tk.DelimOpen Dl.Pr : rest) = do
  let (cond, rest') = collectUntilDelimiter Dl.Pr rest
  let (body, rest'') = statement rest'
   in (St.While (expr cond) body, rest'')
statement (Tk.Do : rest) = do
  let (body, rest') = statement rest
  case rest' of
    (Tk.While : Tk.DelimOpen Dl.Pr : rest'') -> do
      let (cond, rest''') = collectUntilDelimiter Dl.Pr rest''
      case rest''' of
        (Tk.Semicolon : rest'''') -> (St.DoWhile body $ expr cond, rest'''')
        _ -> (St.Invalid "Expected semicolon", rest')
    _ -> (St.Invalid "Expected while (", rest')
statement (Tk.For : Tk.DelimOpen Dl.Pr : rest) = do
  let (decl, rest') = collectUntil Semicolon rest
  let (cond, rest'') = collectUntil Semicolon rest'
  let (incr, rest''') = collectUntilDelimiter Dl.Pr rest''
  let (body, rest'''') = statement rest'''
  case decl of
    (Tk.Type ty : Tk.Id name : assign) ->
      (St.ForVar (varStatement ty name assign) (expr <$> listToMaybeList cond) (expr <$> listToMaybeList incr) body, rest'''')
    _ -> (St.For (expr <$> listToMaybeList decl) (expr <$> listToMaybeList cond) (expr <$> listToMaybeList incr) body, rest'''')
statement (Tk.DelimOpen Dl.Br : rest) =
  let (tokens, rest') = collectUntilDelimiter Dl.Br rest
   in (St.Block (statementList tokens), rest')
statement (Tk.Return : rest) =
  case collectUntil Tk.Semicolon rest of
    ([], rest') -> (St.Return Nothing, rest')
    (tokens, rest') -> (St.Return (Just (expr tokens)), rest')
statement (Tk.Goto : Tk.Id label : Tk.Semicolon : rest) = (St.Goto label, rest)
statement (Tk.Break : Tk.Semicolon : rest) = (St.Break, rest)
statement (Tk.Continue : Tk.Semicolon : rest) = (St.Continue, rest)
statement (Tk.Case : Tk.IntLiteral constant@(Constant ty _) : Tk.Op Op.TernaryElse : rest)
  | Type.isInteger ty = (St.Case constant, rest) -- TODO enum in case
statement (Tk.Id label : Tk.Op Op.TernaryElse : rest) =
  case rest of
    (Tk.NL : Tk.Id _ : Tk.Op Op.TernaryElse : _) -> (St.Labeled label St.Empty, rest)
    (Tk.Id _ : Tk.Op Op.TernaryElse : _) -> (St.Labeled label St.Empty, rest)
    _ ->
      let (st, rest') = statement rest
       in (St.Labeled label st, rest')
statement (Tk.Type ty : Tk.Id name : tokens) = simpleStatement (varStatement ty name) tokens
statement tokens = simpleStatement (St.Expr . expr) tokens

simpleStatement :: ([Token] -> Statement) -> [Token] -> (Statement, [Token])
simpleStatement parser tokens =
  let (tokens', rest') = collectUntil Tk.Semicolon tokens
   in (parser tokens', rest')

varStatement :: Type -> Id -> [Token] -> Statement
varStatement ty name [] = St.Var ty name Nothing
varStatement ty name (Tk.Op Op.Assign : tokens) = St.Var ty name (Just (expr tokens))
varStatement _ _ tokens = St.Invalid ("Invalid assignment : " ++ show tokens)

exprList :: [Token] -> [Expr]
exprList [] = []
exprList (Tk.Op Op.Comma : rest) = exprList rest
exprList tokens = let (ex, rest) = collectUntil (Tk.Op Op.Comma) tokens in expr ex : exprList rest

expr :: [Token] -> Expr
expr [] = Ex.Invalid "Empty expression"
expr (Tk.NL : rest) = expr rest
expr (Tk.BoolLiteral constant@(Constant Ty.Bool _) : rest) = exprNext (Ex.BoolLiteral constant) rest
expr (Tk.IntLiteral constant : rest) | Type.isInteger $ Constant.ty constant = exprNext (Ex.IntLiteral constant) rest
expr (Tk.FltLiteral constant : rest) | Type.isFloating $ Constant.ty constant = exprNext (Ex.FltLiteral constant) rest
expr (Tk.StrLiteral constant@(Constant (Ty.Array Ty.Char _) _) : rest) = exprNext (Ex.StrLiteral constant) rest
expr (Tk.Id identifier : rest) = exprNext (Ex.Id identifier) rest
expr (Tk.Op op : rest) | Op.isUnaryPre op = Ex.UnopPre op (expr rest)
expr (Tk.DelimOpen Dl.Br : rest) = Ex.ArrayDecl (exprList (collectArrayDecl rest))
expr (Tk.DelimOpen Dl.Pr : rest) = Ex.Parenthese (expr (collectParenthese rest))
expr tokens = Ex.Invalid ("Invalid expression : " ++ show tokens)

exprNext :: Expr -> [Token] -> Expr
exprNext ex [] = ex
exprNext ex [Tk.Op op] | Op.isUnaryPost op = Ex.UnopPost op ex
exprNext ex (Tk.Op op : rest) | Op.isBinary op = binop ex op (expr rest)
exprNext ex (Tk.DelimOpen Dl.SqBr : rest) = binop ex Op.Subscript (expr (collectIndex rest))
exprNext ex (Tk.DelimOpen Dl.Pr : rest) = Ex.Call ex (exprList (collectArguments rest))
exprNext ex tokens = Ex.Invalid ("Invalid follow expression : " ++ show ex ++ ", " ++ show tokens)

collectArrayDecl :: [Token] -> [Token]
collectArrayDecl tokens = let (tokens', _) = collectUntilDelimiter Dl.Br tokens in tokens'

collectParenthese :: [Token] -> [Token]
collectParenthese tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectArguments :: [Token] -> [Token]
collectArguments tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectIndex :: [Token] -> [Token]
collectIndex tokens = let (tokens', _) = collectUntilDelimiter Dl.SqBr tokens in tokens'

binop :: Expr -> Op -> Expr -> Expr
binop left op Ex.Binop {left = r_left, op = r_op, right = r_right}
  | precedence op <= precedence r_op =
      Ex.Binop (binop left op r_left) r_op r_right
binop left op right = Ex.Binop left op right

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

funcDef :: Type -> Id -> [(Type, Id)] -> Declaration
funcDef = Declaration.FuncDef

collectFuncBody :: [Token] -> ([Token], [Token])
collectFuncBody = collectUntilDelimiter Dl.Br

funcDec :: Type -> Id -> [(Type, Id)] -> [Token] -> Declaration
funcDec ty name params body = Declaration.FuncDec ty name params (statementList body)

collectStructFields :: [Token] -> ([Token], [Token])
collectStructFields = collectUntilDelimiter Dl.Br

structFields :: [Token] -> [(Type, Id)]
structFields [] = []
structFields (Tk.NL : rest) = structFields rest
structFields tokens = do
  let (field, rest) = collectUntil Tk.Semicolon tokens
  case field of
    [Tk.Type ty, Tk.Id name] -> (ty, name) : structFields rest
    (tk : _) -> error $ "Unexpected token : " ++ show tk
    [] -> error "Empty field"

enum :: Maybe Id -> Type -> [Token] -> (Declaration, [Token])
enum name ty tokens = do
  let (tokens', rest') = collectEnumVariants tokens
  case rest' of
    (Tk.Semicolon : rest'') -> (Declaration.Enum name ty (enumVariants $ filterNL tokens'), rest'')
    (tk : rest'') -> (Declaration.Invalid ("Expected semicolon, got " ++ show tk), rest'')
    [] -> (Declaration.Invalid "Expected semicolon", rest')

collectEnumVariants :: [Token] -> ([Token], [Token])
collectEnumVariants = collectUntilDelimiter Dl.Br

enumVariants :: [Token] -> [(Id, Maybe Expr)]
enumVariants [] = []
enumVariants (Tk.NL : rest) = enumVariants rest
enumVariants tokens = do
  let (field, rest) = collectUntil (Tk.Op Op.Comma) tokens
  case field of
    (Tk.Id name : Tk.Op Op.Assign : rest') -> (name, Just $ expr rest') : enumVariants rest
    [Tk.Id name] -> (name, Nothing) : enumVariants rest
    (tk : _) -> error $ "Unexpected token : " ++ show tk
    [] -> error "Empty field"