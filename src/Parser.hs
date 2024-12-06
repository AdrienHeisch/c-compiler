module Parser (parse) where

import Constant (Constant (..))
import Constant qualified (IntRepr)
import Declaration (Declaration (..))
import Expr as Ex (Expr (..))
import Identifier (Id)
import Op (Op (..))
import Op qualified (isBinary, isUnaryPost, isUnaryPre, precedence)
import Statement as St (Statement (..))
import Token as Dl (Delimiter (..))
import Token as Tk (Token (..))
import Token qualified (filterNL)
import Type as Ty (Type (..))
import Type qualified (isFloating, isInteger, signed, unsigned)
import Utils (listToMaybeList)

parse :: [Token] -> [Declaration]
parse tokens = declarations (static tokens)

static :: [Token] -> [Token]
static tokens = case tokens of
  [] -> []
  (Tk.Signed : Tk.Type ty : rest) ->
    static (Tk.Type (Type.signed ty) : rest)
  (Tk.Unsigned : Tk.Type ty : rest) ->
    static (Tk.Type (Type.unsigned ty) : rest)
  (Tk.Type ty : Tk.Op Op.MultOrIndir : rest) ->
    static (Tk.Type (Ty.Pointer ty) : rest)
  (Tk.Type ty : name@(Tk.Id _) : Tk.DelimOpen Dl.SqBr : Tk.IntLiteral (Constant len_ty len) : Tk.DelimClose Dl.SqBr : rest)
    | Type.isInteger len_ty ->
        static (Tk.Type (Ty.Array ty len) : name : rest)
  (Tk.Type ty : name@(Tk.Id _) : Tk.DelimOpen Dl.SqBr : Tk.DelimClose Dl.SqBr : rest) ->
    static (Tk.Type (Ty.ArrayNoHint ty) : name : rest)
  (Tk.Struct : Tk.Id name : rest) ->
    static (Tk.Type (Ty.Struct (Just name)) : rest)
  (Tk.Struct : rest) ->
    static (Tk.Type (Ty.Struct Nothing) : rest)
  (tk : rest) -> tk : static rest

declarations :: [Token] -> [Declaration]
declarations tokens = case tokens of
  [] -> []
  [Eof] -> []
  (Tk.Enum : Tk.Id name : Tk.Op Op.TernaryElse : Tk.Type ty : Tk.DelimOpen Dl.Br : rest) ->
    put $ enum (Just name) ty rest
  (Tk.Enum : Tk.Op Op.TernaryElse : Tk.Type ty : Tk.DelimOpen Dl.Br : rest) ->
    put $ enum Nothing ty rest
  (Tk.Enum : Tk.Id name : Tk.DelimOpen Dl.Br : rest) ->
    put $ enum (Just name) Ty.Int rest
  (Tk.Enum : Tk.DelimOpen Dl.Br : rest) ->
    put $ enum Nothing Ty.Int rest
  (Tk.Type (Ty.Struct name) : Tk.DelimOpen Dl.Br : rest) ->
    put $ struct name rest
  (Tk.Type ty : Tk.Id name : Tk.DelimOpen Dl.Pr : rest) ->
    put $ func ty name rest
  (Tk.NL : rest) -> declarations rest
  (tk : rest) -> Declaration.Invalid ("Unexpected token " ++ show tk) : declarations rest
  where
    put tuple =
      let (declaration, rest) = tuple
       in declaration : declarations rest

-- collectDirective :: [Token] -> ([Token], [Token])
-- collectDirective = collectUntil Tk.NL

-- parseDirective :: [Token] -> Declaration
-- parseDirective _ = Declaration.Directive

collectUntil :: Token -> [Token] -> ([Token], [Token])
collectUntil end tokens = case tokens of
  [] -> ([], [])
  (tk : rest) | tk == end -> ([], rest)
  (tk : rest) ->
    let (tokens', rest') = collectUntil end rest
     in (tk : tokens', rest')

collectUntilDelimiter :: Delimiter -> [Token] -> ([Token], [Token])
collectUntilDelimiter del = collect 0
  where
    collect :: Int -> [Token] -> ([Token], [Token])
    collect depth tokens = case tokens of
      [] -> ([], [])
      (tk : rest) | tk == Tk.DelimClose del && depth == 0 -> ([], rest)
      (tk : rest) | tk == Tk.DelimClose del -> collectNext tk rest (depth - 1)
      (tk : rest) | tk == Tk.DelimOpen del -> collectNext tk rest (depth + 1)
      (tk : rest) -> collectNext tk rest depth
    collectNext :: Token -> [Token] -> Int -> ([Token], [Token])
    collectNext tk rest depth =
      let (tokens', rest') = collect depth rest
       in (tk : tokens', rest')

parseList :: Token -> ([Token] -> (a, [Token])) -> [Token] -> [a]
parseList end parser tokens = case tokens of
  [] -> []
  (tk : rest) | tk == end -> parseList end parser rest
  _ ->
    let (st, rest) = parser tokens
     in st : parseList end parser rest

statementList :: [Token] -> [Statement]
statementList = parseList Tk.NL statement

statement :: [Token] -> (Statement, [Token])
statement tokens = case tokens of
  [] ->
    (St.Empty, [])
  (Tk.Semicolon : rest) ->
    (St.Empty, rest)
  (Tk.If : Tk.DelimOpen Dl.Pr : rest) ->
    if_ rest
  (Tk.Switch : Tk.DelimOpen Dl.Pr : rest) ->
    switch rest
  (Tk.While : Tk.DelimOpen Dl.Pr : rest) ->
    while rest
  (Tk.Do : rest) ->
    doWhile rest
  (Tk.For : Tk.DelimOpen Dl.Pr : rest) ->
    for rest
  (Tk.DelimOpen Dl.Br : rest) ->
    block rest
  (Tk.Return : rest) ->
    return_ rest
  (Tk.Goto : Tk.Id name : Tk.Semicolon : rest) ->
    (St.Goto name, rest)
  (Tk.Break : Tk.Semicolon : rest) ->
    (St.Break, rest)
  (Tk.Continue : Tk.Semicolon : rest) ->
    (St.Continue, rest)
  (Tk.Case : Tk.IntLiteral constant@(Constant ty _) : Tk.Op Op.TernaryElse : rest) ->
    case_ constant ty rest
  (Tk.Id name : Tk.Op Op.TernaryElse : rest) ->
    label name rest
  (Tk.Type ty : Tk.Id name : tokens') ->
    simpleStatement (varStatement ty name) tokens'
  _ ->
    simpleStatement (St.Expr . expr) tokens

if_ :: [Token] -> (Statement, [Token])
if_ tokens = do
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
  let (then_, rest'') = statement rest'
  case rest'' of
    (Tk.Else : rest''') ->
      let (else_, rest'''') = statement rest'''
       in (St.If (expr cond) then_ (Just else_), rest'''')
    _ -> (St.If (expr cond) then_ Nothing, rest'')

switch :: [Token] -> (Statement, [Token])
switch tokens = do
  let (eval, rest') = collectUntilDelimiter Dl.Pr tokens
  let (body, rest'') = statement rest'
   in (St.Switch (expr eval) body, rest'')

case_ :: Constant Constant.IntRepr -> Type -> b -> (Statement, b)
case_ constant ty tokens =
  ( if Type.isInteger ty
      then St.Case constant -- TODO enum in case
      else St.Invalid $ "Invalid type for case constant: " ++ show ty,
    tokens
  )

while :: [Token] -> (Statement, [Token])
while tokens = do
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
  let (body, rest'') = statement rest'
   in (St.While (expr cond) body, rest'')

doWhile :: [Token] -> (Statement, [Token])
doWhile tokens = do
  let (body, rest') = statement tokens
  case rest' of
    (Tk.While : Tk.DelimOpen Dl.Pr : rest'') -> do
      let (cond, rest''') = collectUntilDelimiter Dl.Pr rest''
      case rest''' of
        (Tk.Semicolon : rest'''') -> (St.DoWhile body $ expr cond, rest'''')
        _ -> (St.Invalid "Expected semicolon", rest')
    _ -> (St.Invalid "Expected while (", rest')

for :: [Token] -> (Statement, [Token])
for tokens = do
  let (decl, rest') = collectUntil Semicolon tokens
  let (cond, rest'') = collectUntil Semicolon rest'
  let (incr, rest''') = collectUntilDelimiter Dl.Pr rest''
  let (body, rest'''') = statement rest'''
  case decl of
    (Tk.Type ty : Tk.Id name : assign) ->
      (St.ForVar (varStatement ty name assign) (expr <$> listToMaybeList cond) (expr <$> listToMaybeList incr) body, rest'''')
    _ -> (St.For (expr <$> listToMaybeList decl) (expr <$> listToMaybeList cond) (expr <$> listToMaybeList incr) body, rest'''')

block :: [Token] -> (Statement, [Token])
block tokens =
  let (tokens', rest') = collectUntilDelimiter Dl.Br tokens
   in (St.Block (statementList tokens'), rest')

return_ :: [Token] -> (Statement, [Token])
return_ tokens =
  case collectUntil Tk.Semicolon tokens of
    ([], rest') -> (St.Return Nothing, rest')
    (tokens', rest') -> (St.Return (Just (expr tokens')), rest')

label :: Id -> [Token] -> (Statement, [Token])
label name tokens =
  case Token.filterNL tokens of
    (Tk.Id _ : Tk.Op Op.TernaryElse : _) -> (St.Labeled name St.Empty, tokens)
    tokens' ->
      let (st, rest') = statement tokens'
       in (St.Labeled name st, rest')

simpleStatement :: ([Token] -> Statement) -> [Token] -> (Statement, [Token])
simpleStatement parser tokens =
  let (tokens', rest') = collectUntil Tk.Semicolon tokens
   in (parser tokens', rest')

varStatement :: Type -> Id -> [Token] -> Statement
varStatement ty name tokens = case tokens of
  [] -> St.Var ty name Nothing
  (Tk.Op Op.Assign : tokens') -> St.Var ty name (Just (expr tokens'))
  _ -> St.Invalid ("Invalid assignment : " ++ show tokens)

exprList :: [Token] -> [Expr]
exprList tokens = case tokens of
  [] -> []
  (Tk.Op Op.Comma : rest) -> exprList rest
  _ -> let (ex, rest) = collectUntil (Tk.Op Op.Comma) tokens in expr ex : exprList rest

expr :: [Token] -> Expr
expr tokens = case tokens of
  [] ->
    Ex.Invalid "Empty expression"
  (Tk.NL : rest) ->
    expr rest
  (Tk.BoolLiteral constant@(Constant Ty.Bool _) : rest) ->
    exprNext (Ex.BoolLiteral constant) rest
  (Tk.IntLiteral constant : rest)
    | Type.isInteger $ Constant.ty constant ->
        exprNext (Ex.IntLiteral constant) rest
  (Tk.FltLiteral constant : rest)
    | Type.isFloating $ Constant.ty constant ->
        exprNext (Ex.FltLiteral constant) rest
  (Tk.StrLiteral constant@(Constant (Ty.Array Ty.Char _) _) : rest) ->
    exprNext (Ex.StrLiteral constant) rest
  (Tk.Id identifier : rest) ->
    exprNext (Ex.Id identifier) rest
  (Tk.Op op : rest)
    | Op.isUnaryPre op ->
        Ex.UnopPre op (expr rest)
  (Tk.DelimOpen Dl.Br : rest) ->
    Ex.ArrayDecl (exprList (collectArrayDecl rest))
  (Tk.DelimOpen Dl.Pr : rest) ->
    Ex.Parenthese (expr (collectParenthese rest))
  _ ->
    Ex.Invalid ("Invalid expression : " ++ show tokens)

exprNext :: Expr -> [Token] -> Expr
exprNext ex tokens = case tokens of
  [] -> ex
  [Tk.Op op] | Op.isUnaryPost op -> Ex.UnopPost op ex
  (Tk.Op op : rest) | Op.isBinary op -> binop ex op (expr rest)
  (Tk.DelimOpen Dl.SqBr : rest) -> binop ex Op.Subscript (expr (collectIndex rest))
  (Tk.DelimOpen Dl.Pr : rest) -> Ex.Call ex (exprList (collectArguments rest))
  _ -> Ex.Invalid ("Invalid follow expression : " ++ show ex ++ ", " ++ show tokens)

collectArrayDecl :: [Token] -> [Token]
collectArrayDecl tokens = let (tokens', _) = collectUntilDelimiter Dl.Br tokens in tokens'

collectParenthese :: [Token] -> [Token]
collectParenthese tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectArguments :: [Token] -> [Token]
collectArguments tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectIndex :: [Token] -> [Token]
collectIndex tokens = let (tokens', _) = collectUntilDelimiter Dl.SqBr tokens in tokens'

binop :: Expr -> Op -> Expr -> Expr
binop left op right = case right of
  Ex.Binop {left = r_left, op = r_op, right = r_right}
    | Op.precedence op <= Op.precedence r_op ->
        Ex.Binop (binop left op r_left) r_op r_right
  _ -> Ex.Binop left op right

collectParameters :: [Token] -> ([(Type, Id)], [Token])
collectParameters tokens = case tokens of
  [] -> ([], [])
  _ ->
    let (tokens', rest) = collectUntilDelimiter Dl.Pr tokens
     in (makeList tokens', rest)
  where
    makeList :: [Token] -> [(Type, Id)]
    makeList tokens'' = case tokens'' of
      [] -> []
      (Tk.Type ty : Tk.Id name : Tk.Op Op.Comma : rest) -> (ty, name) : makeList rest
      (Tk.Type ty : Tk.Id name : _) -> [(ty, name)]
      _ -> error ("Invalid parameters : " ++ show tokens'')

func :: Type -> Id -> [Token] -> (Declaration, [Token])
func ty name tokens =
  case collectParameters tokens of
    (params, Tk.Semicolon : rest) -> (funcDef ty name params, rest)
    (params, Tk.DelimOpen Dl.Br : rest) ->
      let (body, rest') = collectFuncBody rest
       in (funcDec ty name params body, rest')
    (_, rest) -> (Declaration.Invalid "Expected semicolon", rest)

funcDef :: Type -> Id -> [(Type, Id)] -> Declaration
funcDef = Declaration.FuncDef

collectFuncBody :: [Token] -> ([Token], [Token])
collectFuncBody = collectUntilDelimiter Dl.Br

funcDec :: Type -> Id -> [(Type, Id)] -> [Token] -> Declaration
funcDec ty name params body = Declaration.FuncDec ty name params (statementList body)

collectStructFields :: [Token] -> ([Token], [Token])
collectStructFields = collectUntilDelimiter Dl.Br

struct :: Maybe Id -> [Token] -> (Declaration, [Token])
struct name tokens = do
  let (tokens', rest') = collectStructFields tokens
  case rest' of
    (Tk.Semicolon : rest'') -> (Declaration.Struct name (structFields tokens'), rest'')
    (tk : rest'') -> (Declaration.Invalid ("Expected semicolon, got " ++ show tk), rest'')
    [] -> (Declaration.Invalid "Expected semicolon", rest')

structFields :: [Token] -> [(Type, Id)]
structFields tokens = case Token.filterNL tokens of
  [] -> []
  tokens' -> do
    let (field, rest) = collectUntil Tk.Semicolon tokens'
    case field of
      [Tk.Type ty, Tk.Id name] -> (ty, name) : structFields rest
      (tk : _) -> error $ "Unexpected token : " ++ show tk
      [] -> error "Empty field"

enum :: Maybe Id -> Type -> [Token] -> (Declaration, [Token])
enum name ty tokens = do
  let (tokens', rest') = collectEnumVariants tokens
  case rest' of
    (Tk.Semicolon : rest'') -> (Declaration.Enum name ty (enumVariants tokens'), rest'')
    (tk : rest'') -> (Declaration.Invalid ("Expected semicolon, got " ++ show tk), rest'')
    [] -> (Declaration.Invalid "Expected semicolon", rest')

collectEnumVariants :: [Token] -> ([Token], [Token])
collectEnumVariants = collectUntilDelimiter Dl.Br

enumVariants :: [Token] -> [(Id, Maybe Expr)]
enumVariants tokens = case Token.filterNL tokens of
  [] -> []
  tokens' -> do
    let (field, rest) = collectUntil (Tk.Op Op.Comma) tokens'
    case field of
      (Tk.Id name : Tk.Op Op.Assign : rest') -> (name, Just $ expr rest') : enumVariants rest
      [Tk.Id name] -> (name, Nothing) : enumVariants rest
      (tk : _) -> error $ "Unexpected token : " ++ show tk
      [] -> error "Empty field"