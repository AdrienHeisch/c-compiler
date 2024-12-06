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
  ( Tk.Signed
      : Tk.Type ty
      : tks
    ) -> static (Tk.Type (Type.signed ty) : tks)
  ( Tk.Unsigned
      : Tk.Type ty
      : tks
    ) -> static (Tk.Type (Type.unsigned ty) : tks)
  ( Tk.Type ty
      : Tk.Op Op.MultOrIndir
      : tks
    ) -> static (Tk.Type (Ty.Pointer ty) : tks)
  ( Tk.Type ty
      : name@(Tk.Id _)
      : Tk.DelimOpen Dl.SqBr
      : Tk.IntLiteral (Constant len_ty len)
      : Tk.DelimClose Dl.SqBr
      : tks
    )
      | Type.isInteger len_ty ->
          static (Tk.Type (Ty.Array ty len) : name : tks)
  ( Tk.Type ty
      : name@(Tk.Id _)
      : Tk.DelimOpen Dl.SqBr
      : Tk.DelimClose Dl.SqBr
      : tks
    ) -> static (Tk.Type (Ty.ArrayNoHint ty) : name : tks)
  ( Tk.Struct
      : Tk.Id name
      : tks
    ) -> static (Tk.Type (Ty.Struct (Just name)) : tks)
  ( Tk.Struct
      : tks
    ) -> static (Tk.Type (Ty.Struct Nothing) : tks)
  (tk : tks) -> tk : static tks

declarations :: [Token] -> [Declaration]
declarations tokens = case tokens of
  [] -> []
  [Eof] -> []
  ( Tk.Enum
      : Tk.Id name
      : Tk.Op Op.TernaryElse
      : Tk.Type ty
      : Tk.DelimOpen Dl.Br
      : tks
    ) -> put $ enum (Just name) ty tks
  ( Tk.Enum
      : Tk.Op Op.TernaryElse
      : Tk.Type ty
      : Tk.DelimOpen Dl.Br
      : tks
    ) -> put $ enum Nothing ty tks
  ( Tk.Enum
      : Tk.Id name
      : Tk.DelimOpen Dl.Br
      : tks
    ) -> put $ enum (Just name) Ty.Int tks
  ( Tk.Enum
      : Tk.DelimOpen Dl.Br
      : tks
    ) -> put $ enum Nothing Ty.Int tks
  ( Tk.Type (Ty.Struct name)
      : Tk.DelimOpen Dl.Br
      : tks
    ) -> put $ struct name tks
  ( Tk.Type ty
      : Tk.Id name
      : Tk.DelimOpen Dl.Pr
      : tks
    ) -> put $ func ty name tks
  ( Tk.NL
      : tks
    ) -> declarations tks
  ( tk
      : tks
    ) -> Declaration.Invalid ("Unexpected token " ++ show tk) : declarations tks
  where
    put tuple =
      let (declaration, tks) = tuple
       in declaration : declarations tks

-- collectDirective :: [Token] -> ([Token], [Token])
-- collectDirective = collectUntil Tk.NL

-- parseDirective :: [Token] -> Declaration
-- parseDirective _ = Declaration.Directive

collectUntil :: Token -> [Token] -> ([Token], [Token])
collectUntil end tokens = case tokens of
  [] -> ([], [])
  (tk : tks) | tk == end -> ([], tks)
  (tk : tks) ->
    let (tks', rest) = collectUntil end tks
     in (tk : tks', rest)

collectUntilDelimiter :: Delimiter -> [Token] -> ([Token], [Token])
collectUntilDelimiter del = collect 0
  where
    collect :: Int -> [Token] -> ([Token], [Token])
    collect depth tokens = case tokens of
      [] -> ([], [])
      (tk : tks) | tk == Tk.DelimClose del && depth == 0 -> ([], tks)
      (tk : tks) | tk == Tk.DelimClose del -> collectNext tk tks (depth - 1)
      (tk : tks) | tk == Tk.DelimOpen del -> collectNext tk tks (depth + 1)
      (tk : tks) -> collectNext tk tks depth
    collectNext :: Token -> [Token] -> Int -> ([Token], [Token])
    collectNext tk rest depth =
      let (tks', rest') = collect depth rest
       in (tk : tks', rest')

parseList :: Token -> ([Token] -> (el, [Token])) -> [Token] -> [el]
parseList end parser tokens = case tokens of
  [] -> []
  (tk : tks) | tk == end -> parseList end parser tks
  _ ->
    let (el, tks) = parser tokens
     in el : parseList end parser tks

statementList :: [Token] -> [Statement]
statementList = parseList Tk.NL statement

statement :: [Token] -> (Statement, [Token])
statement tokens = case tokens of
  [] ->
    (St.Empty, [])
  ( Tk.Semicolon
      : tks
    ) -> (St.Empty, tks)
  ( Tk.If
      : Tk.DelimOpen Dl.Pr
      : tks
    ) -> if_ tks
  ( Tk.Switch
      : Tk.DelimOpen Dl.Pr
      : tks
    ) -> switch tks
  ( Tk.While
      : Tk.DelimOpen Dl.Pr
      : tks
    ) -> while tks
  ( Tk.Do
      : tks
    ) -> doWhile tks
  ( Tk.For
      : Tk.DelimOpen Dl.Pr
      : tks
    ) -> for tks
  ( Tk.DelimOpen Dl.Br
      : tks
    ) -> block tks
  ( Tk.Return
      : tks
    ) -> return_ tks
  ( Tk.Goto
      : Tk.Id name
      : Tk.Semicolon
      : tks
    ) -> (St.Goto name, tks)
  ( Tk.Break
      : Tk.Semicolon
      : tks
    ) -> (St.Break, tks)
  ( Tk.Continue
      : Tk.Semicolon
      : tks
    ) -> (St.Continue, tks)
  ( Tk.Case
      : Tk.IntLiteral constant@(Constant ty _)
      : Tk.Op Op.TernaryElse
      : tks
    ) -> case_ constant ty tks
  ( Tk.Id
      name
      : Tk.Op Op.TernaryElse
      : tks
    ) -> label name tks
  ( Tk.Type
      ty
      : Tk.Id name
      : tks
    ) -> simpleStatement (varStatement ty name) tks
  tks ->
    simpleStatement (St.Expr . expr) tks

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
      ( St.ForVar
          (varStatement ty name assign)
          (expr <$> listToMaybeList cond)
          (expr <$> listToMaybeList incr)
          body,
        rest''''
      )
    _ ->
      ( St.For
          (expr <$> listToMaybeList decl)
          (expr <$> listToMaybeList cond)
          (expr <$> listToMaybeList incr)
          body,
        rest''''
      )

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
  (Tk.Op Op.Comma : tks) -> exprList tks
  _ -> let (ex, tks) = collectUntil (Tk.Op Op.Comma) tokens in expr ex : exprList tks

expr :: [Token] -> Expr
expr tokens = case tokens of
  [] ->
    Ex.Invalid "Empty expression"
  (Tk.NL : tks) ->
    expr tks
  (Tk.BoolLiteral constant@(Constant Ty.Bool _) : tks) ->
    exprNext (Ex.BoolLiteral constant) tks
  (Tk.IntLiteral constant : tks)
    | Type.isInteger $ Constant.ty constant ->
        exprNext (Ex.IntLiteral constant) tks
  (Tk.FltLiteral constant : tks)
    | Type.isFloating $ Constant.ty constant ->
        exprNext (Ex.FltLiteral constant) tks
  (Tk.StrLiteral constant@(Constant (Ty.Array Ty.Char _) _) : tks) ->
    exprNext (Ex.StrLiteral constant) tks
  (Tk.Id identifier : tks) ->
    exprNext (Ex.Id identifier) tks
  (Tk.Op op : tks)
    | Op.isUnaryPre op ->
        Ex.UnopPre op (expr tks)
  (Tk.DelimOpen Dl.Br : tks) ->
    Ex.ArrayDecl (exprList (collectArrayDecl tks))
  (Tk.DelimOpen Dl.Pr : tks) ->
    Ex.Parenthese (expr (collectParenthese tks))
  tks ->
    Ex.Invalid ("Invalid expression : " ++ show tks)

exprNext :: Expr -> [Token] -> Expr
exprNext ex tokens = case tokens of
  [] -> ex
  [Tk.Op op] | Op.isUnaryPost op -> Ex.UnopPost op ex
  (Tk.Op op : tks) | Op.isBinary op -> binop ex op (expr tks)
  (Tk.DelimOpen Dl.SqBr : tks) -> binop ex Op.Subscript (expr (collectIndex tks))
  (Tk.DelimOpen Dl.Pr : tks) -> Ex.Call ex (exprList (collectArguments tks))
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