module Parser (parse) where

import Constant (Constant (..))
import Constant qualified
import Cursor (fold, (|+|))
import Data.List (intercalate)
import Declaration (Declaration)
import Declaration qualified (errs)
import Declaration qualified as Decl (Declaration (..))
import Delimiter qualified as Dl
import Expr (Expr (Expr))
import Expr qualified (Expr (..))
import Expr qualified as ED (ExprDef (..))
import Identifier (Id)
import Op (Op)
import Op qualified
import Statement (Statement)
import Statement qualified as St
import Token (Token (Token), collectUntil, collectUntilDelimiter, parseList)
import Token qualified (Token (..), filterNL)
import Token qualified as TD (TokenDef (..))
import Type (Type)
import Type qualified as Ty
import Utils (listToMaybeList)

parse :: [Token] -> [Declaration]
parse tokens =
  let decls = declarations (static tokens)
      !_ = case Declaration.errs decls of
        [] -> ()
        tkErrs -> error $ "Parser errors :\n" ++ intercalate "\n" tkErrs
   in decls

static :: [Token] -> [Token]
static tokens = case tokens of
  [] -> []
  Token cl TD.Signed
    : Token cr (TD.Type ty)
    : tks ->
      static (Token (cl |+| cr) (TD.Type (Ty.signed ty)) : tks)
  Token cl TD.Unsigned
    : Token cr (TD.Type ty)
    : tks ->
      static (Token (cl |+| cr) (TD.Type (Ty.unsigned ty)) : tks)
  Token cl (TD.Type ty)
    : Token cr (TD.Op Op.MultOrIndir)
    : tks ->
      static (Token (cl |+| cr) (TD.Type (Ty.Pointer ty)) : tks)
  Token cl TD.Struct
    : Token cr (TD.Id name)
    : tks ->
      static (Token (cl |+| cr) (TD.Type (Ty.Struct (Just name))) : tks)
  Token cursor TD.Struct
    : tks ->
      static (Token cursor (TD.Type (Ty.Struct Nothing)) : tks)
  (tk : tks) -> tk : static tks

declarations :: [Token] -> [Declaration]
declarations tokens = case tokens of
  [] -> []
  [Token _ TD.Eof] -> []
  Token cl TD.Enum
    : Token _ (TD.Id name)
    : Token _ (TD.Op Op.Colon)
    : Token _ (TD.Type ty)
    : Token cr (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum (Just name) ty tks
  Token cl TD.Enum
    : Token _ (TD.Op Op.Colon)
    : Token _ (TD.Type ty)
    : Token cr (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum Nothing ty tks
  Token cl TD.Enum
    : Token _ (TD.Id name)
    : Token cr (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum (Just name) Ty.Int tks
  Token cl TD.Enum
    : Token cr (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum Nothing Ty.Int tks
  Token cl (TD.Type (Ty.Struct name))
    : Token cr (TD.DelimOpen Dl.Br)
    : tks ->
      put $ struct name tks
  Token cl (TD.Type ty)
    : Token _ (TD.Id name)
    : Token cr (TD.DelimOpen Dl.Pr)
    : tks ->
      put $ func ty name tks
  Token _ TD.NL
    : tks ->
      declarations tks
  tk
    : tks ->
      Decl.Invalid ("Unexpected token " ++ show tk) : declarations tks
  where
    put :: (Declaration, [Token]) -> [Declaration]
    put tuple =
      let (declaration, tks) = tuple
       in declaration : declarations tks

statementList :: [Token] -> [Statement]
-- FIXME why NL ?
statementList = parseList TD.NL statement

statement :: [Token] -> (Statement, [Token])
statement tokens = case tokens of
  [] ->
    (St.Empty, [])
  Token _ TD.Semicolon
    : tks ->
      (St.Empty, tks)
  Token _ TD.If
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      if_ tks
  Token _ TD.Switch
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      switch tks
  Token _ TD.While
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      while tks
  Token _ TD.Do
    : tks ->
      doWhile tks
  Token _ TD.For
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      for tks
  Token _ (TD.DelimOpen Dl.Br)
    : tks ->
      block tks
  Token _ TD.Return
    : tks ->
      return_ tks
  Token _ TD.Goto
    : Token _ (TD.Id name)
    : Token _ TD.Semicolon
    : tks ->
      (St.Goto name, tks)
  Token _ TD.Break
    : Token _ TD.Semicolon
    : tks ->
      (St.Break, tks)
  Token _ TD.Continue
    : Token _ TD.Semicolon
    : tks ->
      (St.Continue, tks)
  Token _ TD.Case
    : Token _ (TD.IntLiteral constant@(Constant ty _))
    : Token _ (TD.Op Op.Colon)
    : tks ->
      case_ constant ty tks
  Token _ (TD.Id name)
    : Token _ (TD.Op Op.Colon)
    : tks ->
      label name tks
  -- TODO array cursor
  Token _ (TD.Type ty)
    : Token _ (TD.Id name)
    : Token _ (TD.DelimOpen Dl.SqBr)
    : Token _ (TD.IntLiteral (Constant len_ty len))
    : Token _ (TD.DelimClose Dl.SqBr)
    : tks
      | Ty.isInteger len_ty ->
          -- let clr = cl |+| cr
          --     nameLen = Cursor.len $ Token.crs name
          --     name' = Token (Cursor nameCursor)
          simpleStatement (varStatement (Ty.Array ty len) name) tks
  -- TODO array cursor
  Token _ (TD.Type ty)
    : (Token _ (TD.Id name))
    : Token _ (TD.DelimOpen Dl.SqBr)
    : Token _ (TD.DelimClose Dl.SqBr)
    : tks ->
      simpleStatement (varStatement (Ty.ArrayNoHint ty) name) tks
  Token _ (TD.Type ty)
    : Token _ (TD.Id name)
    : tks ->
      simpleStatement (varStatement ty name) tks
  tks ->
    simpleStatement (St.Expr . expr) tks

if_ :: [Token] -> (Statement, [Token])
if_ tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (then_, rest'') = statement rest'
   in case rest'' of
        Token _ TD.Else : rest''' ->
          let (else_, rest'''') = statement rest'''
           in (St.If (expr cond) then_ (Just else_), rest'''')
        _ -> (St.If (expr cond) then_ Nothing, rest'')

switch :: [Token] -> (Statement, [Token])
switch tokens =
  let (eval, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (St.Switch (expr eval) body, rest'')

case_ :: Constant Constant.IntRepr -> Type -> b -> (Statement, b)
case_ constant ty tokens = (st, tokens)
  where
    st
      | Ty.isInteger ty = St.Case constant -- TODO enum in case
      | otherwise = St.Invalid $ "Invalid type for case constant: " ++ show ty

while :: [Token] -> (Statement, [Token])
while tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (St.While (expr cond) body, rest'')

doWhile :: [Token] -> (Statement, [Token])
doWhile tokens =
  let (body, rest') = statement tokens
   in case rest' of
        Token _ TD.While : (Token _ (TD.DelimOpen Dl.Pr)) : rest'' ->
          let (cond, rest''') = collectUntilDelimiter Dl.Pr rest''
           in case rest''' of
                (Token _ TD.Semicolon : rest'''') -> (St.DoWhile body $ expr cond, rest'''')
                _ -> (St.Invalid "Expected semicolon", rest')
        _ -> (St.Invalid "Expected while (", rest')

for :: [Token] -> (Statement, [Token])
for tokens =
  let (decl, rest') = collectUntil TD.Semicolon tokens
      (cond, rest'') = collectUntil TD.Semicolon rest'
      (incr, rest''') = collectUntilDelimiter Dl.Pr rest''
      (body, rest'''') = statement rest'''
   in ( case decl of
          Token _ (TD.Type ty) : Token _ (TD.Id name) : assign ->
            St.ForVar
              (varStatement ty name assign)
              (expr <$> listToMaybeList cond)
              (expr <$> listToMaybeList incr)
              body
          _ ->
            St.For
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
  case collectUntil TD.Semicolon tokens of
    ([], rest') -> (St.Return Nothing, rest')
    (tokens', rest') -> (St.Return (Just (expr tokens')), rest')

label :: Id -> [Token] -> (Statement, [Token])
label name tokens = case Token.filterNL tokens of
  Token _ (TD.Id _) : Token _ (TD.Op Op.Colon) : _ -> (St.Labeled name St.Empty, tokens)
  tokens' ->
    let (st, rest') = statement tokens'
     in (St.Labeled name st, rest')

simpleStatement :: ([Token] -> Statement) -> [Token] -> (Statement, [Token])
simpleStatement parser tokens =
  let (tokens', rest') = collectUntil TD.Semicolon tokens
   in (parser tokens', rest')

varStatement :: Type -> Id -> [Token] -> Statement
varStatement ty name tokens = case tokens of
  [] -> St.Var ty name Nothing
  Token _ (TD.Op Op.Assign) : tokens' -> St.Var ty name (Just (expr tokens'))
  _ -> St.Invalid ("Invalid assignment : " ++ show tokens)

exprList :: [Token] -> [Expr]
exprList tokens = case tokens of
  [] -> []
  Token _ (TD.Op Op.Comma) : tks -> exprList tks
  _ -> let (ex, tks) = collectUntil (TD.Op Op.Comma) tokens in expr ex : exprList tks

expr :: [Token] -> Expr
expr tokens = case tokens of
  [] ->
    Expr cursor (ED.Invalid "Empty expression")
  Token _ TD.NL : tks ->
    expr tks
  Token _ (TD.IntLiteral constant) : tks
    | Ty.isInteger $ Constant.ty constant ->
        exprNext (Expr cursor (ED.IntLiteral constant)) tks
  Token _ (TD.FltLiteral constant) : tks
    | Ty.isFloating $ Constant.ty constant ->
        exprNext (Expr cursor (ED.FltLiteral constant)) tks
  Token _ (TD.StrLiteral constant@(Constant (Ty.Array Ty.Char _) _)) : tks ->
    exprNext (Expr cursor (ED.StrLiteral constant)) tks
  Token _ (TD.Id identifier) : tks ->
    exprNext (Expr cursor (ED.Id identifier)) tks
  Token _ (TD.Op op) : tks
    | Op.isUnaryPre op ->
        Expr cursor (ED.UnopPre op (expr tks))
  Token _ (TD.DelimOpen Dl.Br) : tks ->
    Expr cursor (ED.ArrayDecl (exprList (collectArrayDecl tks)))
  Token _ (TD.DelimOpen Dl.Pr) : tks ->
    let (pr, rest) = collectParenthese tks
     in exprNext (Expr cursor (ED.Parenthese (expr pr))) rest
  tks ->
    Expr cursor (ED.Invalid ("Invalid expression : " ++ show tks))
  where
    cursor = Cursor.fold (map Token.crs tokens)

exprNext :: Expr -> [Token] -> Expr
exprNext ex tokens = case tokens of
  [] -> ex
  [Token _ (TD.Op op)] | Op.isUnaryPost op -> Expr cursor (ED.UnopPost op ex)
  Token _ (TD.Op op) : tks | Op.isBinary op -> binop ex op (expr tks)
  Token _ (TD.DelimOpen Dl.SqBr) : tks -> binop ex Op.Subscript (expr (collectIndex tks))
  Token _ (TD.DelimOpen Dl.Pr) : tks -> Expr cursor (ED.Call ex (exprList (collectArguments tks)))
  _ -> Expr cursor (ED.Invalid ("Invalid follow expression for " ++ show ex ++ " : " ++ show tokens))
  where
    cursor = Cursor.fold (map Token.crs tokens)

collectArrayDecl :: [Token] -> [Token]
collectArrayDecl tokens = let (tokens', _) = collectUntilDelimiter Dl.Br tokens in tokens'

collectParenthese :: [Token] -> ([Token], [Token])
collectParenthese tokens = let (pr, rest) = collectUntilDelimiter Dl.Pr tokens in (pr, rest)

collectArguments :: [Token] -> [Token]
collectArguments tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectIndex :: [Token] -> [Token]
collectIndex tokens = let (tokens', _) = collectUntilDelimiter Dl.SqBr tokens in tokens'

binop :: Expr -> Op -> Expr -> Expr
binop left op right = case right of
  Expr cr (ED.Binop r_left r_op r_right)
    | Op.precedence op <= Op.precedence r_op ->
        Expr (cl |+| cr) (ED.Binop (binop left op r_left) r_op r_right)
  Expr cr _ -> Expr (cl |+| cr) (ED.Binop left op right)
  where
    cl = Expr.crs left

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
      Token _ (TD.Type ty) : Token _ (TD.Id name) : Token _ (TD.Op Op.Comma) : rest -> (ty, name) : makeList rest
      Token _ (TD.Type ty) : Token _ (TD.Id name) : _ -> [(ty, name)]
      _ -> error ("Invalid parameters : " ++ show tokens'')

func :: Type -> Id -> [Token] -> (Declaration, [Token])
func ty name tokens =
  case rest of
    Token _ TD.Semicolon : rest' -> (funcDef ty name parameters, rest')
    Token _ (TD.DelimOpen Dl.Br) : rest' ->
      let (body, rest'') = collectFuncBody rest'
       in (funcDec ty name parameters body, rest'')
    _ -> (Decl.Invalid "Expected semicolon", rest)
  where
    (parameters, rest) = collectParameters tokens

funcDef :: Type -> Id -> [(Type, Id)] -> Declaration
funcDef = Decl.FuncDef

collectFuncBody :: [Token] -> ([Token], [Token])
collectFuncBody = collectUntilDelimiter Dl.Br

funcDec :: Type -> Id -> [(Type, Id)] -> [Token] -> Declaration
funcDec ty name params body = Decl.FuncDec ty name params (statementList body)

collectStructFields :: [Token] -> ([Token], [Token])
collectStructFields = collectUntilDelimiter Dl.Br

struct :: Maybe Id -> [Token] -> (Declaration, [Token])
struct name tokens =
  let (tokens', rest') = collectStructFields tokens
   in case rest' of
        Token _ TD.Semicolon : rest'' -> (Decl.Struct name (structFields tokens'), rest'')
        tk : rest'' -> (Decl.Invalid ("Expected semicolon, got " ++ show tk), rest'')
        [] -> (Decl.Invalid "Expected semicolon", rest')

structFields :: [Token] -> [(Type, Id)]
structFields tokens = case Token.filterNL tokens of
  [] -> []
  tokens' ->
    let (field, rest) = collectUntil TD.Semicolon tokens'
     in case field of
          [Token _ (TD.Type ty), Token _ (TD.Id name)] -> (ty, name) : structFields rest
          tk : _ -> error $ "Unexpected token : " ++ show tk
          [] -> error "Empty field"

enum :: Maybe Id -> Type -> [Token] -> (Declaration, [Token])
enum name ty tokens =
  let (tokens', rest') = collectEnumVariants tokens
   in case rest' of
        Token _ TD.Semicolon : rest'' -> (Decl.Enum name ty (enumVariants tokens'), rest'')
        tk : rest'' -> (Decl.Invalid ("Expected semicolon, got " ++ show tk), rest'')
        [] -> (Decl.Invalid "Expected semicolon", rest')

collectEnumVariants :: [Token] -> ([Token], [Token])
collectEnumVariants = collectUntilDelimiter Dl.Br

enumVariants :: [Token] -> [(Id, Maybe Expr)]
enumVariants tokens = case Token.filterNL tokens of
  [] -> []
  tokens' ->
    let (field, rest) = collectUntil (TD.Op Op.Comma) tokens'
     in case field of
          Token _ (TD.Id name) : Token _ (TD.Op Op.Assign) : rest' -> (name, Just $ expr rest') : enumVariants rest
          [Token _ (TD.Id name)] -> (name, Nothing) : enumVariants rest
          tk : _ -> error $ "Unexpected token : " ++ show tk
          [] -> error "Empty field"