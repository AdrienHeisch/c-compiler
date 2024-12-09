module Parser (parse) where

import Constant (Constant (..))
import Constant qualified
import Cursor (Cursor (Cursor), fold, (|+|))
import Cursor qualified (Cursor (..))
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
import Statement (Statement (Statement))
import Statement qualified (Statement (..))
import Statement qualified as SD (StatementDef (..))
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
    (Statement (foldTkCrs tokens) SD.Empty, [])
  Token crs TD.Semicolon
    : tks ->
      (Statement crs SD.Empty, tks)
  Token crs TD.If
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      if_ crs tks
  Token crs TD.Switch
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      switch crs tks
  Token crs TD.While
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      while crs tks
  Token crs TD.Do
    : tks ->
      doWhile crs tks
  Token crs TD.For
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      for crs tks
  Token crs (TD.DelimOpen Dl.Br)
    : tks ->
      block crs tks
  Token crs TD.Return
    : tks ->
      return_ crs tks
  Token cl TD.Goto
    : Token _ (TD.Id name)
    : Token cr TD.Semicolon
    : tks ->
      (Statement (cl |+| cr) (SD.Goto name), tks)
  Token cl TD.Break
    : Token cr TD.Semicolon
    : tks ->
      (Statement (cl |+| cr) SD.Break, tks)
  Token cl TD.Continue
    : Token cr TD.Semicolon
    : tks ->
      (Statement (cl |+| cr) SD.Continue, tks)
  Token cursor TD.Case
    : Token _ (TD.IntLiteral constant@(Constant ty _))
    : Token _ (TD.Op Op.Colon)
    : tks ->
      case_ cursor constant ty tks
  Token cursor (TD.Id name)
    : Token _ (TD.Op Op.Colon)
    : tks ->
      label cursor name tks
  -- TODO array cursor
  Token cl (TD.Type ty)
    : Token _ (TD.Id name)
    : Token _ (TD.DelimOpen Dl.SqBr)
    : Token _ (TD.IntLiteral (Constant len_ty len))
    : Token cr (TD.DelimClose Dl.SqBr)
    : tks
      | Ty.isInteger len_ty ->
          -- let clr = cl |+| cr
          --     nameLen = Cursor.len $ Token.crs name
          --     name' = Token (Cursor nameCursor)
          simpleStatement (varStatement (cl |+| cr) (Ty.Array ty len) name) tks
  -- TODO array cursor
  Token cl (TD.Type ty)
    : (Token _ (TD.Id name))
    : Token _ (TD.DelimOpen Dl.SqBr)
    : Token cr (TD.DelimClose Dl.SqBr)
    : tks ->
      simpleStatement (varStatement (cl |+| cr) (Ty.ArrayNoHint ty) name) tks
  Token cl (TD.Type ty)
    : Token cr (TD.Id name)
    : tks ->
      simpleStatement (varStatement (cl |+| cr) ty name) tks
  _ ->
    let (tokens', rest') = collectUntil TD.Semicolon tokens
        expression = expr tokens'
     in (Statement (Expr.crs expression) (SD.Expr expression), rest')

foldTkCrs :: [Token] -> Cursor
foldTkCrs = Cursor.fold . map Token.crs

if_ :: Cursor -> [Token] -> (Statement, [Token])
if_ cursor tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (then_, rest'') = statement rest'
   in case rest'' of
        Token _ TD.Else : rest''' ->
          let (else_, rest'''') = statement rest'''
           in (Statement (cursor |+| Statement.crs else_) (SD.If (expr cond) then_ (Just else_)), rest'''')
        _ -> (Statement (cursor |+| Statement.crs then_) (SD.If (expr cond) then_ Nothing), rest'')

switch :: Cursor -> [Token] -> (Statement, [Token])
switch cursor tokens =
  let (eval, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (Statement (cursor |+| Statement.crs body) (SD.Switch (expr eval) body), rest'')

case_ :: Cursor -> Constant Constant.IntRepr -> Type -> [Token] -> (Statement, [Token])
case_ cursor constant ty tokens = (Statement (cursor |+| foldTkCrs tokens) stDef, tokens)
  where
    stDef
      | Ty.isInteger ty = SD.Case constant -- TODO enum in case
      | otherwise = SD.Invalid $ "Invalid type for case constant: " ++ show ty

while :: Cursor -> [Token] -> (Statement, [Token])
while cursor tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (Statement (cursor |+| Statement.crs body) (SD.While (expr cond) body), rest'')

doWhile :: Cursor -> [Token] -> (Statement, [Token])
doWhile cursor tokens =
  let (body, rest') = statement tokens
   in case rest' of
        Token _ TD.While : (Token _ (TD.DelimOpen Dl.Pr)) : rest'' ->
          let (cond, rest''') = collectUntilDelimiter Dl.Pr rest''
           in case rest''' of
                (Token _ TD.Semicolon : rest'''') -> (Statement (cursor |+| Statement.crs body) (SD.DoWhile body $ expr cond), rest'''')
                _ -> (Statement (cursor |+| Statement.crs body) (SD.Invalid "Expected semicolon"), rest')
        _ -> (Statement (cursor |+| Statement.crs body) (SD.Invalid "Expected while ("), rest')

for :: Cursor -> [Token] -> (Statement, [Token])
for cursor tokens =
  let (decl, rest') = collectUntil TD.Semicolon tokens
      (cond, rest'') = collectUntil TD.Semicolon rest'
      (incr, rest''') = collectUntilDelimiter Dl.Pr rest''
      (body, rest'''') = statement rest'''
   in ( Statement (cursor |+| Statement.crs body) $ case decl of
          Token _ (TD.Type ty) : Token _ (TD.Id name) : assign ->
            SD.ForVar
              (varStatement (foldTkCrs tokens) ty name assign)
              (expr <$> listToMaybeList cond)
              (expr <$> listToMaybeList incr)
              body
          _ ->
            SD.For
              (expr <$> listToMaybeList decl)
              (expr <$> listToMaybeList cond)
              (expr <$> listToMaybeList incr)
              body,
        rest''''
      )

block :: Cursor -> [Token] -> (Statement, [Token])
block cursor tokens =
  let (tokens', rest') = collectUntilDelimiter Dl.Br tokens
      sts = statementList tokens'
   in (Statement (cursor |+| foldTkCrs tokens') (SD.Block sts), rest')

return_ :: Cursor -> [Token] -> (Statement, [Token])
return_ cursor tokens =
  case collectUntil TD.Semicolon tokens of
    ([], rest') -> (Statement (cursor |+| Token.crs (head tokens)) (SD.Return Nothing), rest')
    (tokens', rest') -> (Statement (cursor |+| foldTkCrs tokens') (SD.Return (Just (expr tokens'))), rest')

label :: Cursor -> Id -> [Token] -> (Statement, [Token])
label cursor name tokens = case Token.filterNL tokens of
  Token _ (TD.Id _) : tk@(Token _ (TD.Op Op.Colon)) : _ -> (Statement (cursor |+| Token.crs tk) (SD.Labeled name (Statement (Token.crs tk) SD.Empty)), tokens)
  tokens' ->
    let (st, rest') = statement tokens'
     in (Statement (cursor |+| Statement.crs st) (SD.Labeled name st), rest')

simpleStatement :: ([Token] -> Statement) -> [Token] -> (Statement, [Token])
simpleStatement parser tokens =
  let (tokens', rest') = collectUntil TD.Semicolon tokens
   in (parser tokens', rest')

varStatement :: Cursor -> Type -> Id -> [Token] -> Statement
varStatement cursor ty name tokens = case tokens of
  [] -> Statement cursor (SD.Var ty name Nothing)
  Token _ (TD.Op Op.Assign) : tokens' -> Statement (cursor |+| foldTkCrs tokens) (SD.Var ty name (Just (expr tokens')))
  _ -> Statement (cursor |+| foldTkCrs tokens) (SD.Invalid ("Invalid assignment : " ++ show tokens))

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
    cursor = foldTkCrs tokens

exprNext :: Expr -> [Token] -> Expr
exprNext ex tokens = case tokens of
  [] -> ex
  [Token _ (TD.Op op)] | Op.isUnaryPost op -> Expr cursor (ED.UnopPost op ex)
  Token _ (TD.Op op) : tks | Op.isBinary op -> binop ex op (expr tks)
  Token _ (TD.DelimOpen Dl.SqBr) : tks -> binop ex Op.Subscript (expr (collectIndex tks))
  Token _ (TD.DelimOpen Dl.Pr) : tks -> Expr cursor (ED.Call ex (exprList (collectArguments tks)))
  _ -> Expr cursor (ED.Invalid ("Invalid follow expression for " ++ show ex ++ " : " ++ show tokens))
  where
    cursor = foldTkCrs tokens

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

collectParameters :: [Token] -> (Either [(Type, Id)] String, [Token])
collectParameters tokens = case tokens of
  [] -> (Left [], [])
  _ ->
    let (tokens', rest) = collectUntilDelimiter Dl.Pr tokens
     in (makeList tokens', rest)
  where
    makeList :: [Token] -> Either [(Type, Id)] String
    makeList tokens'' = case tokens'' of
      [] -> Left []
      Token _ (TD.Type ty) : Token _ (TD.Id name) : Token _ (TD.Op Op.Comma) : rest -> next (ty, name) rest
      Token _ (TD.Type ty) : Token _ (TD.Id name) : _ -> Left [(ty, name)]
      _ -> Right ("Invalid parameters : " ++ show tokens'')
      where
        next el tks = case makeList tks of
          Left els -> Left (el : els)
          Right err -> Right err

func :: Type -> Id -> [Token] -> (Declaration, [Token])
func ty name tokens =
  case rest of
    Token _ TD.Semicolon : rest' ->
      case mparameters of
        Left parameters -> (funcDef ty name parameters, rest')
        Right err -> (Decl.Invalid err, rest')
    Token _ (TD.DelimOpen Dl.Br) : rest' ->
      let (body, rest'') = collectFuncBody rest'
       in case mparameters of
            Left parameters ->
              (funcDec ty name parameters body, rest'')
            Right err -> (Decl.Invalid err, rest'')
    _ -> (Decl.Invalid "Expected ; or {", rest)
  where
    (mparameters, rest) = collectParameters tokens

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
        Token _ TD.Semicolon : rest'' -> case structFields tokens' of
          Left fields -> (Decl.Struct name fields, rest'')
          Right err -> (Decl.Invalid err, rest'')
        tk : rest'' -> (Decl.Invalid ("Expected semicolon, got " ++ show tk), rest'')
        [] -> (Decl.Invalid "Expected semicolon", rest')

structFields :: [Token] -> Either [(Type, Id)] String
structFields tokens = case Token.filterNL tokens of
  [] -> Left []
  tokens' ->
    let (field, rest) = collectUntil TD.Semicolon tokens'
     in case field of
          [Token _ (TD.Type ty), Token _ (TD.Id name)] -> next (ty, name) rest
          tk : _ -> Right $ "Unexpected token : " ++ show tk
          [] -> Right "Empty field"
  where
    next field tks = case structFields tks of
      Left fields -> Left (field : fields)
      Right err -> Right err

enum :: Maybe Id -> Type -> [Token] -> (Declaration, [Token])
enum name ty tokens =
  let (tokens', rest') = collectEnumVariants tokens
   in case rest' of
        Token _ TD.Semicolon : rest'' -> case enumVariants tokens' of
          Left variants -> (Decl.Enum name ty variants, rest'')
          Right err -> (Decl.Invalid err, rest'')
        tk : rest'' -> (Decl.Invalid ("Expected semicolon, got " ++ show tk), rest'')
        [] -> (Decl.Invalid "Expected semicolon", rest')

collectEnumVariants :: [Token] -> ([Token], [Token])
collectEnumVariants = collectUntilDelimiter Dl.Br

enumVariants :: [Token] -> Either [(Id, Maybe Expr)] String
enumVariants tokens = case Token.filterNL tokens of
  [] -> Left []
  tokens' ->
    let (variant, rest) = collectUntil (TD.Op Op.Comma) tokens'
     in case variant of
          Token _ (TD.Id name) : Token _ (TD.Op Op.Assign) : rest' -> next (name, Just $ expr rest') rest
          [Token _ (TD.Id name)] -> next (name, Nothing) rest
          tk : _ -> Right $ "Unexpected token : " ++ show tk
          [] -> Right "Empty field"
  where
    next variant tks = case enumVariants tks of
      Left variants -> Left (variant : variants)
      Right err -> Right err