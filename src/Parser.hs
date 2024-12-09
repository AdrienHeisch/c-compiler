module Parser (parse) where

import Constant (Constant (..))
import Cursor (Cursor, CursorOps (..))
import Data.List (intercalate)
import Declaration (Declaration (Declaration), DeclarationDef)
import Declaration qualified (errs)
import Declaration qualified as DD (DeclarationDef (..))
import Delimiter qualified as Dl
import Expr (Expr (Expr))
import Expr qualified (Expr (..))
import Expr qualified as ED (ExprDef (..))
import Identifier (Id)
import Op (Op)
import Op qualified
import Statement (Statement (Statement))
import Statement qualified as SD (StatementDef (..))
import Statement qualified as St (Statement (..))
import Token (Token (Token), collectUntil, collectUntilDelimiter, parseList)
import Token qualified (Token (..), filterNL, foldCrs)
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
  Token cursor TD.Enum
    : Token _ (TD.Id name)
    : Token _ (TD.Op Op.Colon)
    : Token _ (TD.Type ty)
    : Token _ (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum cursor (Just name) ty tks
  Token cursor TD.Enum
    : Token _ (TD.Op Op.Colon)
    : Token _ (TD.Type ty)
    : Token _ (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum cursor Nothing ty tks
  Token cursor TD.Enum
    : Token _ (TD.Id name)
    : Token _ (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum cursor (Just name) Ty.Int tks
  Token cursor TD.Enum
    : Token _ (TD.DelimOpen Dl.Br)
    : tks ->
      put $ enum cursor Nothing Ty.Int tks
  Token cursor (TD.Type (Ty.Struct name))
    : Token _ (TD.DelimOpen Dl.Br)
    : tks ->
      put $ struct cursor name tks
  Token cursor (TD.Type ty)
    : Token _ (TD.Id name)
    : Token _ (TD.DelimOpen Dl.Pr)
    : tks ->
      put $ func cursor ty name tks
  Token _ TD.NL
    : tks ->
      declarations tks
  Token cursor tkDef
    : tks ->
      Declaration cursor (DD.Invalid ("Unexpected token " ++ show tkDef)) : declarations tks
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
    (Statement (Token.foldCrs tokens) SD.Empty, [])
  Token crs TD.NL
    : tks ->
      (Statement crs SD.Empty, tks)
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
    : tks ->
      case_ cursor tks
  Token cursor (TD.Id name)
    : Token _ (TD.Op Op.Colon)
    : tks ->
      label cursor name tks
  Token cl (TD.Type ty)
    : Token _ (TD.Id name)
    : Token _ (TD.DelimOpen Dl.SqBr)
    : Token _ (TD.IntLiteral (Constant len_ty len))
    : Token cr (TD.DelimClose Dl.SqBr)
    : tks
      | Ty.isInteger len_ty ->
          simpleStatement (varStatement (cl |+| cr) (Ty.Array ty len) name) tks
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

if_ :: Cursor -> [Token] -> (Statement, [Token])
if_ cursor tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (then_, rest'') = statement rest'
   in case rest'' of
        Token _ TD.Else : rest''' ->
          let (else_, rest'''') = statement rest'''
           in (Statement (cursor |+| St.crs else_) (SD.If (expr cond) then_ (Just else_)), rest'''')
        _ -> (Statement (cursor |+| St.crs then_) (SD.If (expr cond) then_ Nothing), rest'')

switch :: Cursor -> [Token] -> (Statement, [Token])
switch cursor tokens =
  let (eval, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (Statement (cursor |+| St.crs body) (SD.Switch (expr eval) body), rest'')

case_ :: Cursor -> [Token] -> (Statement, [Token])
case_ cursor tokens = case tokens of
  Token _ (TD.IntLiteral constant@(Constant ty _))
    : Token cursor' (TD.Op Op.Colon)
    : rest
      | Ty.isInteger ty -> (Statement (cursor |+| cursor') (SD.Case constant), rest)
      | otherwise -> (Statement (cursor |+| cursor') (SD.Invalid $ "Invalid type for case constant: " ++ show ty), rest)
  _ ->
    let (tks, rest) = collectUntil (TD.Op Op.Colon) tokens
     in (Statement (cursor |+| Token.foldCrs tks) (SD.Invalid $ "Invalid case constant: " ++ show tks), rest)

while :: Cursor -> [Token] -> (Statement, [Token])
while cursor tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (Statement (cursor |+| St.crs body) (SD.While (expr cond) body), rest'')

doWhile :: Cursor -> [Token] -> (Statement, [Token])
doWhile cursor tokens =
  let (body, rest') = statement tokens
   in case rest' of
        Token _ TD.While : (Token _ (TD.DelimOpen Dl.Pr)) : rest'' ->
          let (cond, rest''') = collectUntilDelimiter Dl.Pr rest''
           in case rest''' of
                (Token _ TD.Semicolon : rest'''') -> (Statement (cursor |+| St.crs body) (SD.DoWhile body $ expr cond), rest'''')
                _ -> (Statement (cursor |+| St.crs body) (SD.Invalid "Expected semicolon"), rest')
        Token cursor' TD.While : rest'' -> (Statement (cursor |+| cursor') (SD.Invalid "Expected ("), rest'')
        _ -> (Statement (cursor |+| St.crs body) (SD.Invalid "Expected while ("), rest')

for :: Cursor -> [Token] -> (Statement, [Token])
for cursor tokens =
  let (decl, rest') = collectUntil TD.Semicolon tokens
      (cond, rest'') = collectUntil TD.Semicolon rest'
      (incr, rest''') = collectUntilDelimiter Dl.Pr rest''
      (body, rest'''') = statement rest'''
   in ( Statement (cursor |+| St.crs body) $ case decl of
          Token _ (TD.Type ty) : Token _ (TD.Id name) : assign ->
            SD.ForVar
              (varStatement (Token.foldCrs tokens) ty name assign)
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
   in (Statement (cursor |+| Token.foldCrs tokens') (SD.Block sts), rest')

return_ :: Cursor -> [Token] -> (Statement, [Token])
return_ cursor tokens =
  case collectUntil TD.Semicolon tokens of
    ([], rest') -> (Statement (cursor |+| Token.crs (head tokens)) (SD.Return Nothing), rest')
    (tokens', rest') -> (Statement (cursor |+| Token.foldCrs tokens') (SD.Return (Just (expr tokens'))), rest')

label :: Cursor -> Id -> [Token] -> (Statement, [Token])
label cursor name tokens = case Token.filterNL tokens of
  Token _ (TD.Id _) : tk@(Token _ (TD.Op Op.Colon)) : _ -> (Statement (cursor |+| Token.crs tk) (SD.Labeled name (Statement (Token.crs tk) SD.Empty)), tokens)
  tokens' ->
    let (st, rest') = statement tokens'
     in (Statement (cursor |+| St.crs st) (SD.Labeled name st), rest')

simpleStatement :: ([Token] -> Statement) -> [Token] -> (Statement, [Token])
simpleStatement parser tokens =
  let (tokens', rest') = collectUntil TD.Semicolon tokens
   in (parser tokens', rest')

varStatement :: Cursor -> Type -> Id -> [Token] -> Statement
varStatement cursor ty name tokens = case tokens of
  [] -> Statement cursor (SD.Var ty name Nothing)
  Token _ (TD.Op Op.Assign) : tokens' -> Statement (cursor |+| Token.foldCrs tokens) (SD.Var ty name (Just (expr tokens')))
  _ -> Statement (cursor |+| Token.foldCrs tokens) (SD.Invalid ("Invalid assignment : " ++ show tokens))

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
    cursor = Token.foldCrs tokens

exprNext :: Expr -> [Token] -> Expr
exprNext ex tokens = case tokens of
  [] -> ex
  [Token _ (TD.Op op)] | Op.isUnaryPost op -> Expr cursor (ED.UnopPost op ex)
  Token _ (TD.Op op) : tks | Op.isBinary op -> binop ex op (expr tks)
  Token _ (TD.DelimOpen Dl.SqBr) : tks -> binop ex Op.Subscript (expr (collectIndex tks))
  Token _ (TD.DelimOpen Dl.Pr) : tks -> Expr cursor (ED.Call ex (exprList (collectArguments tks)))
  _ -> Expr cursor (ED.Invalid ("Invalid follow expression for " ++ show ex ++ " : " ++ show tokens))
  where
    cursor = Token.foldCrs tokens

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

collectParameters :: [Token] -> (Either (Cursor, [(Type, Id)]) String, [Token])
collectParameters tokens = case tokens of
  [] -> (Left (Token.foldCrs tokens, []), [])
  _ ->
    let (tokens', rest) = collectUntilDelimiter Dl.Pr tokens
     in (makeList tokens', rest)
  where
    from = Token.crs $ head tokens
    makeList :: [Token] -> Either (Cursor, [(Type, Id)]) String
    makeList tokens'' = case tokens'' of
      [] -> Left (Token.foldCrs tokens'', [])
      Token _ (TD.Type ty) : Token to (TD.Id name) : Token _ (TD.Op Op.Comma) : rest -> next (from |+| to, (ty, name)) rest
      Token _ (TD.Type ty) : Token to (TD.Id name) : _ -> Left (from |+| to, [(ty, name)])
      _ -> Right ("Invalid parameters : " ++ show tokens'')
      where
        next :: (Cursor, (Type, Id)) -> [Token] -> Either (Cursor, [(Type, Id)]) String
        next (cursor, param) tks = case makeList tks of
          Left (cursor', params) -> Left (cursor |+| cursor', param : params)
          Right err -> Right err

func :: Cursor -> Type -> Id -> [Token] -> (Declaration, [Token])
func cursor ty name tokens =
  case rest of
    Token _ TD.Semicolon : rest' ->
      case mparameters of
        Left (cursor', parameters) -> (Declaration (cursor |+| cursor') (funcDef ty name parameters), rest')
        Right err -> (Declaration cursor (DD.Invalid err), rest')
    Token _ (TD.DelimOpen Dl.Br) : rest' ->
      let (body, rest'') = collectFuncBody rest'
       in case mparameters of
            Left (cursor', parameters) ->
              (Declaration (cursor |+| cursor') (funcDec ty name parameters body), rest'')
            Right err -> (Declaration cursor (DD.Invalid err), rest'')
    _ -> (Declaration cursor (DD.Invalid "Expected ; or {"), rest)
  where
    (mparameters, rest) = collectParameters tokens

funcDef :: Type -> Id -> [(Type, Id)] -> DeclarationDef
funcDef = DD.FuncDef

collectFuncBody :: [Token] -> ([Token], [Token])
collectFuncBody = collectUntilDelimiter Dl.Br

funcDec :: Type -> Id -> [(Type, Id)] -> [Token] -> DeclarationDef
funcDec ty name params body = DD.FuncDec ty name params (statementList body)

collectStructFields :: [Token] -> ([Token], [Token])
collectStructFields = collectUntilDelimiter Dl.Br

struct :: Cursor -> Maybe Id -> [Token] -> (Declaration, [Token])
struct cursor name tokens =
  let (tokens', rest') = collectStructFields tokens
   in case rest' of
        Token _ TD.Semicolon : rest'' -> case structFields tokens' of
          Left fields -> (Declaration cursor (DD.Struct name fields), rest'')
          Right err -> (Declaration cursor (DD.Invalid err), rest'')
        tk : rest'' -> (Declaration cursor (DD.Invalid ("Expected semicolon, got " ++ show tk)), rest'')
        [] -> (Declaration cursor (DD.Invalid "Expected semicolon"), rest')

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

enum :: Cursor -> Maybe Id -> Type -> [Token] -> (Declaration, [Token])
enum cursor name ty tokens =
  let (tokens', rest') = collectEnumVariants tokens
   in case rest' of
        Token _ TD.Semicolon : rest'' -> case enumVariants tokens' of
          Left variants -> (Declaration cursor (DD.Enum name ty variants), rest'')
          Right err -> (Declaration cursor (DD.Invalid err), rest'')
        tk : rest'' -> (Declaration cursor (DD.Invalid ("Expected semicolon, got " ++ show tk)), rest'')
        [] -> (Declaration cursor (DD.Invalid "Expected semicolon"), rest')

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