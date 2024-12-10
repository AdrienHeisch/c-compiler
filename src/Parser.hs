module Parser (parse) where

import Constant (Constant (..))
import Cursor (CursorOps (..))
import Data.List (intercalate)
import Declaration (Declaration (Declaration), DeclarationDef)
import Declaration qualified (errs)
import Declaration qualified as DD (DeclarationDef (..))
import Delimiter qualified as Dl
import Expr (Expr (Expr), ExprDef)
import Expr qualified (Expr (..))
import Expr qualified as ED (ExprDef (..))
import Identifier (Id)
import Op (Op)
import Op qualified
import Statement (Statement (Statement))
import Statement qualified as SD (StatementDef (..))
import Statement qualified as St (Statement (..))
import Token (Token (Token), collectUntil, collectUntilDelimiter, parseList)
import Token qualified (Token (..), filterNL)
import Token qualified as TD (TokenDef (..))
import Type (Type)
import Type qualified as Ty
import Utils (listToMaybeList, withSplit, withSplitTpl)

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
  Token TD.Signed cl
    : Token (TD.Type ty) cr
    : tks ->
      static (Token (TD.Type (Ty.signed ty)) (cl |+| cr) : tks)
  Token TD.Unsigned cl
    : Token (TD.Type ty) cr
    : tks ->
      static (Token (TD.Type (Ty.unsigned ty)) (cl |+| cr) : tks)
  Token (TD.Type ty) cl
    : Token (TD.Op Op.MultOrIndir) cr
    : tks ->
      static (Token (TD.Type (Ty.Pointer ty)) (cl |+| cr) : tks)
  Token TD.Struct cl
    : Token (TD.Id name) cr
    : tks ->
      static (Token (TD.Type (Ty.Struct (Just name))) (cl |+| cr) : tks)
  Token TD.Struct cursor
    : tks ->
      static (Token (TD.Type (Ty.Struct Nothing)) cursor : tks)
  Token (TD.StrLiteral (Constant (Ty.Array Ty.Char lenl) strl)) cl
    : Token (TD.StrLiteral (Constant (Ty.Array Ty.Char lenr) strr)) cr
    : tks ->
      static (Token (TD.StrLiteral (Constant (Ty.Array Ty.Char (lenl + lenr)) (strl ++ strr))) (cl |+| cr) : tks)
  (tk : tks) -> tk : static tks

declarations :: [Token] -> [Declaration]
declarations tokens = case map Token.def tokens of
  [] -> []
  [TD.Eof] -> []
  TD.Enum
    : TD.Id name
    : TD.Op Op.Colon
    : TD.Type ty
    : TD.DelimOpen Dl.Br
    : _ ->
      put $ makeWith (enum (Just name) ty) 5
  TD.Enum
    : TD.Op Op.Colon
    : TD.Type ty
    : TD.DelimOpen Dl.Br
    : _ ->
      put $ makeWith (enum Nothing ty) 4
  TD.Enum
    : TD.Id name
    : TD.DelimOpen Dl.Br
    : _ ->
      put $ makeWith (enum (Just name) Ty.Int) 3
  TD.Enum
    : TD.DelimOpen Dl.Br
    : _ ->
      put $ makeWith (enum Nothing Ty.Int) 2
  TD.Type (Ty.Struct name)
    : TD.DelimOpen Dl.Br
    : _ ->
      put $ makeWith (struct name) 2
  TD.Type ty
    : TD.Id name
    : TD.DelimOpen Dl.Pr
    : _ ->
      put $ makeWith (func ty name) 3
  TD.NL
    : _ ->
      declarations (drop 1 tokens)
  tkDef
    : _ ->
      Declaration (DD.Invalid ("Unexpected token " ++ show tkDef)) (take 1 tokens) : declarations (drop 1 tokens)
  where
    put :: (Declaration, [Token]) -> [Declaration]
    put tuple =
      let (declaration, tks) = tuple
       in declaration : declarations tks

    makeWith = withSplit tokens

statementList :: [Token] -> [Statement]
-- FIXME why NL ?
statementList = parseList TD.NL statement

statement :: [Token] -> (Statement, [Token])
statement tokens = case map Token.def tokens of
  [] ->
    make (Statement SD.Empty) 0
  TD.NL
    : _ ->
      make (Statement SD.Empty) 1
  TD.Semicolon
    : _ ->
      make (Statement SD.Empty) 1
  TD.If
    : TD.DelimOpen Dl.Pr
    : _ ->
      makeWith if_ 1
  TD.Switch
    : TD.DelimOpen Dl.Pr
    : _ ->
      makeWith switch 1
  TD.While
    : TD.DelimOpen Dl.Pr
    : _ ->
      makeWith while 1
  TD.Do
    : _ ->
      makeWith doWhile 1
  TD.For
    : TD.DelimOpen Dl.Pr
    : _ ->
      makeWith for 2
  TD.DelimOpen Dl.Br
    : _ ->
      makeWith block 1
  TD.Return
    : _ ->
      makeWith return_ 1
  TD.Goto
    : TD.Id name
    : TD.Semicolon
    : _ ->
      make (Statement (SD.Goto name)) 3
  TD.Break
    : TD.Semicolon
    : _ ->
      make (Statement SD.Break) 3
  TD.Continue
    : TD.Semicolon
    : _ ->
      make (Statement SD.Continue) 3
  TD.Case
    : _ ->
      makeWith case_ 1
  TD.Id name
    : TD.Op Op.Colon
    : _ ->
      makeWith (label name) 2
  TD.Type ty
    : TD.Id name
    : TD.DelimOpen Dl.SqBr
    : TD.IntLiteral (Constant len_ty len)
    : TD.DelimClose Dl.SqBr
    : _
      | Ty.isInteger len_ty ->
          makeWith (varStatement (Ty.Array ty len) name) 5
  TD.Type ty
    : TD.Id name
    : TD.DelimOpen Dl.SqBr
    : TD.DelimClose Dl.SqBr
    : _ ->
      makeWith (varStatement (Ty.ArrayNoHint ty) name) 4
  TD.Type ty
    : TD.Id name
    : _ ->
      makeWith (varStatement ty name) 2
  _ ->
    let (tokens', rest') = collectUntil TD.Semicolon tokens
        expression = expr tokens'
     in (Statement (SD.Expr expression) (Expr.tks expression), rest')
  where
    make = withSplitTpl tokens
    makeWith = withSplit tokens

if_ :: [Token] -> [Token] -> (Statement, [Token])
if_ taken tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (then_, rest'') = statement rest'
      taken' = taken ++ cond ++ St.tks then_
   in case map Token.def rest'' of
        TD.Else : _ ->
          let (else_, rest''') = statement (drop 1 rest'')
           in (Statement (SD.If (expr cond) then_ (Just else_)) (taken' ++ take 1 rest'' ++ St.tks else_), rest''')
        _ -> (Statement (SD.If (expr cond) then_ Nothing) taken', rest'')

switch :: [Token] -> [Token] -> (Statement, [Token])
switch taken tokens =
  let (eval, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (Statement (SD.Switch (expr eval) body) (taken ++ eval ++ St.tks body), rest'')

case_ :: [Token] -> [Token] -> (Statement, [Token])
case_ taken tokens = case map Token.def tokens of
  TD.IntLiteral constant@(Constant ty _)
    : TD.Op Op.Colon
    : _ ->
      let stDef =
            if Ty.isInteger ty
              then SD.Case constant
              else SD.Invalid $ "Invalid type for case constant: " ++ show ty
       in (Statement stDef (taken ++ take 2 tokens), drop 2 tokens)
  _ ->
    let (tks, rest) = collectUntil (TD.Op Op.Colon) tokens
     in (Statement (SD.Invalid $ "Invalid case constant: " ++ show tks) (taken ++ tks), rest)

while :: [Token] -> [Token] -> (Statement, [Token])
while taken tokens =
  let (cond, rest') = collectUntilDelimiter Dl.Pr tokens
      (body, rest'') = statement rest'
   in (Statement (SD.While (expr cond) body) (taken ++ cond ++ St.tks body), rest'')

doWhile :: [Token] -> [Token] -> (Statement, [Token])
doWhile taken tokens =
  let (body, rest) = statement tokens
   in case map Token.def rest of
        TD.While : ((TD.DelimOpen Dl.Pr)) : _ ->
          let (cond, rest') = collectUntilDelimiter Dl.Pr (drop 2 rest)
           in case Token.def $ head rest' of
                TD.Semicolon ->
                  (Statement (SD.DoWhile body $ expr cond) (taken ++ St.tks body ++ take 1 rest ++ cond), drop 1 rest')
                _ ->
                  (Statement (SD.Invalid "Expected semicolon") (taken ++ St.tks body ++ take 1 rest ++ cond), rest)
        TD.While : _ -> (Statement (SD.Invalid "Expected (") (taken ++ St.tks body ++ take 1 rest), drop 1 rest)
        _ -> (Statement (SD.Invalid "Expected while (") (taken ++ St.tks body), rest)

for :: [Token] -> [Token] -> (Statement, [Token])
for taken tokens =
  let (decl, rest') = collectUntil TD.Semicolon tokens
      (cond, rest'') = collectUntil TD.Semicolon rest'
      (incr, rest''') = collectUntilDelimiter Dl.Pr rest''
      (body, rest'''') = statement rest'''
      (stDef, rest''''') = case map Token.def decl of
        TD.Type ty : TD.Id name : _ ->
          let (declSt, rest'''''') = varStatement ty name (take 2 decl) (drop 2 decl)
           in ( SD.ForVar
                  declSt
                  (expr <$> listToMaybeList cond)
                  (expr <$> listToMaybeList incr)
                  body,
                rest'''' ++ rest''''''
              )
        _ ->
          ( SD.For
              (expr <$> listToMaybeList decl)
              (expr <$> listToMaybeList cond)
              (expr <$> listToMaybeList incr)
              body,
            rest''''
          )
   in ( Statement stDef (taken ++ decl ++ cond ++ incr ++ St.tks body),
        rest'''''
      )

block :: [Token] -> [Token] -> (Statement, [Token])
block taken tokens =
  let (tokens', rest') = collectUntilDelimiter Dl.Br tokens
      sts = statementList tokens'
   in (Statement (SD.Block sts) (taken ++ tokens'), rest')

return_ :: [Token] -> [Token] -> (Statement, [Token])
return_ taken tokens =
  case collectUntil TD.Semicolon tokens of
    ([], rest') -> (Statement (SD.Return Nothing) taken, rest')
    (tokens', rest') -> (Statement (SD.Return (Just (expr tokens'))) (taken ++ tokens'), rest')

label :: Id -> [Token] -> [Token] -> (Statement, [Token])
label name taken tokens_ = case map Token.def tokens of
  TD.Id _ : TD.Op Op.Colon : _ -> (Statement (SD.Labeled name (Statement SD.Empty [tokens !! 1])) (taken ++ take 2 tokens), tokens_)
  _ ->
    let (st, rest') = statement tokens
     in (Statement (SD.Labeled name st) tokens, rest')
  where
    tokens = Token.filterNL tokens_

varStatement :: Type -> Id -> [Token] -> [Token] -> (Statement, [Token])
varStatement ty name taken tokens_ =
  ( case map Token.def tokens of
      [] -> Statement (SD.Var ty name Nothing) taken
      TD.Op Op.Assign : _ -> Statement (SD.Var ty name (Just $ expr $ drop 1 tokens)) (taken ++ tokens)
      _ -> Statement (SD.Invalid ("Invalid assignment : " ++ show tokens)) (taken ++ tokens),
    rest
  )
  where
    (tokens, rest) = collectUntil TD.Semicolon tokens_

exprList :: [Token] -> [Expr]
exprList tokens = case map Token.def tokens of
  [] -> []
  TD.Op Op.Comma : _ -> exprList (drop 1 tokens)
  _ -> let (ex, tks) = collectUntil (TD.Op Op.Comma) tokens in expr ex : exprList tks

expr :: [Token] -> Expr
expr tokens = case map Token.def tokens of
  [] ->
    Expr (ED.Invalid "Empty expression") tokens
  TD.NL : _ ->
    expr tks
  TD.IntLiteral constant : _
    | Ty.isInteger $ Constant.ty constant ->
        exprNext tk (ED.IntLiteral constant) tks
  TD.FltLiteral constant : _
    | Ty.isFloating $ Constant.ty constant ->
        exprNext tk (ED.FltLiteral constant) tks
  TD.StrLiteral constant@(Constant (Ty.Array Ty.Char _) _) : _ ->
    exprNext tk (ED.StrLiteral constant) tks
  TD.Id identifier : _ ->
    exprNext tk (ED.Id identifier) tks
  TD.Op op : _
    | Op.isUnaryPre op ->
        let ex = expr tks
         in Expr (ED.UnopPre op ex) (tk ++ Expr.tks ex)
  TD.DelimOpen Dl.Br : _ ->
    let exs = exprList (collectArrayDecl tks)
     in Expr (ED.ArrayDecl exs) (tk ++ concatMap Expr.tks exs)
  TD.DelimOpen Dl.Pr : _ ->
    let (pr, rest) = collectParenthese tks
     in exprNext (tk ++ pr) (ED.Parenthese (expr pr)) rest
  _ ->
    Expr (ED.Invalid ("Invalid expression : " ++ show tokens)) tokens
  where
    (tk, tks) = splitAt 1 tokens

exprNext :: [Token] -> ExprDef -> [Token] -> Expr
exprNext taken ex tokens = case map Token.def tokens of
  [] -> Expr ex taken
  [TD.Op op] | Op.isUnaryPost op -> Expr (ED.UnopPost op (Expr ex taken)) tks
  (TD.Op op) : _ | Op.isBinary op -> binop tks ex op (expr (tail tokens))
  (TD.DelimOpen Dl.SqBr) : _ -> binop tks ex Op.Subscript (expr (collectIndex (tail tokens)))
  (TD.DelimOpen Dl.Pr) : _ ->
    let args = collectArguments (tail tokens)
     in Expr (ED.Call (Expr ex taken) (exprList args)) (taken ++ take (1 + length args) tokens)
  _ -> Expr (ED.Invalid ("Invalid follow expression for " ++ show ex ++ " : " ++ show tokens)) tokens
  where
    tks = taken ++ take 1 tokens

collectArrayDecl :: [Token] -> [Token]
collectArrayDecl tokens = let (tokens', _) = collectUntilDelimiter Dl.Br tokens in tokens'

collectParenthese :: [Token] -> ([Token], [Token])
collectParenthese tokens = let (pr, rest) = collectUntilDelimiter Dl.Pr tokens in (pr, rest)

collectArguments :: [Token] -> [Token]
collectArguments tokens = let (tokens', _) = collectUntilDelimiter Dl.Pr tokens in tokens'

collectIndex :: [Token] -> [Token]
collectIndex tokens = let (tokens', _) = collectUntilDelimiter Dl.SqBr tokens in tokens'

binop :: [Token] -> ExprDef -> Op -> Expr -> Expr
binop lTks leftDef op right = case right of
  Expr (ED.Binop r_left r_op r_right) rTks
    | Op.precedence op <= Op.precedence r_op ->
        Expr (ED.Binop (binop lTks leftDef op r_left) r_op r_right) (lTks ++ rTks)
  Expr _ rTks -> Expr (ED.Binop (Expr leftDef lTks) op right) (lTks ++ rTks)

collectParameters :: [Token] -> (Either [(Type, Id)] String, [Token], [Token])
collectParameters tokens = case tokens of
  [] -> (Left [], [], [])
  _ ->
    let (tokens', rest) = collectUntilDelimiter Dl.Pr tokens
        (params, tokens'') = makeList tokens'
     in (params, tokens'', rest)
  where
    makeList :: [Token] -> (Either [(Type, Id)] String, [Token])
    makeList tokens''' = case map Token.def tokens''' of
      [] -> (Left [], [])
      [TD.Type ty, TD.Id name] -> (Left [(ty, name)], take 2 tokens''')
      TD.Type ty : TD.Id name : TD.Op Op.Comma : _ -> withSplit tokens''' (next (ty, name)) 3
      TD.Type _ : TD.Id _ : tk : _ -> (Right $ "Expected , or ) but got " ++ show tk, take 2 tokens''')
      _ -> (Right $ "Invalid parameters : " ++ show tokens''', [])

    next :: (Type, Id) -> [Token] -> [Token] -> (Either [(Type, Id)] String, [Token])
    next param taken tokens''' = case makeList tokens''' of
      (Left params, tokens'''') -> (Left (param : params), taken ++ tokens'''')
      (Right err, tokens'''') -> (Right err, tokens'''')

func :: Type -> Id -> [Token] -> [Token] -> (Declaration, [Token])
func ty name taken tokens =
  case map Token.def rest of
    TD.Semicolon : _ ->
      case parametersResult of
        Left parameters -> (Declaration (funcDef ty name parameters) (taken ++ paramTks ++ take 1 rest), drop 1 rest)
        Right err -> (Declaration (DD.Invalid err) (taken ++ paramTks ++ take 1 rest), drop 1 rest)
    TD.DelimOpen Dl.Br : _ ->
      let (body, rest'') = collectFuncBody (drop 1 rest)
       in case parametersResult of
            Left parameters ->
              (Declaration (funcDec ty name parameters body) (taken ++ paramTks ++ body), rest'')
            Right err -> (Declaration (DD.Invalid err) (taken ++ paramTks), rest'')
    _ -> (Declaration (DD.Invalid "Expected ; or {") (taken ++ paramTks), rest)
  where
    (parametersResult, paramTks, rest) = collectParameters tokens

funcDef :: Type -> Id -> [(Type, Id)] -> DeclarationDef
funcDef = DD.FuncDef

collectFuncBody :: [Token] -> ([Token], [Token])
collectFuncBody = collectUntilDelimiter Dl.Br

funcDec :: Type -> Id -> [(Type, Id)] -> [Token] -> DeclarationDef
funcDec ty name params body = DD.FuncDec ty name params (statementList body)

collectStructFields :: [Token] -> ([Token], [Token])
collectStructFields = collectUntilDelimiter Dl.Br

struct :: Maybe Id -> [Token] -> [Token] -> (Declaration, [Token])
struct name taken tokens =
  let (tokens', rest') = collectStructFields tokens
   in case map Token.def rest' of
        TD.Semicolon : _ -> case structFields tokens' of
          Left fields -> (Declaration (DD.Struct name fields) (taken ++ tokens' ++ take 1 rest'), drop 1 rest')
          Right err -> (Declaration (DD.Invalid err) (taken ++ tokens' ++ take 1 rest'), drop 1 rest')
        [] -> (Declaration (DD.Invalid "Expected semicolon") (taken ++ tokens'), rest')
        tk : _ -> (Declaration (DD.Invalid ("Expected semicolon, got " ++ show tk)) (taken ++ tokens'), rest')

structFields :: [Token] -> Either [(Type, Id)] String
structFields tokens = case Token.filterNL tokens of
  [] -> Left []
  tokens' ->
    let (field, rest) = collectUntil TD.Semicolon tokens'
     in case map Token.def field of
          [TD.Type ty, TD.Id name] -> next (ty, name) rest
          tk : _ -> Right $ "Unexpected token : " ++ show tk
          [] -> Right "Empty field"
  where
    next field tks = case structFields tks of
      Left fields -> Left (field : fields)
      Right err -> Right err

enum :: Maybe Id -> Type -> [Token] -> [Token] -> (Declaration, [Token])
enum name ty taken tokens =
  let (tokens', rest') = collectEnumVariants tokens
   in case map Token.def rest' of
        TD.Semicolon : _ -> case enumVariants tokens' of
          Left variants -> (Declaration (DD.Enum name ty variants) (taken ++ tokens' ++ take 1 rest'), drop 1 rest')
          Right err -> (Declaration (DD.Invalid err) (taken ++ tokens' ++ take 1 rest'), drop 1 rest')
        [] -> (Declaration (DD.Invalid "Expected semicolon") (taken ++ tokens'), rest')
        tk : _ -> (Declaration (DD.Invalid ("Expected semicolon, got " ++ show tk)) (taken ++ tokens'), rest')

collectEnumVariants :: [Token] -> ([Token], [Token])
collectEnumVariants = collectUntilDelimiter Dl.Br

enumVariants :: [Token] -> Either [(Id, Maybe Expr)] String
enumVariants tokens = case Token.filterNL tokens of
  [] -> Left []
  tokens' ->
    let (variant, rest) = collectUntil (TD.Op Op.Comma) tokens'
     in case map Token.def variant of
          TD.Id name : TD.Op Op.Assign : _ -> next (name, Just $ expr (drop 2 variant)) rest
          [TD.Id name] -> next (name, Nothing) rest
          tk : _ -> Right $ "Unexpected token : " ++ show tk
          [] -> Right "Empty field"
  where
    next variant tks = case enumVariants tks of
      Left variants -> Left (variant : variants)
      Right err -> Right err