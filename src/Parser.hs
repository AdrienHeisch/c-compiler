module Parser (parse) where

import Constant (Constant (..))
import Control.Monad.State.Lazy (State, evalState, get, modify, runState)
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
import Utils (listToMaybeList, withSplit)

parse :: [Token] -> [Declaration]
parse tokens =
  let decls = evalState declarations (static tokens)
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

declarations :: State [Token] [Declaration]
declarations = do
  tokens <- get
  case map Token.def tokens of
    [] -> return []
    [TD.Eof] -> return []
    TD.Enum
      : TD.Id name
      : TD.Op Op.Colon
      : TD.Type ty
      : TD.DelimOpen Dl.Br
      : _ -> do
        go (enum (Just name) ty) 5
    TD.Enum
      : TD.Op Op.Colon
      : TD.Type ty
      : TD.DelimOpen Dl.Br
      : _ ->
        go (enum Nothing ty) 4
    TD.Enum
      : TD.Id name
      : TD.DelimOpen Dl.Br
      : _ ->
        go (enum (Just name) Ty.Int) 3
    TD.Enum
      : TD.DelimOpen Dl.Br
      : _ ->
        go (enum Nothing Ty.Int) 2
    TD.Type (Ty.Struct name)
      : TD.DelimOpen Dl.Br
      : _ ->
        go (struct name) 2
    TD.Type ty
      : TD.Id name
      : TD.DelimOpen Dl.Pr
      : _ ->
        go (func ty name) 3
    TD.NL
      : _ -> do
        modify $ drop 1
        declarations
    tkDef
      : _ -> do
        modify $ drop 1
        decls <- declarations
        return $ Declaration (DD.Invalid ("Unexpected token " ++ show tkDef)) (take 1 tokens) : decls
  where
    go :: ([Token] -> State [Token] Declaration) -> Int -> State [Token] [Declaration]
    go make skip = do
      tokens <- get
      let taken = take skip tokens
      modify $ drop skip
      decl <- make taken
      decls <- declarations
      return $ decl : decls

statementList :: State [Token] [Statement]
-- FIXME why NL and not ; ?
statementList = parseList TD.NL statement

statement :: State [Token] Statement
statement = do
  tokens <- get
  case map Token.def tokens of
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
    _ : _ -> do
      tokens' <- collectUntil TD.Semicolon
      let expression = expr tokens'
      return $ Statement (SD.Expr expression) (Expr.tks expression)
  where
    make :: ([Token] -> Statement) -> Int -> State [Token] Statement
    make st skip = do
      tokens <- get
      let taken = take skip tokens
      modify $ drop skip
      return $ st taken

    makeWith :: ([Token] -> State [Token] Statement) -> Int -> State [Token] Statement
    makeWith makeF skip = do
      tokens <- get
      let taken = take skip tokens
      modify $ drop skip
      makeF taken

if_ :: [Token] -> State [Token] Statement
if_ taken = do
  cond <- collectUntilDelimiter Dl.Pr
  then_ <- statement
  let taken' = taken ++ cond ++ St.tks then_
  tokens <- get
  case map Token.def tokens of
    TD.Else : _ -> do
      modify $ drop 1
      else_ <- statement
      return $ Statement (SD.If (expr cond) then_ (Just else_)) (taken' ++ take 1 tokens ++ St.tks else_)
    _ -> return $ Statement (SD.If (expr cond) then_ Nothing) taken'

switch :: [Token] -> State [Token] Statement
switch taken = do
  eval <- collectUntilDelimiter Dl.Pr
  body <- statement
  return $ Statement (SD.Switch (expr eval) body) (taken ++ eval ++ St.tks body)

case_ :: [Token] -> State [Token] Statement
case_ taken = do
  tokens <- get
  case map Token.def tokens of
    TD.IntLiteral constant@(Constant ty _)
      : TD.Op Op.Colon
      : _ -> do
        modify $ drop 2
        let stDef =
              if Ty.isInteger ty
                then SD.Case constant
                else SD.Invalid $ "Invalid type for case constant: " ++ show ty
        return $ Statement stDef (taken ++ take 2 tokens)
    _ -> do
      tks <- collectUntil (TD.Op Op.Colon)
      return $ Statement (SD.Invalid $ "Invalid case constant: " ++ show tks) (taken ++ tks)

while :: [Token] -> State [Token] Statement
while taken = do
  cond <- collectUntilDelimiter Dl.Pr
  body <- statement
  return $ Statement (SD.While (expr cond) body) (taken ++ cond ++ St.tks body)

doWhile :: [Token] -> State [Token] Statement
doWhile taken = do
  body <- statement
  tokens <- get
  case map Token.def tokens of
    TD.While : TD.DelimOpen Dl.Pr : _ -> do
      modify $ drop 2
      cond <- collectUntilDelimiter Dl.Pr
      tokens' <- get
      case map Token.def $ take 1 tokens' of
        TD.Semicolon : _ -> do
          modify $ drop 1
          return $ Statement (SD.DoWhile body $ expr cond) (taken ++ St.tks body ++ take 1 tokens ++ cond)
        _ -> return $ Statement (SD.Invalid "Expected semicolon") (taken ++ St.tks body ++ take 1 tokens ++ cond)
    TD.While : _ -> return $ Statement (SD.Invalid "Expected (") (taken ++ St.tks body ++ take 1 tokens)
    _ -> return $ Statement (SD.Invalid "Expected while (") (taken ++ St.tks body)

for :: [Token] -> State [Token] Statement
for taken = do
  decl <- collectUntil TD.Semicolon
  cond <- collectUntil TD.Semicolon
  incr <- collectUntilDelimiter Dl.Pr
  body <- statement
  stDef <- case map Token.def decl of
    TD.Type ty : TD.Id name : _ -> do
      modify $ drop 2
      declSt <- varStatement ty name (take 2 decl)
      return
        ( SD.ForVar
            declSt
            (expr <$> listToMaybeList cond)
            (expr <$> listToMaybeList incr)
            body
        )
    _ ->
      return
        ( SD.For
            (expr <$> listToMaybeList decl)
            (expr <$> listToMaybeList cond)
            (expr <$> listToMaybeList incr)
            body
        )
  return $ Statement stDef (taken ++ decl ++ cond ++ incr ++ St.tks body)

block :: [Token] -> State [Token] Statement
block taken = do
  tokens <- collectUntilDelimiter Dl.Br
  sts <- statementList
  return $ Statement (SD.Block sts) (taken ++ tokens)

return_ :: [Token] -> State [Token] Statement
return_ taken = do
  tokens <- collectUntil TD.Semicolon
  case tokens of
    [] -> return $ Statement (SD.Return Nothing) taken
    _ -> return $ Statement (SD.Return (Just (expr tokens))) (taken ++ tokens)

label :: Id -> [Token] -> State [Token] Statement
label name taken = do
  unfiltered <- get
  let tokens = Token.filterNL unfiltered
  case map Token.def tokens of
    TD.Id _ : TD.Op Op.Colon : _ -> return $ Statement (SD.Labeled name (Statement SD.Empty [tokens !! 1])) (taken ++ take 2 tokens)
    _ -> do
      st <- statement
      return $ Statement (SD.Labeled name st) tokens

varStatement :: Type -> Id -> [Token] -> State [Token] Statement
varStatement ty name taken = do
  statementTks <- collectUntil TD.Semicolon
  case map Token.def statementTks of
    [] -> return $ Statement (SD.Var ty name Nothing) taken
    TD.Op Op.Assign : _ -> return $ Statement (SD.Var ty name (Just $ expr $ drop 1 statementTks)) (taken ++ statementTks)
    _ -> return $ Statement (SD.Invalid ("Invalid assignment : " ++ show statementTks)) (taken ++ statementTks)

exprList :: [Token] -> [Expr]
exprList tokens = case map Token.def tokens of
  [] -> []
  TD.Op Op.Comma : _ -> exprList (drop 1 tokens)
  _ -> let (ex, tks) = runState (collectUntil (TD.Op Op.Comma)) tokens in expr ex : exprList tks

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
    let (pr, rest) = runState collectParenthese tks
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
collectArrayDecl = evalState (collectUntilDelimiter Dl.Br)

collectParenthese :: State [Token] [Token]
collectParenthese = collectUntilDelimiter Dl.Pr

collectArguments :: [Token] -> [Token]
collectArguments = evalState (collectUntilDelimiter Dl.Pr)

collectIndex :: [Token] -> [Token]
collectIndex = evalState (collectUntilDelimiter Dl.SqBr)

binop :: [Token] -> ExprDef -> Op -> Expr -> Expr
binop lTks leftDef op right = case right of
  Expr (ED.Binop r_left r_op r_right) rTks
    | Op.precedence op <= Op.precedence r_op ->
        Expr (ED.Binop (binop lTks leftDef op r_left) r_op r_right) (lTks ++ rTks)
  Expr _ rTks -> Expr (ED.Binop (Expr leftDef lTks) op right) (lTks ++ rTks)

collectParameters :: State [Token] (Either [(Type, Id)] String, [Token])
collectParameters = do
  tokens <- get
  case tokens of
    [] -> return (Left [], [])
    _ -> do
      tokens' <- collectUntilDelimiter Dl.Pr
      let (params, tokens'') = evalState makeList tokens'
       in return (params, tokens'')
  where
    makeList :: State [Token] (Either [(Type, Id)] String, [Token])
    makeList = do
      tokens <- get
      case map Token.def tokens of
        [] -> return (Left [], [])
        [TD.Type ty, TD.Id name] -> return (Left [(ty, name)], take 2 tokens)
        TD.Type ty : TD.Id name : TD.Op Op.Comma : _ -> do
          modify $ drop 3
          (mparams, tks) <- makeList
          case mparams of
            Left params -> return (Left $ (ty, name) : params, take 2 tokens ++ tks)
            Right err -> return (Right err, tks)
          -- withSplit tokens''' (next (ty, name)) 3
        TD.Type _ : TD.Id _ : tk : _ -> return (Right $ "Expected , or ) but got " ++ show tk, take 2 tokens)
        _ -> return (Right $ "Invalid parameters : " ++ show tokens, [])

func :: Type -> Id -> [Token] -> State [Token] Declaration
func ty name taken = do
  (parametersResult, paramTks) <- collectParameters
  tokens <- get
  case map Token.def tokens of
    TD.Semicolon : _ -> do
      modify $ drop 1
      case parametersResult of
        Left parameters -> return $ Declaration (funcDef ty name parameters) (taken ++ paramTks)
        Right err -> return $ Declaration (DD.Invalid err) (taken ++ paramTks)
    TD.DelimOpen Dl.Br : _ -> do
      modify $ drop 1
      body <- collectFuncBody
      case parametersResult of
        Left parameters -> return $ Declaration (funcDec ty name parameters body) (taken ++ paramTks ++ body)
        Right err -> return $ Declaration (DD.Invalid err) (taken ++ paramTks ++ body)
    _ -> return $ Declaration (DD.Invalid "Expected ; or {") (taken ++ paramTks)

funcDef :: Type -> Id -> [(Type, Id)] -> DeclarationDef
funcDef = DD.FuncDef

collectFuncBody :: State [Token] [Token]
collectFuncBody = collectUntilDelimiter Dl.Br

funcDec :: Type -> Id -> [(Type, Id)] -> [Token] -> DeclarationDef
funcDec ty name params body = DD.FuncDec ty name params $ evalState statementList body

collectStructFields :: State [Token] [Token]
collectStructFields = collectUntilDelimiter Dl.Br

struct :: Maybe Id -> [Token] -> State [Token] Declaration
struct name taken = do
  structTks <- collectStructFields
  tokens <- get
  def <- case map Token.def tokens of
    TD.Semicolon : _ -> do
      modify $ drop 1
      case structFields structTks of
        Left fields -> return $ DD.Struct name fields
        Right err -> return $ DD.Invalid err
    [] -> return $ DD.Invalid "Expected semicolon"
    tk : _ -> return $ DD.Invalid ("Expected semicolon, got " ++ show tk)
  return $ Declaration def (taken ++ structTks)

structFields :: [Token] -> Either [(Type, Id)] String
structFields tokens = case Token.filterNL tokens of
  [] -> Left []
  tokens' ->
    let (field, rest) = runState (collectUntil TD.Semicolon) tokens'
     in case map Token.def field of
          [TD.Type ty, TD.Id name] -> next (ty, name) rest
          tk : _ -> Right $ "Unexpected token : " ++ show tk
          [] -> Right "Empty field"
  where
    next field tks = case structFields tks of
      Left fields -> Left (field : fields)
      Right err -> Right err

enum :: Maybe Id -> Type -> [Token] -> State [Token] Declaration
enum name ty taken = do
  enumTks <- collectEnumVariants
  tokens <- get
  def <- case map Token.def tokens of
    TD.Semicolon : _ -> do
      modify $ drop 1
      case enumVariants enumTks of
        Left variants -> return $ DD.Enum name ty variants
        Right err -> return $ DD.Invalid err
    [] -> return $ DD.Invalid "Expected semicolon"
    tk : _ -> return $ DD.Invalid ("Expected semicolon, got " ++ show tk)
  return $ Declaration def (taken ++ enumTks)

collectEnumVariants :: State [Token] [Token]
collectEnumVariants = collectUntilDelimiter Dl.Br

enumVariants :: [Token] -> Either [(Id, Maybe Expr)] String
enumVariants tokens = case Token.filterNL tokens of
  [] -> Left []
  tokens' ->
    let (variant, rest) = runState (collectUntil (TD.Op Op.Comma)) tokens'
     in case map Token.def variant of
          TD.Id name : TD.Op Op.Assign : _ -> next (name, Just $ expr (drop 2 variant)) rest
          [TD.Id name] -> next (name, Nothing) rest
          tk : _ -> Right $ "Unexpected token : " ++ show tk
          [] -> Right "Empty field"
  where
    next variant tks = case enumVariants tks of
      Left variants -> Left (variant : variants)
      Right err -> Right err