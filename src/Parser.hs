module Parser (parse) where

import Constant (Constant (..))
import Control.Monad.State.Lazy (State, evalState, get, modify, runState)
import Cursor (CursorOps (..))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust, isNothing)
import Delimiter qualified as Dl
import Expr (Expr (Expr), ExprDef)
import Expr qualified (Expr (..))
import Expr qualified as ED (ExprDef (..))
import Identifier (Id)
import Op (Op)
import Op qualified
import Statement (Statement (Statement), StatementDef)
import Statement qualified (errs, isTopLevel)
import Statement qualified as SD (StatementDef (..))
import Statement qualified as St (Statement (..))
import Token (Token (Token), collectUntil, collectUntilDelimiter, parseListWithInner)
import Token qualified (Token (..), filterNL, foldCrs)
import Token qualified as TD (TokenDef (..))
import Type (Type)
import Type qualified as Ty
import Utils (Display (display), listToMaybeList)

parse :: [Token] -> [Statement]
parse tokens =
  let filtered = Token.filterNL tokens
      decls = evalState statementList (static filtered)
      !_ = case Statement.errs decls of
        [] -> ()
        tkErrs -> error $ "Parser errors :\n" ++ intercalate "\n" tkErrs
      !_ = case topLevelCheck decls of
        [] -> ()
        errs -> error $ "Parser errors :\n" ++ intercalate "\n" errs
   in decls
  where
    topLevelCheck :: [Statement] -> [String]
    topLevelCheck decls = case map St.def decls of
      [] -> []
      def : _ | not $ Statement.isTopLevel def -> (display (head decls) ++ " at " ++ show (Token.foldCrs $ St.tks $ head decls) ++ " not allowed in top-level") : topLevelCheck (tail decls)
      _ -> topLevelCheck (tail decls)

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
  Token (TD.StrLiteral (Constant (Ty.Array Ty.Char lenl) strl)) cl
    : Token (TD.StrLiteral (Constant (Ty.Array Ty.Char lenr) strr)) cr
    : tks ->
      static (Token (TD.StrLiteral (Constant (Ty.Array Ty.Char (lenl + lenr)) (strl ++ strr))) (cl |+| cr) : tks)
  (tk : tks) -> tk : static tks

statementList :: State [Token] [Statement]
statementList = parseListWithInner TD.Semicolon Dl.Br statement

statement :: State [Token] Statement
statement = do
  tokens <- get
  case map Token.def tokens of
    [] ->
      make (Statement SD.Empty) 0
    [TD.Eof] ->
      make (Statement SD.Empty) 1
    TD.Typedef
      : _ -> makeWith typedef 1
    TD.Enum
      : TD.Id name
      : TD.Op Op.Colon
      : TD.Type ty
      : TD.DelimOpen Dl.Br
      : _ -> do
        makeWith (enum (Just name) ty) 5
    TD.Enum
      : TD.Op Op.Colon
      : TD.Type ty
      : TD.DelimOpen Dl.Br
      : _ ->
        makeWith (enum Nothing ty) 4
    TD.Enum
      : TD.Id name
      : TD.DelimOpen Dl.Br
      : _ ->
        makeWith (enum (Just name) Ty.Int) 3
    TD.Enum
      : TD.DelimOpen Dl.Br
      : _ ->
        makeWith (enum Nothing Ty.Int) 2
    TD.Struct
      : _ ->
        makeWith struct 1
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
      : TD.DelimOpen Dl.Pr
      : _ ->
        makeWith (func ty name) 3
    TD.Type ty
      : _ ->
        makeWith (declaration ty) 1
    TD.Id tyid
      : TD.Id _
      : _ ->
        makeWith (declaration (Ty.Typedef tyid)) 1
    _ ->
      makeExpr
  where
    make :: ([Token] -> Statement) -> Int -> State [Token] Statement
    make st skip = do
      tokens <- get
      modify $ drop skip
      return . st $ take skip tokens

    makeWith :: ([Token] -> State [Token] Statement) -> Int -> State [Token] Statement
    makeWith makeF skip = do
      tokens <- get
      modify $ drop skip
      makeF $ take skip tokens

    makeExpr :: State [Token] Statement
    makeExpr = do
      tokens <- collectUntil TD.Semicolon
      let ex = expr tokens
      return $ Statement (SD.Expr ex) (Expr.tks ex)

declaration :: Type -> [Token] -> State [Token] Statement
declaration spec taken = do
  (mdecls, tokens) <- go
  case mdecls of
    Left [] -> return $ Statement (SD.Invalid "Empty declaration") (taken ++ tokens)
    Left (decl : decls) -> return $ Statement (SD.Var (decl :| decls)) (taken ++ tokens)
    Right err -> return $ Statement (SD.Invalid err) (taken ++ tokens)
  where
    go :: State [Token] (Either [(Type, Id, Maybe Expr)] String, [Token])
    go = do
      (res, tks) <- makeType spec
      case res of
        Right err -> return (Right err, tks)
        Left (_, Nothing) -> return (Right "Expected variable identifier", tks)
        Left (ty, Just name) -> do
          ex <- assign
          tokens <- get
          case Token.def $ head tokens of
            TD.Op Op.Comma -> do
              modify $ drop 1
              (mdecls, dTks) <- go
              case mdecls of
                Right err -> return (Right err, tks)
                Left decls -> return (Left ((ty, name, ex) : decls), tks ++ dTks)
            _ -> return (Left [(ty, name, ex)], tks)

getType :: State [Token] (Either (Type, Maybe Id) String, [Token])
getType = do
  tokens <- get
  case Token.def $ head tokens of
    TD.Type ty -> do
      modify $ drop 1
      makeType ty
    TD.Id name -> do
      modify $ drop 1
      makeType (Ty.Typedef name)
    TD.Struct -> do
      modify $ drop 1
      (mty, tks) <- structType
      case mty of
        Right err -> return (Right err, tks)
        Left ty -> makeType ty
    tk -> return (Right $ "Expected type, got " ++ show tk, take 1 tokens)

makeType :: Type -> State [Token] (Either (Type, Maybe Id) String, [Token])
makeType spec = go spec Nothing False
  where
    go :: Type -> Maybe Id -> Bool -> State [Token] (Either (Type, Maybe Id) String, [Token])
    go ty name pr = do
      tokens <- get
      case Token.def $ head tokens of
        TD.DelimClose Dl.Pr
          | pr -> do
              modify $ drop 1
              thenGo ty name
        _ | isJust name -> do
          thenGo ty name
        TD.Id name' | isNothing name -> do
          modify $ drop 1
          go ty (Just name') pr
        TD.Op Op.MultOrIndir -> do
          modify $ drop 1
          go (Ty.Pointer ty) Nothing pr
        TD.DelimOpen Dl.Pr
          | not pr -> do
              modify $ drop 1
              go (Ty.Pointer ty) Nothing True
        tk | pr -> return (Right $ "Unexpected token " ++ show tk, take 1 tokens)
        _ -> thenGo ty name

    thenGo :: Type -> Maybe Id -> State [Token] (Either (Type, Maybe Id) String, [Token])
    thenGo ty name = do
      tokens <- get
      case Token.def $ head tokens of
        TD.DelimOpen Dl.SqBr -> do
          modify $ drop 1
          lenTks <- collectUntilDelimiter Dl.SqBr
          if null lenTks
            then return (Left (Ty.ArrayNoHint ty, name), take 1 tokens)
            else
              let ex = expr lenTks
               in case Expr.def ex of
                    ED.IntLiteral (Constant len_ty len)
                      | Ty.isInteger len_ty ->
                          return (Left (Ty.Array ty len, name), take 1 tokens)
                    _ -> return (Right $ "Invalid array size : " ++ show lenTks, lenTks)
        TD.DelimOpen Dl.Pr -> do
          tys <- parseListWithInner (TD.Op Op.Comma) Dl.Pr getType
          case params tys of
            Left ps -> return (Left (Ty.FunctionPointer ty ps, name), take 1 tokens)
            Right (err, errTks) -> return (Right err, errTks)
        _ -> return (Left (ty, name), take 1 tokens)

    params :: [(Either (Type, Maybe Id) String, [Token])] -> Either [Type] (String, [Token])
    params tyList = case tyList of
      [] -> Left []
      (Left (ty, Nothing), _) : rest ->
        case params rest of
          Left tys -> Left (ty : tys)
          Right (err, errTks) -> Right (err, errTks)
      (Left (_, Just name), tks) : _ -> Right ("Unexpected identifier : " ++ show name, tks)
      (Right err, tks) : _ -> Right (err, tks)

assign :: State [Token] (Maybe Expr)
assign = do
  tokens <- get
  case map Token.def tokens of
    TD.Op Op.Assign : _ -> do
      modify $ drop 1
      return $ Just . expr $ drop 1 tokens
    _ -> return Nothing

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
  tokens <- get
  case map Token.def tokens of
    TD.DelimOpen Dl.Pr : _ -> do
      modify $ drop 1
      cond <- collectUntilDelimiter Dl.Pr
      body <- statement
      return $ Statement (SD.While (expr cond) body) (taken ++ cond ++ St.tks body)
    _ -> return $ Statement (SD.Invalid "Expected (") taken

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
  let sts = evalState statementList tokens
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
    [] -> return $ Statement (SD.Var ((ty, name, Nothing) :| [])) taken
    TD.Op Op.Assign : _ -> return $ Statement (SD.Var ((ty, name, Just $ expr $ drop 1 statementTks) :| [])) (taken ++ statementTks)
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
    Expr (ED.Invalid ("Invalid expression : " ++ display tokens)) tokens
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
  _ -> Expr (ED.Invalid ("Invalid follow expression for " ++ display ex ++ " : " ++ display tokens)) tokens
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

typedef :: [Token] -> State [Token] Statement
typedef taken = do
  (mty, tokens) <- getType
  case mty of
    Left (ty, Just name) -> return $ Statement (SD.Typedef ty name) (taken ++ tokens)
    Left (_, Nothing) -> return $ Statement (SD.Invalid "Expected typedef name") (taken ++ tokens)
    Right err -> return $ Statement (SD.Invalid err) (taken ++ tokens)

collectParameters :: State [Token] (Either [(Type, Maybe Id)] String, [Token])
collectParameters = do
  tokens <- get
  case tokens of
    [] -> return (Left [], [])
    _ -> do
      mparams <- parseListWithInner (TD.Op Op.Comma) Dl.Pr getType
      case parameters mparams of
        Right (err, errTks) -> return (Right err, errTks)
        Left params -> return (Left params, tokens)
  where
    parameters :: [(Either (Type, Maybe Id) String, [Token])] -> Either [(Type, Maybe Id)] (String, [Token])
    parameters tyList = case tyList of
      [] -> Left []
      (Left (ty, name), _) : rest -> 
        case parameters rest of
          Left tys -> Left ((ty, name) : tys)
          Right (err, errTks) -> Right (err, errTks)
      (Right err, tks) : _ -> Right (err, tks)

func :: Type -> Id -> [Token] -> State [Token] Statement
func ty name taken = do
  (parametersResult, paramTks) <- collectParameters
  tokens <- get
  case map Token.def tokens of
    TD.Semicolon : _ -> do
      modify $ drop 1
      case parametersResult of
        Left params -> return $ Statement (funcDef ty name params) (taken ++ paramTks)
        Right err -> return $ Statement (SD.Invalid err) (taken ++ paramTks)
    TD.DelimOpen Dl.Br : _ -> do
      modify $ drop 1
      body <- collectFuncBody
      case parametersResult of
        Left params -> return $ Statement (funcDec ty name params body) (taken ++ paramTks ++ body)
        Right err -> return $ Statement (SD.Invalid err) (taken ++ paramTks ++ body)
    _ -> return $ Statement (SD.Invalid "Expected ; or {") (taken ++ paramTks)

funcDef :: Type -> Id -> [(Type, Maybe Id)] -> StatementDef
funcDef = SD.FuncDef

collectFuncBody :: State [Token] [Token]
collectFuncBody = collectUntilDelimiter Dl.Br

funcDec :: Type -> Id -> [(Type, Maybe Id)] -> [Token] -> StatementDef
funcDec ty name params body = SD.FuncDec ty name params $ evalState statementList body

collectStructFields :: State [Token] [Token]
collectStructFields = collectUntilDelimiter Dl.Br

struct :: [Token] -> State [Token] Statement
struct taken = do
  (mty, tokens) <- structType
  case mty of
    Right err -> return $ Statement (SD.Invalid err) tokens
    Left ty@(Ty.Struct name fields) -> do
      tokens' <- get
      case map Token.def tokens' of
        TD.Semicolon : _ -> do
          modify $ drop 1
          return $ Statement (SD.Struct name fields) (taken ++ tokens ++ take 1 tokens')
        [] -> return $ Statement (SD.Invalid "Unexpected end of file") (taken ++ tokens)
        _ -> declaration ty (tokens ++ [head tokens'])
    Left ty -> return $ Statement (SD.Invalid $ "Unexpected type " ++ show ty) (taken ++ tokens)

structType :: State [Token] (Either Type String, [Token])
structType = do
  tokens <- get
  name <- case Token.def $ head tokens of
    TD.Id name' -> do
      modify $ drop 1
      return $ Just name'
    _ -> return Nothing
  tokens' <- get
  case Token.def $ head tokens' of
    TD.DelimOpen Dl.Br -> do
      modify $ drop 1
      structTks <- collectStructFields
      case structFields structTks of
        Left fields -> return (Left (Ty.Struct name fields), [head tokens, head tokens'] ++ structTks)
        Right err -> return (Right err, structTks)
    _ -> return (Left (Ty.Struct name []), take 1 tokens)

structFields :: [Token] -> Either [(Type, Id)] String
structFields tokens = validate $ evalState statementList $ Token.filterNL tokens
  where
    validate :: [Statement] -> Either [(Type, Id)] String
    validate fields = case map St.def fields of
      [] -> Left []
      [SD.Var (var :| vars)] -> Left $ map varToField (var : vars)
      SD.Var (var :| vars) : _ -> case validate (tail fields) of
        Left rest -> Left $ map varToField (var : vars) ++ rest
        Right err -> Right err
      def -> Right $ "Unexpected statement : " ++ show def

    varToField (ty, name, _) = (ty, name)

enum :: Maybe Id -> Type -> [Token] -> State [Token] Statement
enum name ty taken = do
  enumTks <- collectEnumVariants
  tokens <- get
  case map Token.def tokens of
    TD.Semicolon : _ -> do
      modify $ drop 1
      case enumVariants enumTks of
        Left variants -> return $ Statement (SD.Enum name ty variants) (taken ++ enumTks ++ [head tokens])
        Right err -> return $ Statement (SD.Invalid err) (taken ++ enumTks ++ [head tokens])
    [] -> return $ Statement (SD.Invalid "Expected semicolon") (taken ++ enumTks ++ [head tokens])
    tk : _ -> return $ Statement (SD.Invalid $ "Expected semicolon, got " ++ show tk) [head tokens]

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