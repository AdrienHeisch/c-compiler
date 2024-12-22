{-# OPTIONS_GHC -Wno-type-defaults #-}

module Compiler (compile) where

import Constant (Constant (Constant))
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe, maybeToList)
import Expr (Expr, InitializerKind)
import Expr qualified (Expr (..))
import Expr qualified as ED (ExprDef (..))
import Expr qualified as IK (InitializerKind (..))
import Identifier (Id (..))
import Instruction (Instruction (..), Program (..), Register (..), Value (..), regLen)
import Op (Op)
import Op qualified
import Scope (Scope, declareFunc, defineFunc, getFunc, newFunction, newScope)
import Scope qualified (addLabel, addVar, getVar, hasLabel, makeAnonLabel, new)
import Statement (Statement)
import Statement qualified
import Statement qualified as SD (StatementDef (..))
import Type (Type, paddedSizeof, sizeof)
import Type qualified (Type (..), mask)
import Utils (Display (display), maybeListToList)

compile :: [Statement] -> Program
compile decls = Program $ evalState (statements decls) Scope.new

statements :: [Statement] -> State Scope [Instruction]
statements sts = case sts of
  [] -> return []
  st : sts' -> do
    insSt <- statement st
    insSts <- statements sts'
    return $ insSt ++ insSts

statement :: Statement -> State Scope [Instruction]
statement st = case Statement.def st of
  SD.Empty -> return []
  SD.FuncDec ty name params -> funcDec ty name params
  SD.FuncDef ty name params body -> funcDef ty name params body
  SD.Block sts -> block sts
  SD.Expr e -> expr e
  SD.Var (v :| vs) -> vars (v : vs)
  SD.If cond then_ else_ -> if_ cond then_ else_
  -- SD.Switch {eval :: Expr, body :: Statement} ->
  SD.While cond body -> while cond body
  SD.DoWhile body cond -> dowhile cond body
  SD.For finit fcond fincr fbody -> for finit fcond fincr fbody
  -- SD.Break ->
  -- SD.Continue ->
  SD.Return mexpr -> return_ mexpr
  -- SD.Case (Constant IntRepr) ->
  SD.Goto lblName -> goto lblName
  SD.Labeled lblName st' -> label lblName st'
  SD.Invalid str -> error $ "Invalid statement : " ++ str
  _ -> error $ "Statement not implemented yet : " ++ display st
  where
    vars :: [(Type, Id, Maybe Expr)] -> State Scope [Instruction]
    vars vs = do
      ins <- mapM (\(ty, name, e) -> var ty name e) vs
      return . concat $ ins

funcDec :: Type -> Id -> [(Type, Maybe Id)] -> State Scope [Instruction]
funcDec ret name params = do
  !_ <- Scope.declareFunc (Type.Function ret (map fst params), name)
  return []

funcDef :: Type -> Id -> [(Type, Maybe Id)] -> [Statement] -> State Scope [Instruction]
funcDef ret name params body = do
  !_ <- Scope.defineFunc (Type.Function ret (map fst params), name)
  scope <- get
  Scope.newFunction
  collectLabels body
  let namedParams = mapMaybe (\(ty, nm) -> (ty,) <$> nm) params
  loadParams namedParams
  ins <- statements body
  put scope
  let Id nameStr = name
  return $ LABEL nameStr : ins

loadParams :: [(Type, Id)] -> State Scope ()
loadParams params = case params of
  [] -> return ()
  (ty, name) : rest -> do
    !_ <- Scope.addVar (ty, name)
    loadParams rest

var :: Type -> Id -> Maybe Expr -> State Scope [Instruction]
var ty name mexpr = do
  !_ <- Scope.addVar (ty, name)
  case mexpr of
    Nothing -> return [ADD SP (Cst $ paddedSizeof ty)]
    Just ex -> case Expr.def ex of
      ED.Initializer exs -> do
        ins <- initializer ty exs
        return $ SET R2 (Reg SP) : SET R6 (Reg SP) : ins ++ [ADD SP (Cst $ paddedSizeof ty)] ++ [PUSH (Reg R2)] -- FIXME R2 is a hack
      _ -> do
        ins <- expr ex
        return $ ins ++ [PUSH (Reg R0)]

initializer :: Type -> [(InitializerKind, Expr)] -> State Scope [Instruction]
initializer ty = go 0
  where
    go idx exs = case exs of
      [] -> return []
      (IK.Simple, ex) : _ -> do
        insEx <- expr ex
        insRest <- go (idx + 1) $ tail exs
        return $ insEx ++ [STORE R6 (Reg R0), ADD R6 (Cst $ sizeAt idx)] ++ insRest
      ex@(ik@(IK.Index _), _) : next : _ -> do
        isUnified <- unified ex next
        if isUnified
          then error $ "Initializer not implemented " ++ show ik
          else error "Index initialized but list is not uniform"
      (ik, _) : _ -> error $ "Initializer not implemented " ++ show ik
    sizeAt _ = case ty of
      Type.Array ty' _ -> sizeof ty'
      Type.ArrayNoHint ty' -> sizeof ty'
      _ -> error "Invalid type for initializer"
    unified (_, first) (_, next) = do
      fTy <- evalOrThrow first
      nTy <- evalOrThrow next
      return $ fTy == nTy

block :: [Statement] -> State Scope [Instruction]
block sts = do
  scope <- get
  Scope.newScope
  ins <- statements sts
  put scope
  return ins

if_ :: Expr -> Statement -> Maybe Statement -> State Scope [Instruction]
if_ cond then_ else_ = do
  lblElse <- Scope.makeAnonLabel
  lblPost <- Scope.makeAnonLabel
  insCond <- expr cond
  insThen <- statement then_
  insElse <- maybe (return Nothing) (fmap Just . statement) else_
  return $
    insCond
      ++ [JEQ R0 (Lbl lblElse)]
      ++ insThen
      ++ [JMP (Lbl lblPost), LABEL lblElse]
      ++ maybeListToList insElse
      ++ [LABEL lblPost] -- TODO avoid double label if no else

while :: Expr -> Statement -> State Scope [Instruction]
while cond body = do
  lblPre <- Scope.makeAnonLabel
  lblPost <- Scope.makeAnonLabel
  insCond <- expr cond
  insBody <- statements [body]
  return $ LABEL lblPre : insCond ++ [JEQ R0 (Lbl lblPost)] ++ insBody ++ [JMP (Lbl lblPre)] ++ [LABEL lblPost]

dowhile :: Expr -> Statement -> State Scope [Instruction]
dowhile cond body = do
  lblPre <- Scope.makeAnonLabel
  lblPost <- Scope.makeAnonLabel
  insCond <- expr cond
  insBody <- statements [body]
  return $ LABEL lblPre : insBody ++ insCond ++ [JEQ R0 (Lbl lblPost)] ++ [JMP (Lbl lblPre)] ++ [LABEL lblPost]

for :: Either Statement (Maybe Expr) -> Maybe Expr -> Maybe Expr -> Statement -> State Scope [Instruction]
for finit fcond fincr fbody = do
  lblPre <- Scope.makeAnonLabel
  lblPost <- Scope.makeAnonLabel
  insInit <- case finit of
    Left st -> statement st
    Right (Just e) -> expr e
    Right Nothing -> return []
  insCond <- concat <$> sequence (maybeToList $ expr <$> fcond)
  insIncr <- concat <$> sequence (maybeToList $ expr <$> fincr)
  insBody <- statement fbody
  return $ insInit ++ [LABEL lblPre] ++ insCond ++ [JEQ R0 (Lbl lblPost)] ++ insBody ++ insIncr ++ [JMP (Lbl lblPre), LABEL lblPost]

goto :: Id -> State Scope [Instruction]
goto (Id lbl) = do
  hasLabel <- Scope.hasLabel lbl
  if hasLabel
    then return [JMP (Lbl lbl)]
    else error $ "Undefined label : " ++ show lbl

label :: Id -> Statement -> State Scope [Instruction]
label (Id lbl) st = do
  exists <- Scope.hasLabel lbl
  if exists
    then do
      ins <- statement st
      return $ LABEL lbl : ins
    else error "Label does not exist"

expr :: Expr -> State Scope [Instruction]
expr e = do
  ins <- case Expr.def e of
    ED.Id name -> do
      insVar <- getVarAddr name
      return $ insVar ++ [LOAD R0 (Reg R1)]
    ED.IntLiteral (Constant Type.Int int) -> do
      return [SET R0 (Cst int)]
    -- ED.FltLiteral flt -> []
    -- ED.StrLiteral str -> []
    ED.Initializer _ -> error $ "Can't evaluate initializer : " ++ display e
    ED.UnopPre op ex -> unop op ex
    -- ED.UnopPost op ex -> []
    ED.Binop left op right -> do
      case Op.getBinaryAssignOp op of
        Just innerOp -> do
          insBinop <- binop left innerOp right
          insAssign <- binop left Op.Assign right
          return $ insBinop ++ insAssign
        Nothing -> binop left op right
    -- ED.Ternary ter_cond ter_then ter_else -> []
    ED.Call ex args -> call ex args
    ED.Parenthese ex -> expr ex
    ED.Invalid str -> error $ "Invalid expression : " ++ str
    _ -> error $ "Expression not implemented yet : " ++ display e
  withMask ins
  where
    withMask ins = do
      ty <- evalOrThrow e
      let size = sizeof ty
      if 0 < size && size < regLen
        then return $ ins ++ [AND R0 (Cst $ Type.mask ty)]
        else return ins

unop :: Op -> Expr -> State Scope [Instruction]
unop op ex =
  let insOp = case op of
        Op.MultOrIndir -> [LOAD R0 (Reg R0)]
        Op.BitAndOrAddr -> [SET R0 (Reg R1)]
        _ -> error $ "Operator not implemented : " ++ show op
   in case op of
        _ | Op.isUnopAddressing op -> do
          insAddr <- exprAddress ex
          return $ insAddr ++ insOp
        _ | otherwise -> do
          insEx <- expr ex
          return $ insEx ++ insOp

binop :: Expr -> Op -> Expr -> State Scope [Instruction]
binop left op right = do
  leftTy <- evalOrThrow left
  let insOp = case op of
        Op.AddOrPlus -> case leftTy of
          Type.Array ty' _ -> [MUL R4 (Cst $ sizeof ty'), ADD R0 (Reg R4)]
          Type.Pointer ty' -> [MUL R4 (Cst $ sizeof ty'), ADD R0 (Reg R4)]
          _ -> [ADD R0 (Reg R4)]
        Op.SubOrNeg -> case leftTy of
          Type.Array ty' _ -> [MUL R4 (Cst $ sizeof ty'), SUB R0 (Reg R4)]
          Type.Pointer ty' -> [MUL R4 (Cst $ sizeof ty'), SUB R0 (Reg R4)]
          _ -> [SUB R0 (Reg R4)]
        Op.MultOrIndir -> [MUL R0 (Reg R4)]
        Op.Div -> [DIV R0 (Reg R4)]
        Op.Mod -> [MOD R0 (Reg R4)]
        Op.Assign -> [STORE R5 (Reg R0)]
        Op.Subscript -> case leftTy of
          Type.Array ty' _ -> [MUL R0 (Cst $ sizeof ty'), ADD R5 (Reg R0), LOAD R0 (Reg R5)]
          Type.Pointer ty' -> [MUL R0 (Cst $ sizeof ty'), ADD R5 (Reg R0), LOAD R0 (Reg R5)]
          _ -> error "Subscript on invalid value"
        Op.Member -> [ADD R1 (Reg R0), LOAD R0 (Reg R1)]
        _ -> error $ "Operator not implemented : " ++ show op
   in case op of
        _ | Op.isBinopAddressing op -> do
          insAddr <- exprAddress left
          insVal <- expr right
          return $ insAddr ++ [PUSH (Reg R1)] ++ insVal ++ [POP R5] ++ insOp
        _ | Op.isBinopMember op -> case Expr.def right of
          ED.Id name -> do
            insAddr <- exprAddress left
            let (_, addr) = getMember name leftTy
            return $ insAddr ++ [SET R0 (Cst addr)] ++ insOp
          _ -> error $ "Invalid member " ++ display right
        _ | otherwise -> do
          insL <- expr left
          insR <- expr right
          return $ insR ++ [PUSH (Reg R0)] ++ insL ++ [POP R4] ++ insOp

call :: Expr -> [Expr] -> State Scope [Instruction]
call ex params = case Expr.def ex of
  ED.Parenthese ex' -> call ex' params
  ED.Id name -> do
    mfunc <- Scope.getFunc name
    case mfunc of
      Just (_, Id nameStr, _) -> go (Lbl nameStr)
      Nothing -> do
        insAddr <- expr ex
        insCall <- go (Reg R7)
        return $ insAddr ++ [SET R7 (Reg R0)] ++ insCall -- FIXME that's a hack
  _ -> error "Unimplemented call operand"
  where
    go addr = do
      insParams <- map (\ins -> ins ++ [PUSH (Reg R0)]) <$> mapM expr params
      return $ [PUSH (Reg LR), PUSH (Reg BP), SET BP (Reg SP)] ++ concat insParams ++ [CALL addr, SET R0 (Reg RR), POP BP, POP LR]

return_ :: Maybe Expr -> State Scope [Instruction]
return_ mexpr = case mexpr of
  Nothing -> return [SET SP (Reg BP), RET (Cst 0)]
  Just ex -> do
    insEx <- expr ex
    return $ insEx ++ [SET SP (Reg BP), RET (Reg R0)]

exprAddress :: Expr -> State Scope [Instruction]
exprAddress e = case Expr.def e of
  ED.Parenthese ex' -> exprAddress ex'
  ED.Id name -> getVarAddr name
  ED.UnopPre Op.MultOrIndir e' -> do
    insEx <- expr e'
    return $ insEx ++ [SET R1 (Reg R0)]
  ED.Binop left Op.Subscript right -> do
    ty <- evalOrThrow left
    let ty' = case ty of
          Type.Array ty'' _ -> ty''
          Type.ArrayNoHint ty'' -> ty''
          Type.Pointer ty'' -> ty''
          _ -> error "Subscript on invalid value"
    insAddr <- exprAddress left
    insVal <- expr right
    return $ insAddr ++ [PUSH (Reg R1)] ++ insVal ++ [POP R5, MUL R0 (Cst $ sizeof ty'), ADD R5 (Reg R0), SET R1 (Reg R5)]
  ED.Binop left Op.Member right -> do
    leftTy <- evalOrThrow left
    case Expr.def right of
      ED.Id name -> do
        insAddr <- exprAddress left
        let (_, addr) = getMember name leftTy
        return $ insAddr ++ [ADD R1 (Cst addr)]
      _ -> error $ "Invalid member " ++ display right
  _ -> error $ "Can't get address of : " ++ display e

getVarAddr :: Id -> State Scope [Instruction]
getVarAddr name = do
  mvar <- Scope.getVar name
  case mvar of
    Just (addr, _) ->
      return [SET R1 (Reg BP), ADD R1 addr]
    Nothing -> error $ "Undefined identifier : " ++ show name

getMember :: Id -> Type -> (Type, Int)
getMember name ty = case ty of
  Type.Struct _ initFields -> go 0 initFields
  _ -> error $ "Type has no fields : " ++ show ty
  where
    go :: Int -> [(Type, Id)] -> (Type, Int)
    go addr fields = case fields of
      [] -> error $ "Member not found " ++ show name ++ " in " ++ show ty
      (ty', name') : rest ->
        if name' == name
          then (ty', addr)
          else go (addr + sizeof ty') rest

collectLabels :: [Statement] -> State Scope ()
collectLabels sts = case map Statement.def sts of
  [] -> return ()
  SD.Labeled (Id lbl) _ : _ -> do
    Scope.addLabel lbl
    collectLabels $ tail sts
  _ : _ -> collectLabels $ tail sts

eval :: Expr -> State Scope (Either Type String)
eval ex = case Expr.def ex of
  ED.Id name -> do
    mty <- Scope.getVar name
    case mty of
      Just (_, ty) -> return $ Left ty
      Nothing -> error $ "Undefined identifier : " ++ show name
  ED.IntLiteral (Constant ty _) -> return $ Left ty
  ED.FltLiteral (Constant ty _) -> return $ Left ty
  ED.StrLiteral (Constant ty _) -> return $ Left ty
  ED.Initializer _ -> return $ Right "Can't evaluate initializer" -- Type.Struct Nothing (map (second eval) exs) -- FIXME eval whole array
  ED.UnopPre Op.BitAndOrAddr ex' -> do
    ev <- eval ex'
    case ev of
      Left ty -> return . Left $ Type.Pointer ty
      ret -> return ret
  ED.UnopPre Op.MultOrIndir ex' -> do
    ev <- eval ex'
    case ev of
      Left (Type.Pointer ty) -> return $ Left ty
      Left ty -> return . Right $ "Not a pointer : " ++ show ty
      ret -> return ret
  ED.UnopPre _ ex' -> eval ex' -- TODO probably wrong
  ED.UnopPost _ ex' -> eval ex' -- TODO probably wrong
  ED.Binop left Op.Member right -> do
    ev <- eval left
    case ev of -- TODO probably wrong
      Left ty -> case Expr.def right of
        ED.Id field -> do
          let (ty', _) = getMember field ty
           in return $ Left ty'
        _ -> error $ "Invalid member " ++ display right
      ret -> return ret
  ED.Binop left _ right -> do
    ev <- eval left
    case ev of -- TODO probably wrong
      Left Type.Infer -> eval right
      ret -> return ret
  ED.Ternary _ ter_then ter_else -> do
    ev <- eval ter_then
    case ev of -- TODO probably wrong
      Left Type.Infer -> eval ter_else
      ret -> return ret
  ED.Call ex' _ -> do
    ev <- eval ex'
    return $ evalCall ev
  ED.Parenthese ex' -> eval ex'
  ED.SizeofType _ -> return $ Left Type.Int
  ED.Invalid str -> return $ Right $ "Evaluating invalid expression : " ++ str
  where
    evalCall :: Either Type String -> Either Type String
    evalCall mty = case mty of
      Left (Type.Function ty _) -> Left ty
      Left (Type.Pointer ty) -> evalCall (Left ty)
      Left ty -> Right $ "Tried to call a value of type " ++ show ty
      Right err -> Right err

evalOrThrow :: Expr -> State Scope Type
evalOrThrow ex = do
  ev <- eval ex
  return $ case ev of Left ty -> ty; Right err -> error err
