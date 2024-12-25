{-# OPTIONS_GHC -Wno-type-defaults #-}

module Compiler (compile) where

import Control.Monad.State.Lazy (State, get, put, runState)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe, maybeToList)
import Expr (Expr (Expr), InitializerKind)
import Expr qualified (Expr (..))
import Expr qualified as ED (ExprDef (..))
import Expr qualified as IK (InitializerKind (..))
import Identifier (Id (..))
import Instruction (Instruction (..), Program (..), Register (..), Value (..), regLen)
import Op (Op)
import Op qualified
import Scope (Scope (Scope), declareFunc, defineFunc, getFunc, getGlobal, newFunction, newScope, setGlobal)
import Scope qualified (Scope (..), addLabel, addVar, getLocalFuncs, getVar, hasLabel, makeAnonLabel, new)
import Scope qualified as Context (Context (..))
import Statement (Statement, StorageClass)
import Statement qualified
import Statement qualified as SC (StorageClass (..))
import Statement qualified as SD (StatementDef (..))
import Type (Type, paddedSizeof, sizeof)
import Type qualified
import Utils (Display (display), intoBytes, maybeListToList, unreachable)

compile :: [Statement] -> Program
compile decls =
  let (instructions, globalScope) = runState (statements decls) Scope.new
      globalData = case globalScope of
        Scope _ _ _ (Context.Global gvars) -> makeData gvars
        _ -> unreachable
   in Program (instructions ++ globalData)

makeData :: [(Type, Id, Maybe Expr)] -> [Instruction]
makeData gvars = case gvars of
  [] -> []
  (ty, Id name, mexpr) : rest ->
    let value = case mexpr of
          Nothing -> [0]
          Just ex -> case Expr.def ex of
            ED.IntLiteral _ int -> intoBytes (Type.paddedSizeof ty) int
            _ -> error $ "Not a constant value: " ++ display ex
     in STATIC name value : makeData rest

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
    vars :: [(StorageClass, Type, Id, Maybe Expr)] -> State Scope [Instruction]
    vars vs = do
      ins <- mapM (\(sc, ty, name, e) -> var sc ty name e) vs
      return . concat $ ins

funcDec :: Type -> Id -> [(Type, Maybe Id)] -> State Scope [Instruction]
funcDec ret name params = do
  !_ <- Scope.declareFunc (Type.Function ret (map fst params), name)
  return []

funcDef :: Type -> Id -> [(Type, Maybe Id)] -> [Statement] -> State Scope [Instruction]
funcDef ret name params body = do
  scope <- get
  Scope.newFunction
  Scope.declareFunc (Type.Function ret (map fst params), name)
  collectLabels body
  let namedParams = mapMaybe (\(ty, nm) -> (ty,) <$> nm) params
  loadParams namedParams
  ins <- statements body
  insLocals <- Scope.getLocalFuncs
  put scope
  let Id nameStr = name
  insFunc <- Scope.defineFunc (Type.Function ret (map fst params), name) (LABEL nameStr : ins)
  return $ insFunc ++ insLocals

loadParams :: [(Type, Id)] -> State Scope ()
loadParams params = case params of
  [] -> return ()
  (ty, name) : rest -> do
    !_ <- Scope.addVar (ty, name)
    loadParams rest

var :: StorageClass -> Type -> Id -> Maybe Expr -> State Scope [Instruction]
var sc' ty name mexpr = checkAssign ty mexpr $ do
  !_ <- Scope.addVar (ty, name)
  scope <- get
  let sc = case (Scope.ctxt scope, sc') of
        (Context.Local {}, SC.Infer) -> SC.Auto
        (Context.Global {}, SC.Infer) -> SC.Static
        _ -> sc'
  case (Scope.ctxt scope, sc) of
    (_, SC.Static) -> do
      ctxt <- Scope.getGlobal
      case ctxt of
        Context.Global gvars -> Scope.setGlobal $ Context.Global (gvars ++ [(ty, name, mexpr)])
        _ -> unreachable
      return []
    (Context.Local {}, SC.Auto) -> case mexpr of
      Nothing -> return [ADD SP (Cst $ paddedSizeof ty)]
      Just ex -> case Expr.def ex of
        ED.Initializer exs -> do
          ins <- initializer ty exs
          return $ SET R6 (Reg SP) : ins ++ [ADD SP (Cst $ paddedSizeof ty)]
        _ -> do
          ins <- expr ex
          return $ ins ++ [PUSH (Reg R0)]
    (ctxt, _) -> error $ "Invalid storage class " ++ show sc ++ " in context " ++ show ctxt

initializer :: Type -> [(InitializerKind, Expr)] -> State Scope [Instruction]
initializer ty = go 0
  where
    go idx exs = case exs of
      [] -> return []
      (IK.Simple, ex) : _ -> do
        insEx <- expr ex
        insRest <- go (idx + 1) $ tail exs
        return $ insEx ++ [storeTy (tyAt idx) R6 (Reg R0), ADD R6 (Cst $ sizeAt idx)] ++ insRest
      ex@(ik@(IK.Index _), _) : next : _ -> do
        isUnified <- unified ex next
        if isUnified
          then error $ "Initializer not implemented " ++ show ik
          else error "Index initialized but list is not uniform"
      (ik, _) : _ -> error $ "Initializer not implemented " ++ show ik

    tyAt _ = case ty of -- TODO struct with idx
      Type.Array ty' _ -> ty'
      Type.ArrayNoHint ty' -> ty'
      _ -> error "Invalid type for initializer"

    sizeAt idx = sizeof $ tyAt idx

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
  ty <- evalOrThrow e
  ins <- case Expr.def e of
    ED.Id name -> do
      insVar <- getVarAddr name
      if Type.isComplex ty
        then return $ insVar ++ [SET R0 (Reg R1)]
        else return $ insVar ++ [LOAD R0 (Reg R1)]
    ED.IntLiteral _ int -> do
      return [SET R0 (Cst int)]
    -- ED.FltLiteral flt -> []
    -- ED.StrLiteral str -> []
    ED.Initializer _ -> error $ "Can't evaluate initializer : " ++ display e
    ED.UnopPre op ex -> unop op ex
    -- ED.UnopPost op ex -> []
    ED.Binop left op right -> do
      case Op.getBinaryAssignOp op of
        Just innerOp ->
          expr $ Expr (ED.Binop left Op.Assign (Expr (ED.Binop left innerOp right) (Expr.tks e))) (Expr.tks e)
        Nothing -> binop left op right
    -- ED.Ternary ter_cond ter_then ter_else -> []
    ED.Call ex args -> call ex args
    ED.Parenthese ex -> expr ex
    ED.Ambiguous left right -> expr $ solveAmbiguous left right
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
unop op ex = do
  let insOp = case op of
        Op.MultOrIndir -> [LOAD R0 (Reg R0)]
        Op.BitAndOrAddr -> [SET R0 (Reg R1)]
        _ -> error $ "Operator not implemented : " ++ show op
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
        Op.Assign -> [storeTy leftTy R5 (Reg R0)]
        Op.Subscript -> case leftTy of
          Type.Array ty' _ -> [MUL R4 (Cst $ sizeof ty'), ADD R4 (Reg R0), LOAD R0 (Reg R4)]
          Type.Pointer ty' -> [MUL R4 (Cst $ sizeof ty'), ADD R4 (Reg R0), LOAD R0 (Reg R4)]
          _ -> error "Subscript on invalid value"
        Op.Member -> [ADD R1 (Reg R0), LOAD R0 (Reg R1)]
        Op.MemberPtr -> [LOAD R1 (Reg R1), ADD R1 (Reg R0), LOAD R0 (Reg R1)]
        _ -> error $ "Operator not implemented : " ++ show op
   in case op of
        Op.Assign -> checkAssign leftTy (Just right) $ do
          insAddr <- lvalue left
          insVal <- expr right
          return $ insAddr ++ [PUSH (Reg R1)] ++ insVal ++ [POP R5] ++ insOp
        Op.Member -> case Expr.def right of
          ED.Id name -> do
            insAddr <- expr left
            let (_, addr) = getMember name leftTy
            return $ insAddr ++ [SET R0 (Cst addr)] ++ insOp
          _ -> error $ "Invalid member " ++ display right
        Op.MemberPtr -> case Expr.def right of
          ED.Id name -> do
            let innerTy = case leftTy of
                  Type.Pointer ty' -> ty'
                  _ -> error "Not a pointer"
            insAddr <- expr left
            let (_, addr) = getMember name innerTy
            return $ insAddr ++ [SET R0 (Cst addr)] ++ insOp
          _ -> error $ "Invalid member " ++ display right
        _ | otherwise -> do
          insL <- expr left
          insR <- expr right
          return $ insR ++ [PUSH (Reg R0)] ++ insL ++ [POP R4] ++ insOp

call :: Expr -> [Expr] -> State Scope [Instruction] -- TODO type checking
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

lvalue :: Expr -> State Scope [Instruction]
lvalue e = case Expr.def e of
  ED.Id name -> getVarAddr name
  ED.UnopPre Op.MultOrIndir e' -> do
    ins <- expr e'
    return $ ins ++ [SET R1 (Reg R0)]
  ED.Binop left Op.Subscript right -> do
    ty <- evalOrThrow left
    insAddr <- expr left
    insVal <- expr right
    return $ insAddr ++ [PUSH (Reg R1)] ++ insVal ++ [POP R5, MUL R0 (Cst $ sizeof ty), ADD R5 (Reg R0), SET R1 (Reg R5)]
  ED.Binop left Op.Member right -> do
    leftTy <- evalOrThrow left
    case Expr.def right of
      ED.Id name -> do
        insAddr <- lvalue left
        let (_, addr) = getMember name leftTy
        return $ insAddr ++ [ADD R1 (Cst addr)]
      _ -> error $ "Invalid member " ++ display right
  ED.Binop left Op.MemberPtr right -> do
    leftTy <- evalOrThrow left
    let innerTy = case leftTy of
          Type.Pointer ty' -> ty'
          _ -> error "Not a pointer"
    case Expr.def right of
      ED.Id name -> do
        insAddr <- lvalue left
        let (_, addr) = getMember name innerTy
        return $ insAddr ++ [LOAD R1 (Reg R1)] ++ [ADD R1 (Cst addr)]
      _ -> error $ "Invalid member " ++ display right
  ED.Parenthese ex' -> lvalue ex'
  ED.Ambiguous left right -> lvalue $ solveAmbiguous left right
  _ -> error $ "Not an lvalue : " ++ display e

getVarAddr :: Id -> State Scope [Instruction]
getVarAddr name = do
  mvar <- Scope.getVar name
  case mvar of
    Just (_, Lbl lbl, _) -> return [SET R1 (Lbl lbl)]
    Just (depth, addr, _) ->
      let insBack = concat $ replicate depth [SUB R1 (Cst 1), LOAD R1 (Reg R1)]
       in return $ SET R1 (Reg BP) : insBack ++ [ADD R1 addr]
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
      Just (_, _, ty) -> return $ Left ty
      Nothing -> error $ "Undefined identifier : " ++ show name
  ED.IntLiteral ty _ -> return $ Left ty
  ED.FltLiteral ty _ -> return $ Left ty
  ED.StrLiteral str -> return . Left $ Type.Array Type.Char (length str)
  ED.Initializer _ -> return $ Left Type.Infer {- case exs of -- return $ Right "Can't evaluate initializer" -- Type.Struct Nothing (map (second eval) exs) -- FIXME eval whole array
                                               [] -> return $ Left Type.Void
                                               (_, ex') : _ -> do
                                                 ty <- evalOrThrow ex'
                                                 return $ Left (Type.Array ty (length exs)) -}
  ED.UnopPre Op.BitAndOrAddr ex' -> do
    ev <- eval ex'
    case ev of
      Left ty -> return . Left $ Type.Pointer ty
      ret -> return ret
  ED.UnopPre Op.MultOrIndir ex' -> do
    ev <- eval ex'
    case ev of
      Left (Type.Pointer ty) -> return $ Left ty
      Left (Type.Array ty _) -> return $ Left ty
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
  ED.Binop left Op.MemberPtr right -> do
    ev <- eval left
    case ev of
      Left (Type.Pointer ty) -> case Expr.def right of
        ED.Id field -> do
          let (ty', _) = getMember field ty
           in return $ Left ty'
        _ -> error $ "Invalid member " ++ display right
      Left _ -> error "Not a pointer"
      ret -> return ret
  ED.Binop left Op.Subscript _ -> do
    ev <- eval left
    case ev of -- TODO probably wrong
      Left (Type.Array ty _) -> return $ Left ty
      Left (Type.Pointer ty) -> return $ Left ty
      Left _ -> error $ "Can't perform subscript on " ++ display left
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
  ED.Ambiguous left right -> eval $ solveAmbiguous left right
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

checkAssign :: Type -> Maybe Expr -> State Scope a -> State Scope a
checkAssign _ Nothing f = f
checkAssign leftTy (Just right) f = do
  rightTy <- evalOrThrow right
  let ret
        | leftTy == rightTy = f
        | rightTy == Type.Infer = f
        | Type.canCast rightTy leftTy = f
        | otherwise = error $ "Can't assign " ++ Type.toStr rightTy ++ " to " ++ Type.toStr leftTy
   in ret

storeTy :: Type -> Register -> Value -> Instruction
storeTy ty reg val = case sizeof ty of
  1 -> STRB reg val
  2 -> STRH reg val
  4 -> STRW reg val
  8 -> STRD reg val
  _ -> error $ "Can't store type " ++ show ty

solveAmbiguous :: Expr -> Expr -> Expr
solveAmbiguous left right = case (Expr.def left, Expr.def right) of
  (ED.UnopPre (Op.Cast _) _, _) -> right
  -- case ty' of
  -- Type.Typedef _ -> error "Typedefs not implemented"
  -- _ -> right
  (_, _) -> error $ "Unknown ambiguous case " ++ display left ++ " vs " ++ display right
