module Scope (Scope (..), new, newFunction, newScope, addVar, addVars, getVar, addLabel, hasLabel, makeAnonLabel, defineFunc, declareFunc, getFunc, getLocals) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (find)
import Identifier (Id (..))
import Identifier qualified as Id (toStr)
import Instruction (Value (..))
import Type (Type, paddedSizeof)
import Type qualified (Type (..), toStr)
import Utils (modifyFirst)

type Var = (Type, Id)

type Func = (Type, Id, Bool)

data Scope = Scope
  { funcs :: [Func],
    vars :: [Var],
    lbls :: [String],
    addr :: Int,
    ctxt :: Context
  }
  deriving (Show)

data Context
  = Local {prev :: Scope}
  | Global
  deriving (Show)

new :: Scope
new = Scope [] [] [] 0 Global

newFunction :: State Scope ()
newFunction = do
  ctxt <- get
  put $ Scope [] [] [] 0 (Local ctxt)

newScope :: State Scope ()
newScope = do
  ctxt@(Scope _ vars l _ _) <- get
  put $ Scope [] [] l (length vars) (Local ctxt)

addVars :: [(Type, Id)] -> State Scope ()
addVars vars = case vars of
  [] -> return ()
  (var : rest) -> addVar var >> addVars rest

addVar :: (Type, Id) -> State Scope ()
addVar (ty, name) = do
  mvar <- findVar False name
  case mvar of
    Nothing -> do
      Scope f vars l a p <- get
      put $ Scope f (vars ++ [(ty, name)]) l a p
    Just _ -> error $ "Redefinition of " ++ Id.toStr name

getLocals :: State Scope [Var]
getLocals = do
  Scope _ vars _ _ _ <- get
  return vars

getVar :: Id -> State Scope (Maybe (Value, Type))
getVar = findVar True

findVar :: Bool -> Id -> State Scope (Maybe (Value, Type))
findVar doPrev name = do
  Scope _ vars _ addr _ <- get
  case go vars 0 of
    Just (idx, ty) -> return $ Just (Cst (addr + idx), ty)
    Nothing -> do
      mfunc <- Scope.getFunc name
      case mfunc of
        Just (ty, Id nameStr, _) -> do
          return $ Just (Lbl nameStr, Type.Pointer ty)
        Nothing | doPrev -> lookInPrev $ findVar doPrev name
        Nothing -> return Nothing
  where
    go :: [Var] -> Int -> Maybe (Int, Type)
    go vars addr = case vars of
      [] -> Nothing
      (ty, name') : _
        | name == name' -> Just (addr, ty)
      (ty, _) : rest -> go rest (addr + paddedSizeof ty)

defineFunc :: (Type, Id) -> State Scope ()
defineFunc f = _addFunc f True

declareFunc :: (Type, Id) -> State Scope ()
declareFunc f = _addFunc f False

_addFunc :: (Type, Id) -> Bool -> State Scope () -- TODO error handling
_addFunc (ty, name) doDefine = do
  mfunc <- findFunc False name
  case mfunc of
    Just (ty', _, _) | ty /= ty' -> error $ "Conflicting types for " ++ Id.toStr name ++ " : " ++ Type.toStr ty ++ ", already declared as " ++ Type.toStr ty'
    Just (_, _, True) | doDefine -> error $ "Redefinition of " ++ Id.toStr name
    Just (_, _, True) -> return ()
    Just (_, _, False) | doDefine -> do
      Scope funcs v l a p <- get
      let funcs' = modifyFirst (byIdFunc name) (\(t, n, _) -> (t, n, True)) funcs
      put $ Scope funcs' v l a p
      return ()
    _ -> do
      Scope funcs v l a p <- get
      put $ Scope (funcs ++ [(ty, name, doDefine)]) v l a p
      return ()

getFunc :: Id -> State Scope (Maybe Func)
getFunc = findFunc True

findFunc :: Bool -> Id -> State Scope (Maybe Func)
findFunc doPrev name = do
  Scope funcs _ _ _ _ <- get
  case find (byIdFunc name) funcs of
    Just func -> return $ Just func
    Nothing | doPrev -> lookInPrev $ findFunc doPrev name
    Nothing -> return Nothing

addLabel :: String -> State Scope ()
addLabel lbl = do
  _ <- _addLabel $ Just lbl
  return ()

makeAnonLabel :: State Scope String
makeAnonLabel = _addLabel Nothing

_addLabel :: Maybe String -> State Scope String -- TODO error if already defined
_addLabel mlbl = do
  Scope f v lbls a p <- get
  let lbl = case mlbl of
        Just lbl' -> lbl'
        Nothing -> ".L" ++ show (length lbls)
  put $ Scope f v (lbls ++ [lbl]) a p
  return lbl

hasLabel :: String -> State Scope Bool
hasLabel lbl = do
  Scope _ _ lbls _ _ <- get
  return $ lbl `elem` lbls

byIdFunc :: Id -> Func -> Bool
byIdFunc name = (== name) . (\var -> let (_, name', _) = var in name')

lookInPrev :: State Scope (Maybe a) -> State Scope (Maybe a)
lookInPrev f = do
  scope@(Scope _ _ _ _ mprev) <- get
  case mprev of
    Global -> return Nothing
    Local {prev = ctxt} -> do
      put ctxt
      var <- f
      put scope
      return var