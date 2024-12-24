module Scope (Scope (..), Context (..), new, newFunction, newScope, addVar, addVars, getVar, addLabel, hasLabel, makeAnonLabel, defineFunc, declareFunc, getFunc, getLocals, getLocalFuncs, getGlobal, setGlobal) where

import Control.Monad.State.Lazy (State, get, put, modify)
import Data.List (find)
import Identifier (Id (..))
import Identifier qualified as Id (toStr)
import Instruction (Instruction, Value (..))
import Type (Type)
import Type qualified
import Utils (modifyFirst)
import Expr (Expr)

type Var = (Type, Id)

type Func = (Type, Id, Bool)

data Scope = Scope
  { funcs :: [Func],
    vars :: [Var],
    addr :: Int,
    ctxt :: Context
  }
  deriving (Show)

data Context
  = Local
      { prev :: Scope,
        lfuncs :: [Instruction],
        lbls :: [String]
      }
  | Global {gvars :: [(Type, Maybe Expr)]}
  deriving (Show)

getGlobal :: State Scope Context
getGlobal = do
  go <$> get
  where
    go scope = case scope of
      Scope {ctxt = Local prev _ _} -> do
        go prev
      Scope {ctxt = ctxt@(Global _)} -> ctxt

setGlobal :: Context -> State Scope ()
setGlobal newGlobal = do
  modify go
  where
    go :: Scope -> Scope
    go scope = case scope of
      Scope f v a (Local prev lf l) -> do
        let newPrev = go prev
        Scope f v a (Local newPrev lf l)
      Scope f v a (Global _) -> do
        Scope f v a newGlobal

getLocalFuncs :: State Scope [Instruction]
getLocalFuncs = do
  go <$> get
  where
    go scope =
      case scope of
        Scope {ctxt = Global _} -> []
        Scope {ctxt = Local prev lfuncs _} -> do
          lfuncs ++ go prev

new :: Scope
new = Scope [] [] 0 (Global [])

newFunction :: State Scope ()
newFunction = do
  scope <- get
  put $ Scope [] [] 0 (Local scope [] [])

newScope :: State Scope ()
newScope = do
  scope <- get
  case scope of
    Scope _ vars _ (Local {lbls = lbls}) -> put $ Scope [] [] (length vars) (Local scope [] lbls)
    Scope _ _ _ (Global _) -> error "Tried to create scope in global context"

addVars :: [(Type, Id)] -> State Scope ()
addVars vars = case vars of
  [] -> return ()
  (var : rest) -> addVar var >> addVars rest

addVar :: (Type, Id) -> State Scope ()
addVar (ty, name) = do
  mvar <- findVar False name
  case mvar of
    Nothing -> do
      Scope f vars a c <- get
      put $ Scope f (vars ++ [(ty, name)]) a c
    Just _ -> error $ "Redefinition of " ++ Id.toStr name

getLocals :: State Scope [Var]
getLocals = do
  Scope _ vars _ _ <- get
  return vars

getVar :: Id -> State Scope (Maybe (Int, Value, Type))
getVar = findVar True

findVar :: Bool -> Id -> State Scope (Maybe (Int, Value, Type))
findVar doPrev name = go 0
  where
    go :: Int -> State Scope (Maybe (Int, Value, Type))
    go depth = do
      Scope _ vars addr _ <- get
      case go' vars 0 of
        Just (idx, ty) -> return $ Just (depth, Cst (addr + idx), ty)
        Nothing -> do
          mfunc <- Scope.getFunc name
          case mfunc of
            Just (ty, Id nameStr, _) -> do
              return $ Just (depth, Lbl nameStr, Type.Pointer ty)
            Nothing | doPrev -> lookInPrev $ go (depth + 1)
            Nothing -> return Nothing

    go' :: [Var] -> Int -> Maybe (Int, Type)
    go' vars addr = case vars of
      [] -> Nothing
      (ty, name') : _
        | name == name' -> Just (addr, ty)
      (ty, _) : rest -> go' rest (addr + Type.paddedSizeof ty)

declareFunc :: (Type, Id) -> State Scope ()
declareFunc def = _addFunc def False

defineFunc :: (Type, Id) -> [Instruction] -> State Scope [Instruction]
defineFunc def ins = do
  _addFunc def True
  scope <- get
  case scope of
    Scope f v a (Local p lfuncs l) -> do
      put $ Scope f v a $ Local p (lfuncs ++ ins) l
      return []
    Scope _ _ _ (Global _) -> return ins

_addFunc :: (Type, Id) -> Bool -> State Scope () -- TODO error handling
_addFunc (ty, name) doDefine = do
  mfunc <- findFunc False name
  case mfunc of
    Just (ty', _, _) | ty /= ty' -> error $ "Conflicting types for " ++ Id.toStr name ++ " : " ++ Type.toStr ty ++ ", already declared as " ++ Type.toStr ty'
    Just (_, _, True) | doDefine -> error $ "Redefinition of " ++ Id.toStr name
    Just (_, _, True) -> return ()
    Just (_, _, False) | doDefine -> do
      Scope funcs v a c <- get
      let funcs' = modifyFirst (byIdFunc name) (\(t, n, _) -> (t, n, True)) funcs
      put $ Scope funcs' v a c
      return ()
    _ -> do
      Scope funcs v a c <- get
      put $ Scope (funcs ++ [(ty, name, doDefine)]) v a c
      return ()

getFunc :: Id -> State Scope (Maybe Func)
getFunc = findFunc True

findFunc :: Bool -> Id -> State Scope (Maybe Func) -- FIXME local names might conflict when functions are flattened into labels
findFunc doPrev name = do
  Scope funcs _ _ _ <- get
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
  scope <- get
  case scope of
    Scope f v a (Local p lf lbls) -> do
      let lbl = case mlbl of
            Just lbl' -> lbl'
            Nothing -> ".L" ++ show (length lbls)
      put $ Scope f v a (Local p lf (lbls ++ [lbl]))
      return lbl
    _ -> error "Labels are not allowed in global scope"

hasLabel :: String -> State Scope Bool
hasLabel lbl = do
  scope <- get
  case scope of
    Scope _ _ _ (Local _ _ lbls) -> return $ lbl `elem` lbls
    _ -> error "Labels are not allowed in global scope"

byIdFunc :: Id -> Func -> Bool
byIdFunc name = (== name) . (\var -> let (_, name', _) = var in name')

lookInPrev :: State Scope (Maybe a) -> State Scope (Maybe a)
lookInPrev f = do
  scope@(Scope _ _ _ mctxt) <- get
  case mctxt of
    Global _ -> return Nothing
    Local {prev = ctxt} -> do
      put ctxt
      var <- f
      put scope
      return var