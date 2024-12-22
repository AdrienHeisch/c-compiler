module Context (Context (..), new, newFunction, newScope, addVar, addVars, getVar, addLabel, hasLabel, makeAnonLabel, defineFunc, declareFunc, getFunc, getLocals) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (find)
import Identifier (Id)
import Identifier qualified as Id (toStr)
import Type (Type, sizeofWithPointer, paddedSizeof, isComplex)
import Type qualified (toStr)
import Utils (modifyFirst)

type Var = (Type, Id)

type Func = (Type, Id, Bool)

data Context = Context
  { funcs :: [Func],
    vars :: [Var],
    lbls :: [String],
    addr :: Int,
    prev :: Maybe Context
  }
  deriving (Show)

new :: Context
new = Context [] [] [] 0 Nothing

newFunction :: State Context ()
newFunction = do
  prev <- get
  put $ Context [] [] [] 0 (Just prev)

newScope :: State Context ()
newScope = do
  prev@(Context _ vars l _ _) <- get
  put $ Context [] [] l (length vars) (Just prev)

addVars :: [(Type, Id)] -> State Context ()
addVars vars = case vars of
  [] -> return ()
  (var : rest) -> addVar var >> addVars rest

addVar :: (Type, Id) -> State Context ()
addVar (ty, name) = do
  mvar <- findVar False name
  case mvar of
    Nothing -> do
      Context f vars l a p <- get
      put $ Context f (vars ++ [(ty, name)]) l a p
    Just _ -> error $ "Redefinition of " ++ Id.toStr name

getLocals :: State Context [(Int, Type)]
getLocals = do
  Context _ vars _ _ _ <- get
  return (zipWith (curry (\(idx, (ty, _)) -> (idx, ty))) [0 ..] vars)

getVar :: Id -> State Context (Maybe (Int, Type))
getVar = findVar True

findVar :: Bool -> Id -> State Context (Maybe (Int, Type))
findVar doPrev name = do
  Context _ vars _ addr _ <- get
  case go vars 0 of
    Just (idx, ty) -> return $ Just (addr + idx, ty)
    Nothing | doPrev -> lookInPrev $ findVar doPrev name
    Nothing -> return Nothing
  where
    go :: [Var] -> Int -> Maybe (Int, Type)
    go vars addr = case vars of
      [] -> Nothing
      (ty, name') : _ | name == name' -> if isComplex ty
        then Just (addr + paddedSizeof ty, ty)
        else Just (addr, ty)
      (ty, _) : rest -> go rest (addr + sizeofWithPointer ty)

defineFunc :: (Type, Id) -> State Context ()
defineFunc f = _addFunc f True

declareFunc :: (Type, Id) -> State Context ()
declareFunc f = _addFunc f False

_addFunc :: (Type, Id) -> Bool -> State Context () -- TODO error handling
_addFunc (ty, name) doDefine = do
  mfunc <- findFunc False name
  case mfunc of
    Just (ty', _, _) | ty /= ty' -> error $ "Conflicting types for " ++ Id.toStr name ++ " : " ++ Type.toStr ty ++ ", already declared as " ++ Type.toStr ty'
    Just (_, _, True) | doDefine -> error $ "Redefinition of " ++ Id.toStr name
    Just (_, _, True) -> return ()
    Just (_, _, False) | doDefine -> do
      Context funcs v l a p <- get
      let funcs' = modifyFirst (byIdFunc name) (\(t, n, _) -> (t, n, True)) funcs
      put $ Context funcs' v l a p
      return ()
    _ -> do
      Context funcs v l a p <- get
      put $ Context (funcs ++ [(ty, name, doDefine)]) v l a p
      return ()

getFunc :: Id -> State Context (Maybe Func)
getFunc = findFunc True

findFunc :: Bool -> Id -> State Context (Maybe Func)
findFunc doPrev name = do
  Context funcs _ _ _ _ <- get
  case find (byIdFunc name) funcs of
    Just func -> return $ Just func
    Nothing | doPrev -> lookInPrev $ findFunc doPrev name
    Nothing -> return Nothing

addLabel :: String -> State Context ()
addLabel lbl = do
  _ <- _addLabel $ Just lbl
  return ()

makeAnonLabel :: State Context String
makeAnonLabel = _addLabel Nothing

_addLabel :: Maybe String -> State Context String -- TODO error if already defined
_addLabel mlbl = do
  Context f v lbls a p <- get
  let lbl = case mlbl of
        Just lbl' -> lbl'
        Nothing -> ".L" ++ show (length lbls)
  put $ Context f v (lbls ++ [lbl]) a p
  return lbl

hasLabel :: String -> State Context Bool
hasLabel lbl = do
  Context _ _ lbls _ _ <- get
  return $ lbl `elem` lbls

byIdFunc :: Id -> Func -> Bool
byIdFunc name = (== name) . (\var -> let (_, name', _) = var in name')

lookInPrev :: State Context (Maybe a) -> State Context (Maybe a)
lookInPrev f = do
  context@(Context _ _ _ _ mprev) <- get
  case mprev of
    Nothing -> return Nothing
    Just prev -> do
      put prev
      var <- f
      put context
      return var