module Context (Context (..), new, newFunction, newScope, addVar, addVars, getVar, addLabel, hasLabel, makeAnonLabel, defineFunc, declareFunc) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (find, findIndex)
import Identifier (Id)
import Identifier qualified as Id (toStr)
import Instruction (regLen)
import Type (Type)
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

newFunction :: Context -> Context
newFunction prev = Context [] [] [] 0 (Just prev)

newScope :: Context -> Context
newScope prev@(Context _ vars l _ _) = Context [] [] l (length vars) (Just prev)

addVars :: [(Type, Id)] -> State Context ()
addVars vars = case vars of
  [] -> return ()
  (var : rest) -> addVar var >> addVars rest

addVar :: (Type, Id) -> State Context ()
addVar (ty, name) = do
  mvar <- findVar name False
  case mvar of
    Nothing -> do
      Context f vars l a p <- get
      put $ Context f (vars ++ [(ty, name)]) l a p
    Just _ -> error $ "Redefinition of " ++ Id.toStr name

getVar :: Id -> State Context (Maybe (Int, Type))
getVar name = findVar name True

findVar :: Id -> Bool -> State Context (Maybe (Int, Type))
findVar name doPrev = do
  (Context _ vars _ addr _) <- get
  case findIndex (byId name) vars of
    Just idx -> return $ Just ((addr + idx) * regLen, fst $ vars !! idx)
    Nothing | doPrev -> lookInPrev $ getVar name
    Nothing -> return Nothing

defineFunc :: (Type, Id) -> State Context ()
defineFunc f = _addFunc f True

declareFunc :: (Type, Id) -> State Context ()
declareFunc f = _addFunc f False

_addFunc :: (Type, Id) -> Bool -> State Context () -- TODO error handling
_addFunc (ty, name) doDefine = do
  mfunc <- getFunc name
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
getFunc name = do
  Context funcs _ _ _ _ <- get
  return $ find (byIdFunc name) funcs

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

byId :: Id -> Var -> Bool
byId name = (== name) . (\var -> let (_, name') = var in name')

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