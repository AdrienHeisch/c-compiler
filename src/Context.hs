module Context (Context (..), new, newFunction, newScope, addVar, addVars, getVar, addLabel, hasLabel, makeAnonLabel, defineFunc, declareFunc) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (find, findIndex)
import Debug.Trace (trace)
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
    prev :: Maybe Context
  }
  deriving (Show)

new :: Context
new = Context [] [] [] Nothing

newFunction :: Context -> Context
newFunction prev@(Context f v _ _) = Context f v [] (Just prev)

newScope :: Context -> Context
newScope prev@(Context f v l _) = Context f v l (Just prev)

addVars :: [(Type, Id)] -> State Context ()
addVars vars = case vars of
  [] -> return ()
  (var : rest) -> addVar var >> addVars rest

addVar :: (Type, Id) -> State Context ()
addVar (ty, name) = do
  Context f vars l p <- get
  put $ Context f (vars ++ [(ty, name)]) l p

getVar :: Id -> State Context (Maybe (Int, Type)) -- TODO error if already declared
getVar name = do
  Context _ vars _ _ <- get
  case findIndex (byId name) vars of
    Nothing -> return Nothing
    Just idx -> return $ Just (idx * regLen, fst $ vars !! idx)

defineFunc :: (Type, Id) -> State Context ()
defineFunc f = _addFunc f True

declareFunc :: (Type, Id) -> State Context ()
declareFunc f = _addFunc f False

_addFunc :: (Type, Id) -> Bool -> State Context () -- TODO error handling
_addFunc (ty, name) doDefine = do
  mfunc <- getFunc name
  trace ("doDefine: " ++ show doDefine ++ "; mfunc: " ++ show mfunc) $ case mfunc of
    Just (ty', _, _) | ty /= ty' -> error $ "Conflicting types for " ++ Id.toStr name ++ " : " ++ Type.toStr ty ++ ", already declared as " ++ Type.toStr ty'
    Just (_, _, True) | doDefine -> error $ "Redefinition of " ++ Id.toStr name
    Just (_, _, True) -> return ()
    Just (_, _, False) | doDefine -> do
      Context funcs v l p <- get
      let funcs' = modifyFirst (byIdFunc name) (\(t, n, _) -> (t, n, True)) funcs
      put $ Context funcs' v l p
      return ()
    _ -> do
      Context funcs v l p <- get
      put $ Context (funcs ++ [(ty, name, doDefine)]) v l p
      return ()

getFunc :: Id -> State Context (Maybe Func)
getFunc name = do
  Context funcs _ _ _ <- get
  return $ find (byIdFunc name) funcs

addLabel :: String -> State Context ()
addLabel lbl = do
  _ <- _addLabel $ Just lbl
  return ()

makeAnonLabel :: State Context String
makeAnonLabel = _addLabel Nothing

_addLabel :: Maybe String -> State Context String -- TODO error if already defined
_addLabel mlbl = do
  Context f v lbls p <- get
  let lbl = case mlbl of
        Just lbl' -> lbl'
        Nothing -> ".L" ++ show (length lbls)
  put $ Context f v (lbls ++ [lbl]) p
  return lbl

hasLabel :: String -> State Context Bool
hasLabel lbl = do
  Context _ _ lbls _ <- get
  return $ lbl `elem` lbls

byId :: Id -> Var -> Bool
byId name = (== name) . (\var -> let (_, name') = var in name')

byIdFunc :: Id -> Func -> Bool
byIdFunc name = (== name) . (\var -> let (_, name', _) = var in name')