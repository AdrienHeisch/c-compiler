module Context (Context (..), new, newFunction, newScope, addVar, addVars, getVar, makeLabel, getLabel, makeAnonLabel) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (elemIndex, findIndex)
import Identifier (Id)
import Instruction (regLen)
import Type (Type)

type Var = (Type, Id)
type Func = (Type, Id, Bool)

data Context = Context
  { funcs :: [Maybe Func],
    vars :: [Var],
    lbls :: [Maybe String],
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
  (Context f vars l p) <- get
  put $ Context f (vars ++ [(ty, name)]) l p

getVar :: Context -> Id -> Maybe (Int, Type)
getVar (Context _ vars _ _) name = case findIndex (byId name) vars of
  Nothing -> Nothing
  Just idx -> Just (idx * regLen, fst $ vars !! idx)

makeLabel :: String -> State Context Int
makeLabel lbl = _addLabel $ Just lbl

makeAnonLabel :: State Context Int
makeAnonLabel = _addLabel Nothing

_addLabel :: Maybe String -> State Context Int
_addLabel lbl = do
  Context f v lbls p <- get
  put $ Context f v (lbls ++ [lbl]) p
  return $ length lbls

getLabel :: String -> State Context (Maybe Int)
getLabel lbl = do
  Context _ _ lbls _ <- get
  return $ elemIndex (Just lbl) lbls

byId :: Id -> Var -> Bool
byId name = (== name) . (\var -> let (_, name') = var in name')