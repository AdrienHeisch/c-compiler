module Context (Context (..), new, addVar, addVars, getVar, makeLabel, getLabel, makeAnonLabel) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (elemIndex, findIndex)
import Identifier (Id)
import Instruction (regLen)
import Type (Type)

type Var = (Type, Id)

data Context = Context
  { vars :: [Var],
    lbls :: [Maybe String],
    prev :: Maybe Context
  }
  deriving (Show)

new :: Maybe Context -> Context
new = Context [] []

addVars :: [(Type, Id)] -> State Context ()
addVars vars = case vars of
  [] -> return ()
  (var : rest) -> addVar var >> addVars rest

addVar :: (Type, Id) -> State Context ()
addVar (ty, name) = do
  (Context vars lbls prev) <- get
  put $ Context (vars ++ [(ty, name)]) lbls prev

getVar :: Context -> Id -> Maybe (Int, Type)
getVar (Context vars _ _) name = case findIndex (byId name) vars of
  Nothing -> Nothing
  Just idx -> Just (idx * regLen, fst $ vars !! idx)

makeLabel :: String -> State Context Int
makeLabel lbl = _addLabel $ Just lbl

makeAnonLabel :: State Context Int
makeAnonLabel = _addLabel Nothing

_addLabel :: Maybe String -> State Context Int
_addLabel lbl = do
  Context v lbls p <- get
  put $ Context v (lbls ++ [lbl]) p
  return $ length lbls

getLabel :: String -> State Context (Maybe Int)
getLabel lbl = do
  Context _ lbls _ <- get
  return $ elemIndex (Just lbl) lbls

byId :: Id -> Var -> Bool
byId name = (== name) . (\var -> let (_, name') = var in name')