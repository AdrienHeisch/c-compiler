module Context (Context (..), new, addVar, addVars, getVar, addLabel, getLabel, anonLabel) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (elemIndex, find)
import Identifier (Id)
import Instruction (regLen)
import Type (Type)

type Var = (Int, Type, Id) -- TODO might be able to remove the address

data Context = Context
  { vars :: [Var],
    stlb :: [String],
    nlbl :: Int, -- TODO remove this
    prev :: Maybe Context
  }
  deriving (Show)

new :: Maybe Context -> Context
new = Context [] [] 0

addVars :: [(Type, Id)] -> Context -> Context
addVars vars context = foldl (flip addVar) context vars

addVar :: (Type, Id) -> Context -> Context
addVar (ty, name) (Context vars stlb nlbl prev) = Context (vars ++ [(length vars * regLen, ty, name)]) stlb nlbl prev

getVar :: Context -> Id -> Maybe (Int, Type)
getVar (Context vars _ _ _) name = case find (byId name) vars of
  Nothing -> Nothing
  Just (idx, ty, _) -> Just (idx, ty)

addLabel :: String -> State Context Int
addLabel lbl = do
  (Context v stlb nlbl p) <- get
  put $ Context v (stlb ++ [lbl]) (nlbl + 1) p
  return nlbl

getLabel :: Context -> String -> Maybe Int
getLabel (Context _ stlb _ _) lbl = elemIndex lbl stlb

anonLabel :: State Context Int -- TODO do this for all functions above
anonLabel = do
  Context vars stlb nlbl prev <- get
  put $ Context vars (stlb ++ [""]) (nlbl + 1) prev
  return nlbl

byId :: Id -> Var -> Bool
byId name = (== name) . (\var -> let (_, _, name') = var in name')