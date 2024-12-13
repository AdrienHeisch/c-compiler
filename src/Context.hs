module Context (Context (..), new, addVar, addVars, getVar, getLabel) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (find)
import Identifier (Id)
import Type (Type)
import Type qualified (len)

type Var = (Int, Type, Id)

data Context = Context
  { vars :: [Var],
    nvar :: Int,
    nlbl :: Int,
    prev :: Maybe Context
  }
  deriving (Show)

new :: Maybe Context -> Context
new = Context [] 0 0

addVars :: [(Type, Id)] -> Context -> Context
addVars vars context = foldl (flip addVar) context vars

addVar :: (Type, Id) -> Context -> Context
addVar (ty, name) (Context vars nvar lbls prev) = Context (vars ++ [(nvar, ty, name)]) (nvar + Type.len ty) lbls prev

getVar :: Context -> Id -> Maybe (Int, Type)
getVar (Context vars _ _ _) name = case find (byId name) vars of
  Nothing -> Nothing
  Just (idx, ty, _) -> Just (idx, ty)

getLabel :: State Context Int
getLabel = do
  Context vars nvar nlbl prev <- get
  put $ Context vars nvar (nlbl + 1) prev
  return nlbl

byId :: Id -> Var -> Bool
byId name = (== name) . (\var -> let (_, _, name') = var in name')