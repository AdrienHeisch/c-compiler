module Context (Context (..), new, addVar, addVars, getVar, getLabel) where

import Control.Monad.State.Lazy (State, get, put)
import Data.List (find)
import Identifier (Id)
import Type (Type)
import Instruction (regLen)

type Var = (Int, Type, Id)

data Context = Context
  { vars :: [Var],
    nlbl :: Int,
    prev :: Maybe Context
  }
  deriving (Show)

new :: Maybe Context -> Context
new = Context [] 0

addVars :: [(Type, Id)] -> Context -> Context
addVars vars context = foldl (flip addVar) context vars

addVar :: (Type, Id) -> Context -> Context
addVar (ty, name) (Context vars nlbl prev) = Context (vars ++ [(length vars * regLen, ty, name)]) nlbl prev

getVar :: Context -> Id -> Maybe (Int, Type)
getVar (Context vars _ _) name = case find (byId name) vars of
  Nothing -> Nothing
  Just (idx, ty, _) -> Just (idx, ty)

getLabel :: State Context Int
getLabel = do
  Context vars nlbl prev <- get
  put $ Context vars (nlbl + 1) prev
  return nlbl

byId :: Id -> Var -> Bool
byId name = (== name) . (\var -> let (_, _, name') = var in name')