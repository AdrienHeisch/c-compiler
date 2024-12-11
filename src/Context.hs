module Context (Context (..), new, addVar, addVars, getVar) where

import Identifier (Id)
import Type (Type)
import Data.List (findIndex)

type Var = (Type, Id)

data Context = Context
  { vars :: [Var],
    lbls :: Int,
    -- labels :: []
    prev :: Maybe Context
  }
  deriving (Show)

new :: Maybe Context -> Context
new = Context [] 0

addVars :: [(Type, Id)] -> Context -> Context
addVars vars context = foldl (flip addVar) context vars

addVar :: (Type, Id) -> Context -> Context
addVar (ty, name) (Context vars lbls prev) = Context (vars ++ [(ty, name)]) lbls prev

getVar :: Context -> Id -> Maybe (Int, Type)
getVar (Context vars _ _) name = case findIndex (byId name) vars of
  Nothing -> Nothing
  Just idx -> let (ty, _) = vars !! idx in Just (idx, ty)

byId :: Id -> Var -> Bool
byId name = (== name) . snd