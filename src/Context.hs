module Context (Context (..), new, addVar, getVar) where

import Identifier (Id)
import Type (Type)
import Data.List (findIndex)

type Var = (Type, Id)

data Context = Context
  { vars :: [Var],
    prev :: Maybe Context
  }
  deriving (Show)

new :: Context
new = Context [] Nothing

addVar :: Type -> Id -> Context -> Context
addVar ty name (Context vars prev) = Context (vars ++ [(ty, name)]) prev

getVar :: Context -> Id -> Maybe (Int, Type)
getVar (Context vars _) name = case findIndex (byId name) vars of
  Nothing -> Nothing
  Just idx -> let (ty, _) = vars !! idx in Just (idx, ty)

byId :: Id -> Var -> Bool
byId name = (== name) . snd