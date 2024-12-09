module Declaration (Declaration (..), errs) where

import Identifier (Id)
import Statement (Statement)
import Statement qualified (errs)
import Type (Type (..))
import Expr (Expr)

data Declaration
  = FuncDef Type Id [(Type, Id)]
  | FuncDec Type Id [(Type, Id)] [Statement]
  | Global
  | Static
  | Struct (Maybe Id) [(Type, Id)]
  -- TODO enforce constants in enum
  | Enum (Maybe Id) Type [(Id, Maybe Expr)]
  | Invalid String
  deriving (Show)

errs :: [Declaration] -> [String]
errs = concatMap err
  where
    err :: Declaration -> [String]
    err decl = case decl of
      FuncDec _ _ _ sts -> Statement.errs sts
      -- Enum _ _ [(Id, Maybe Expr)] -> []
      Invalid str -> [str]
      _ -> []
