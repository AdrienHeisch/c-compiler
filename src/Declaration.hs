module Declaration (Declaration (..), DeclarationDef (..), errs) where

import Expr (Expr)
import Identifier (Id)
import Statement (Statement)
import Statement qualified (errs)
import Type (Type (..))
import Token (Token, foldCrs)
import Utils (Display (..))

data Declaration = Declaration {def :: DeclarationDef, tks :: [Token]}
  deriving (Show)

instance Display Declaration where
  display :: Declaration -> String
  display = show . def

data DeclarationDef
  = FuncDef Type Id [(Type, Id)]
  | FuncDec Type Id [(Type, Id)] [Statement]
  | Global
  | Static
  | Struct (Maybe Id) [(Type, Id)]
  | -- TODO enforce constants in enum / replace with underlying type at parsing and remove this
    Enum (Maybe Id) Type [(Id, Maybe Expr)]
  | Invalid String
  deriving (Show)

errs :: [Declaration] -> [String]
errs = concatMap err
  where
    err :: Declaration -> [String]
    err decl = case def decl of
      FuncDec _ _ _ sts -> Statement.errs sts
      -- Enum _ _ [(Id, Maybe Expr)] -> []
      Invalid str -> [str ++ " at " ++ show (Token.foldCrs $ tks decl)]
      _ -> []
