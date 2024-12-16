module Statement (Statement (..), StatementDef (..), isTopLevel, errs) where

import Constant (Constant, IntRepr)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, mapMaybe)
import Expr (Expr)
import Expr qualified (errs)
import Identifier (Id)
import Token (Token, foldCrs)
import Type (Type)
import Utils (Display (..))

-- TODO change order
data Statement = Statement {def :: StatementDef, tks :: [Token]}
  deriving (Show)

instance Display Statement where
  display :: Statement -> String
  display = display . def

data StatementDef
  = Empty
  | FuncDef Type Id [(Type, Maybe Id)]
  | FuncDec Type Id [(Type, Maybe Id)] [Statement]
  | Struct (Maybe Id) [(Type, Id)]
  | Union (Maybe Id) [(Type, Id)]
  | Enum (Maybe Id) Type [(Id, Maybe Expr)] -- TODO enforce constants in enum / replace with underlying type at parsing and remove this
  | Typedef Type Id
  | Expr Expr
  | Var (NonEmpty (Type, Id, Maybe Expr))
  | Block [Statement]
  | If {cond :: Expr, then_ :: Statement, else_ :: Maybe Statement}
  | Switch {eval :: Expr, body :: Statement}
  | While {cond :: Expr, body :: Statement}
  | DoWhile {body :: Statement, cond :: Expr}
  | For {finit :: Maybe Expr, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement}
  | ForVar {fdecl :: Statement, fcond :: Maybe Expr, fincr :: Maybe Expr, fbody :: Statement}
  | Break
  | Continue
  | Return (Maybe Expr)
  | Case (Constant IntRepr)
  | Goto Id
  | Labeled Id Statement
  | Ambiguous (Statement, Statement)
  | Invalid String
  deriving (Show)

isTopLevel :: StatementDef -> Bool
isTopLevel def = case def of
  FuncDef {} -> True
  FuncDec {} -> True
  Struct {} -> True
  Union {} -> True
  Enum {} -> True
  Typedef {} -> True
  Var {} -> True
  Invalid {} -> True
  _ -> False

instance Display StatementDef where
  display :: StatementDef -> String
  display statement = case statement of
    FuncDec ty name params sts -> unwords ["FuncDec", show ty, show name, show params, display sts]
    Enum name ty vars -> unwords ["Enum", show name, show ty, displayVariants vars]
    Expr e -> "Expr " ++ display e
    Var (var :| vars) -> unwords ("Var" : map displayVar (var : vars))
    If e st0 st1 -> unwords ["If", display e, display st0, display st1]
    Switch e st -> unwords ["Switch", display e, display st]
    While e st -> unwords ["While", display e, display st]
    DoWhile st e -> unwords ["DoWhile", display st, display e]
    For e0 e1 e2 st -> unwords ["ForVar", display e0, display e1, display e1, display e2, display st]
    ForVar st0 e0 e1 st1 -> unwords ["ForVar", display st0, display e0, display e1, display e1, display st1]
    Return e -> "Return " ++ display e
    Block block -> "Block " ++ display block
    _ -> show statement
    where
      displayVar (ty, name, e) = "(" ++ unwords [show ty, show name, display e] ++ ")"

      displayVariants [] = ""
      displayVariants [(name, ex)] = unwords [show name, display ex]
      displayVariants (var : vars) = displayVariants [var] ++ displayVariants vars

errs :: [Statement] -> [String]
errs = concatMap err
  where
    err :: Statement -> [String]
    err statement = case def statement of
      FuncDec _ _ _ sts -> Statement.errs sts
      Expr e -> Expr.errs [e]
      Var ((_, _, ex) :| vars) -> Expr.errs (catMaybes [ex]) ++ Expr.errs (mapMaybe (\(_, _, e) -> e) vars)
      If e st0 st1 -> Expr.errs [e] ++ errs (catMaybes [Just st0, st1])
      Switch e st -> Expr.errs [e] ++ errs [st]
      While e st -> Expr.errs [e] ++ errs [st]
      DoWhile st e -> Expr.errs [e] ++ errs [st]
      For e0 e1 e2 st -> Expr.errs (catMaybes [e0, e1, e2]) ++ errs [st]
      ForVar st0 e0 e1 st1 -> Expr.errs (catMaybes [e0, e1]) ++ errs [st0, st1]
      Return e -> Expr.errs (catMaybes [e])
      Block block -> concatMap err block
      Invalid str -> [str ++ " at " ++ show (Token.foldCrs $ tks statement)]
      _ ->  []