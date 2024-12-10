module Statement (Statement (..), StatementDef (..), errs) where

import Constant (Constant, IntRepr)
import Expr (Expr)
import Expr qualified (errs)
import Identifier (Id)
import Token (Token, foldCrs)
import Type (Type (..))
import Utils (Display (..))

-- TODO change order
data Statement = Statement {def :: StatementDef, tks :: [Token]}
  deriving (Show)

instance Display Statement where
  display :: Statement -> String
  display = display . def

data StatementDef
  = Empty
  | Expr Expr
  | Var Type Id (Maybe Expr)
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
  | Invalid String
  deriving (Show)

instance Display StatementDef where
  display :: StatementDef -> String
  display statement = case statement of
    Expr e -> "Expr " ++ display e
    Var ty name e -> unwords ["Var", show ty, show name, display e]
    If e st0 st1 -> unwords ["If", display e, display st0, display st1]
    Switch e st -> unwords ["Switch", display e, display st]
    While e st -> unwords ["Switch", display e, display st]
    DoWhile st e -> unwords ["DoWhile", display st,  display e]
    For e0 e1 e2 st -> unwords ["ForVar", display e0, display e1, display e1, display e2, display st]
    ForVar st0 e0 e1 st1 -> unwords ["ForVar", display st0, display e0, display e1, display e1, display st1]
    Return e -> "Return " ++ display e
    Block block -> display block
    _ -> show statement

errs :: [Statement] -> [String]
errs = concatMap err
  where
    err :: Statement -> [String]
    err statement = case def statement of
      Expr e -> Expr.errs [e]
      Var _ _ (Just e) -> Expr.errs [e]
      If e st0 (Just st1) -> Expr.errs [e] ++ errs [st0, st1]
      Switch e st -> Expr.errs [e] ++ errs [st]
      While e st -> Expr.errs [e] ++ errs [st]
      DoWhile st e -> Expr.errs [e] ++ errs [st]
      For (Just e0) (Just e1) (Just e2) st -> Expr.errs [e0, e1, e2] ++ errs [st]
      ForVar st0 (Just e0) (Just e1) st1 -> Expr.errs [e0, e1, e1] ++ errs [st0, st1]
      Return (Just e) -> Expr.errs [e]
      Block block -> concatMap err block
      Invalid str -> [str ++ " at " ++ show (Token.foldCrs $ tks statement)]
      _ -> []