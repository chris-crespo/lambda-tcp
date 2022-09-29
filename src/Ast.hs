module Ast where

data Decl 
  = VarDecl Ident Expr
  | ExprDecl Expr
  deriving Eq

instance Show Decl where
  show (VarDecl ident expr) = show ident ++ " = " ++ show expr
  show (ExprDecl expr) = show expr

data Expr 
  = Var Ident
  | Lambda Ident Expr
  | App Expr Expr
  deriving Eq

instance Show Expr where
  show (Var ident) = show ident
  show (Lambda param expr) = "λ" ++ show param ++ "." ++ show expr
  show (App expr1 expr2) = parenthesize (show expr1) ++ parenthesize (show expr2)

parenthesize :: String -> String
parenthesize str = "(" ++ str ++ ")"

newtype Ident = Ident { unIdent :: String } deriving Eq

instance Show Ident where
  show (Ident str) = str

newtype Param = Param { unParam :: String } deriving Eq

instance Show Param where
  show (Param str) = str
