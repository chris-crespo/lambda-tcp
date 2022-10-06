module Ast where

data Decl
  = VarDecl Ident Expr
  | ExprDecl Expr
  deriving (Eq, Show)

data Expr
  = Var Ident
  | Abst Ident Expr
  | App Expr Expr
  deriving (Eq, Show)

newtype Ident = Ident {unIdent :: String} deriving (Eq, Ord)

instance Show Ident where
  show (Ident str) = str
