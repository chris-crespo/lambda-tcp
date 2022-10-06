module Eval.Env (
  Env (Env),
  empty,
  bind,
  extend,
  lookup,
) where

import Ast (Expr, Ident)
import Control.Applicative ((<|>))
import qualified Data.Map as M
import Eval.Prelude (prelude)
import Prelude hiding (lookup)

-- Since both λ-calculus and Haskell are pure, we don't need to handle
-- a parent-pointer tree ‐ users will have access to both the previous
-- and new envs after adding a new binding. However, we still need
-- a way to refresh existing bindings when adding a top level declaration.

newtype Env = Env {_bindings :: M.Map Ident Expr} deriving (Eq, Show)

empty :: Env
empty = Env M.empty

bind :: Ident -> Expr -> Env -> Env
bind ident value (Env bindings) = Env (M.insert ident value bindings)

extend :: Ident -> Expr -> Env -> Env
extend ident value (Env bindings) = Env (M.insert ident value bindings)

lookup :: Ident -> Env -> Maybe Expr
lookup ident (Env bindings) = M.lookup ident bindings <|> M.lookup ident prelude
