module Eval.Env (Env, empty) where

import Ast (Expr, Ident)
import qualified Data.Map as M

newtype Env = Env {_bindings :: M.Map Ident Expr} 

instance Eq Env

instance Show Env

empty :: Env
