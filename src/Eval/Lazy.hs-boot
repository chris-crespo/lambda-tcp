module Eval.Lazy (eval) where

import Ast
import {-# SOURCE #-} Eval.Env

eval :: Decl -> Env -> (Maybe Expr, Env)
