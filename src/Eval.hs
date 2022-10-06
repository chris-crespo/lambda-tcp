module Eval (EvalMode (..), eval) where

import Ast
import Eval.Core
import Eval.Env
import qualified Eval.Lazy as L

data EvalMode = Lazy | Eager deriving (Eq, Show)

eval :: EvalMode -> Decl -> Env -> (Maybe Expr, Env)
eval Lazy = L.eval
eval Eager = undefined
