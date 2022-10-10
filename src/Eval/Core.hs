module Eval.Core (Interpreter, runInterpreter) where

import Control.Monad.State
import Eval.Env

type Interpreter a = State Env a

runInterpreter :: Interpreter a -> Env -> (a, Env)
runInterpreter = runState
