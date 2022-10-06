module Eval.Core where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Eval.Env

-- newtype Interpreter a = Interpreter {_runInterpreter :: MaybeT (State Env) a}

-- instance Functor Interpreter where
--   fmap f (Interpreter i) = Interpreter $ fmap f i
--
-- instance Applicative Interpreter where
--   pure x = Interpreter $ pure x
--   (Interpreter f) <*> (Interpreter i) = Interpreter $ f <*> i
--
-- instance Monad Interpreter where
--   i >>= k = Interpreter $ do
--     v <- _runInterpreter i
--     _runInterpreter $ k v
--
-- runInterpreter :: Interpreter a -> Env -> (Maybe a, Env)
-- runInterpreter = runState . runMaybeT . _runInterpreter
--

type Interpreter a = MaybeT (State Env) a

runInterpreter :: Interpreter a -> Env -> (Maybe a, Env)
runInterpreter = runState . runMaybeT
