module Eval.Prelude (Prelude, prelude, lookup) where

import Ast
import qualified Data.Map as M
import Prelude hiding (succ)

type Prelude = M.Map Ident Expr

prelude :: Prelude
prelude = M.fromList bindings

bindings :: [(Ident, Expr)]
bindings =
  [ (Ident "one", one)
  , (Ident "succ", succ)
  , (Ident "plus", plus)
  ]

-- Maybe use Template Haskell to make this more readable?

one :: Expr
one = Abst f (Abst x (App (Var f) (Var x)))
  where 
    f = Ident "f"
    x = Ident "x"

succ :: Expr
succ = Abst n (Abst f (Abst x (App (Var f) (App (App (Var n) (Var f)) (Var x)))))
 where
  n = Ident "n"
  f = Ident "f"
  x = Ident "x"

plus :: Expr
plus = Abst m (Abst n (Abst f (Abst x (App (App (Var m) (Var f)) (App (App (Var n) (Var f)) (Var x))))))
 where
  m = Ident "m"
  n = Ident "n"
  f = Ident "f"
  x = Ident "x"
