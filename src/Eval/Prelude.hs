module Eval.Prelude (Prelude, prelude, lookup) where

import Ast
import qualified Data.Map as M
import Prelude hiding (and, not, or, succ)

type Prelude = M.Map Ident Expr

prelude :: Prelude
prelude = M.fromList $ bindings ++ churchNumerals

bindings :: [(Ident, Expr)]
bindings =
  [ (Ident "succ", succ)
  , (Ident "plus", plus)
  , (Ident "true", true)
  , (Ident "false", false)
  , (Ident "and", and)
  , (Ident "or", or)
  , (Ident "not", not)
  , (Ident "if", if')
  , (Ident "zero?", zerop)
  , (Ident "cons", cons)
  , (Ident "fst", fst')
  , (Ident "snd", snd')
  , (Ident "nil", nil)
  , (Ident "nil?", nilp)
  ]

churchNumerals :: [(Ident, Expr)]
churchNumerals = map (\x -> (Ident $ show x, toChurchNumeral x)) [0 .. 100]

-- Maybe use Template Haskell to make this more readable?
succ :: Expr
succ = Abst n (Abst f (Abst x (App (Var f) (App (App (Var n) (Var f)) (Var x)))))

plus :: Expr
plus = Abst m (Abst n (Abst f (Abst x (App (App (Var m) (Var f)) (App (App (Var n) (Var f)) (Var x))))))

true :: Expr
true = Abst x (Abst y (Var x))

false :: Expr
false = Abst x (Abst y (Var y))

and :: Expr
and = Abst p (Abst q (App (App (Var p) (Var q)) (Var p)))

or :: Expr
or = Abst p (Abst q (App (App (Var p) (Var p)) (Var q)))

not :: Expr
not = Abst p (App (App (Var p) false) true)

if' :: Expr
if' = Abst p (Abst a (Abst b (App (App (Var p) (Var a)) (Var b))))

zerop :: Expr
zerop = Abst n (App (App (Var n) (Abst x false)) true)

cons :: Expr
cons = Abst x (Abst y (Abst f (App (App (Var f) (Var x)) (Var y))))

fst' :: Expr
fst' = Abst p (App (Var p) true)

snd' :: Expr
snd' = Abst p (App (Var p) false)

nil :: Expr
nil = Abst x true

nilp :: Expr
nilp = Abst p (App (Var p) (Abst x (Abst y false)))

toChurchNumeral :: Integer -> Expr
toChurchNumeral n = Abst f (Abst x $ go n)
 where
  go 0 = Var x
  go n = App (Var f) (go $ n - 1)

a :: Ident
a = Ident "a"

b :: Ident
b = Ident "b"

f :: Ident
f = Ident "f"

m :: Ident
m = Ident "m"

n :: Ident
n = Ident "n"

p :: Ident
p = Ident "p"

q :: Ident
q = Ident "q"

x :: Ident
x = Ident "x"

y :: Ident
y = Ident "y"
