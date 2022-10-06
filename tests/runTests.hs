module Main where

import qualified Eval.LazyTests as EL
import qualified Parse.CoreTests as PC
import qualified ParseTests as P
import Test.Hspec

main :: IO ()
main = hspec $ do
  EL.spec
  P.spec
  PC.spec
