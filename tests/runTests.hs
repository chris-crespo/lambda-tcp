module Main where

import qualified Parse.CoreTests as PC
import Test.Hspec

main :: IO ()
main = hspec $ do
  PC.spec
