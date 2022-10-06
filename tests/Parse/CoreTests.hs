module Parse.CoreTests (spec) where

import Data.Char (isDigit)
import Parse.Core
import Parse.Error
import Parse.Utils
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Parse.Core" $ do
    describe "satisfy" $ do
      context "when input is empty" $ do
        it "fails" $ do
          parse (satisfy $ const True) "" `shouldFailWith` Unexpected "end of input" []

      context "when input satisfies predicate" $ do
        it "parses the input" $ do
          parse (satisfy isDigit) "2" `shouldParse` '2'

        it "leaves the rest of the input" $ do
          parse (satisfy isDigit) "2ab" `successLeaving` "ab"

      context "when input does not satisfy the predicate" $ do
        it "fails" $ do
          parse (satisfy isDigit) "ab" `shouldFailWith` Unexpected "char `a`" []

        it "leaves the input untouched" $ do
          parse (satisfy isDigit) "ab" `failsLeaving` "ab"

    describe "many" $ do
      it "runs a parser until it fails and returns the accumulated results" $ do
        parse (many digit) "123a" `shouldParse` "123"

    describe "lexeme" $ do
      it "skips leading spaces before running a given parser" $ do
        parse (lexeme $ char 'c') "  c" `shouldParse` 'c'

    describe "symbol" $ do
      it "skips leading spaces before parsing a given string" $ do
        parse (symbol ".") "  ." `shouldParse` "."
