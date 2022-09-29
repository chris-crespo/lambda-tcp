module Parse.CoreTests (spec) where

import Control.Monad (unless)
import Data.Char (isDigit)
import Parse.Core
import Test.Hspec
import Utils

shouldFailWith :: (HasCallStack, Show a) => (Either ParseError a, String) -> ParseError -> Expectation
result `shouldFailWith` expected = case result of
  (Left given, _) ->
    unless (given == expected)
      . expectationFailure
      $ "expected to fail with " ++ backticks (show expected)
        ++ " but it failed with "
        ++ backticks (show given)
  (Right value, _) -> expectationFailure $ "expected to fail, but parsed: " ++ show value

shouldParse :: (HasCallStack, Show a, Eq a) => (Either ParseError a, String) -> a -> Expectation
result `shouldParse` expected = case result of
  (Left err, _) ->
    expectationFailure $
      "expected to parse "
        ++ backticks (show expected)
        ++ " but it failed with "
        ++ backticks (show err)
  (Right given, _) ->
    unless (given == expected)
      . expectationFailure
      $ "expected to parse "
        ++ backticks (show expected)
        ++ " but it parsed "
        ++ backticks (show given)

successLeaving :: HasCallStack => (Either ParseError a, String) -> String -> Expectation
result `successLeaving` expected = case result of
  (Left err, _) ->
    expectationFailure $
      "expected to succeed but it failed with " ++ backticks (show err)
  (Right _, given) ->
    unless (given == expected)
      . expectationFailure
      $ "expected to leave "
        ++ backticks (show expected)
        ++ " but it left "
        ++ backticks (show given)

failsLeaving :: (HasCallStack, Show a) => (Either ParseError a, String) -> String -> Expectation
result `failsLeaving` expected = case result of
  (Left _, given) ->
    unless (given == expected)
      . expectationFailure
      $ "expected to leave "
        ++ backticks (show expected)
        ++ " but it left "
        ++ backticks (show given)
  (Right value, _) ->
    expectationFailure $
      "expected to fail but it parsed " ++ backticks (show value)

spec :: SpecWith ()
spec = do
  describe "Parse.Core" $ do
    describe "satisfy" $ do
      context "when input is empty" $ do
        it "fails" $ do
          parse (satisfy $ const True) "" `shouldFailWith` Unexpected "end of input"

      context "when input satisfies predicate" $ do
        it "parses the input" $ do
          parse (satisfy isDigit) "2" `shouldParse` '2'

        it "leaves the rest of the input" $ do
          parse (satisfy isDigit) "2ab" `successLeaving` "ab"

      context "when input does not satisfy the predicate" $ do
        it "fails" $ do
          parse (satisfy isDigit) "ab" `shouldFailWith` Unexpected "char `a`"

        it "leaves the input untouched" $ do
          parse (satisfy isDigit) "ab" `failsLeaving` "ab"
