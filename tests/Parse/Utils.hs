module Parse.Utils (
  shouldFailWith,
  shouldParse,
  successLeaving,
  failsLeaving,
) where

import Control.Monad (unless)
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
