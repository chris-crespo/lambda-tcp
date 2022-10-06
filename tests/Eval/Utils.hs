module Eval.Utils (
  shouldEvalTo,
  shouldBind,
) where

import Ast (Expr, Ident)
import Control.Monad (unless)
import Eval.Env (Env)
import qualified Eval.Env as Env
import Test.Hspec
import Utils

shouldEvalTo :: HasCallStack => (Maybe Expr, Env) -> Expr -> Expectation
result `shouldEvalTo` expected = case fst result of
  Nothing ->
    expectationFailure $
      "expected to evaluate to: "
        ++ backticks (show expected)
        ++ " but it evaluated to nothing"
  Just given ->
    unless (given == expected)
      . expectationFailure
      $ "expected to evaluate to: "
        ++ backticks (show expected)
        ++ " but it evaluated to: "
        ++ backticks (show given)

shouldBind :: HasCallStack => (Maybe Expr, Env) -> (Ident, Expr) -> Expectation
(_, env) `shouldBind` (ident, expected) = case Env.lookup ident env of
  Nothing ->
    expectationFailure $
      concat
        [ "expected to bind "
        , show ident
        , " to "
        , show expected
        , " but it got bound to nothing"
        ]
  Just given ->
    unless (given == expected)
      . expectationFailure
      $ concat
        [ "expected to bind "
        , show ident
        , " to "
        , show expected
        , " but it got bound to "
        , show given
        ]
