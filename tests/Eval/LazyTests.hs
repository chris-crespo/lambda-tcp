module Eval.LazyTests (spec) where

import Ast
import Eval.Env (Env)
import qualified Eval.Env as Env
import Eval.Lazy
import Eval.Utils
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Eval.Lazy" $ do
    context "var" $ do
      context "when it's bound" $ do
        it "evaluates to the binding's value" $ do
          let x = Var $ Ident "x"
          let y = Var $ Ident "y"
          let env = Env.bind (Ident "x") y Env.empty
          eval (ExprDecl x) env `shouldEvalTo` y

      context "when it's unbound" $ do
        it "leaves the expression unchanged" $ do
          let x = Var $ Ident "x"
          eval (ExprDecl x) Env.empty `shouldEvalTo` x

    context "abst" $ do
      context "when it's not an η-redex" $ do
        it "evaluates the leftmost outermost β-redex" $ do
          let body = App (Abst (Ident "x") (Var $ Ident "x")) (Var $ Ident "y")
          let abst = Abst (Ident "x") body
          eval (ExprDecl abst) Env.empty `shouldEvalTo` Abst (Ident "x") (Var $ Ident "y")

      context "when it's an η-redex" $ do
        it "η-reduces the expression " $ do
          let expected = Abst (Ident "x") (Var $ Ident "x")
          let body = App expected (Var $ Ident "x")
          let abst = Abst (Ident "x") body
          eval (ExprDecl abst) Env.empty `shouldEvalTo` expected

    context "app" $ do
      context "when it's not a β-redex" $ do
        it "evaluates to the same expression" $ do
          let expr = App (Var $ Ident "x") (Var $ Ident "x")
          eval (ExprDecl expr) Env.empty `shouldEvalTo` expr

      context "when it's a β-redex" $ do
        it "β-reduces the expression" $ do
          let abst = Abst (Ident "x") (Var $ Ident "x")
          let app = App abst (Var $ Ident "x")
          eval (ExprDecl app) Env.empty `shouldEvalTo` Var (Ident "x")

    context "decl" $ do
      it "binds an identifier to an evaluated expression" $ do
        let expr = App (Abst (Ident "x") (Var $ Ident "x")) (Var $ Ident "y")
        eval (VarDecl (Ident "x") expr) Env.empty `shouldBind` (Ident "x", Var $ Ident "y")

      it "shadows a previous binding" $ do
        let ident = Ident "x"
        let prev = Env.bind ident (Var $ Ident "he") Env.empty
        let var = Var $ Ident "y"
        eval (VarDecl (Ident "x") var) prev `shouldBind` (Ident "x", var)
