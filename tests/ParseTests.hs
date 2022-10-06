module ParseTests where

import Ast
import Parse
import Parse.Utils
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Parse" $ do
    describe "expr" $ do
      it "parses a variable" $ do
        parse expr "x" `shouldParse` Var (Ident "x")

      it "parses an abstraction" $ do
        parse expr "\\x.x" `shouldParse` Abst (Ident "x") (Var $ Ident "x")

      it "parses a group" $ do
        parse expr "(x)" `shouldParse` Var (Ident "x")

      it "parses an application" $ do
        let var = Var (Ident "x")
        let expected = App (App (Abst (Ident "x") var) var) var
        parse expr "(\\x.x)x x" `shouldParse` expected

    describe "decl" $ do
      it "parses a variable" $ do
        parse decl "x" `shouldParse` ExprDecl (Var $ Ident "x")

      it "parses a variable declaration" $ do
        let x = Ident "x"
        let f = Ident "f"
        let expected = VarDecl (Ident "one") (Abst f (Abst x (App (Var f) (App (Var f) (Var x)))))
        parse decl "one = \\f.\\x.f(f x)" `shouldParse` expected
