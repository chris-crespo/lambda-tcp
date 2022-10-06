{-# LANGUAGE LambdaCase #-}

module Eval.Lazy (eval) where

import Ast
import Control.Applicative ((<|>))
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Eval.Core
import Eval.Env (Env)
import qualified Eval.Env as Env

eval :: Decl -> Env -> (Maybe Expr, Env)
eval decl = runInterpreter $ evalDecl decl

evalDecl :: Decl -> Interpreter Expr
evalDecl (VarDecl ident expr) = evalVarDecl ident expr
evalDecl (ExprDecl expr) = evalExpr expr

evalVarDecl :: Ident -> Expr -> Interpreter Expr
evalVarDecl ident expr = do
  env <- get
  expr' <- evalExpr expr
  put $ Env.bind ident expr' env
  MaybeT . pure $ Nothing

evalExpr :: Expr -> Interpreter Expr
evalExpr = (etaReduce <$>) . normalForm

normalForm :: Expr -> Interpreter Expr
normalForm (Var ident) = deltaReduce ident <|> return (Var ident)
normalForm (Abst param body) = Abst param <$> normalForm body
normalForm (App e1 e2) =
  weakNormalForm e1 >>= \case
    Abst param body -> normalForm (substitute param e2 body)
    e1' -> do
      e1'' <- normalForm e1'
      if e1' == e1''
        then App e1' <$> normalForm e2
        else normalForm $ App e1'' e2

weakNormalForm :: Expr -> Interpreter Expr
weakNormalForm (Var ident) = deltaReduce ident <|> return (Var ident)
weakNormalForm (Abst param body) = return $ Abst param body
weakNormalForm (App e1 e2) = case e1 of
  Abst param body -> weakNormalForm (substitute param e2 body)
  _ -> return $ App e1 e2

-- Using `(MaybeT . pure)` instead of `hoistMaybe` because it isn't exported
deltaReduce :: Ident -> Interpreter Expr
deltaReduce ident = get >>= MaybeT . pure . Env.lookup ident

etaReduce :: Expr -> Expr
etaReduce (Abst param (App left (Var ident)))
  | param == ident && not (isFree param left) = App left (Var ident)
etaReduce expr = expr

isFree :: Ident -> Expr -> Bool
isFree ident (Var ident') = ident /= ident'
isFree ident (Abst param body) = ident == param || isFree ident body
isFree ident (App left right) = isFree ident left && isFree ident right

substitute :: Ident -> Expr -> Expr -> Expr
substitute ident subs expr = case expr of
  (Var ident') | ident == ident' -> subs
  (Abst param body) | ident /= param -> Abst param (substitute ident subs body)
  (App left right) -> App (substitute ident subs left) (substitute ident subs right)
  _ -> expr
