{-# LANGUAGE LambdaCase #-}

module Eval.Lazy (eval) where

import Ast
import Control.Monad.State.Lazy
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Eval.Core
import Eval.Env (Env)
import qualified Eval.Env as Env

eval :: Decl -> Env -> (Expr, Env)
eval decl = runInterpreter $ evalDecl decl

evalDecl :: Decl -> Interpreter Expr
evalDecl (VarDecl ident expr) = evalVarDecl ident expr
evalDecl (ExprDecl expr) = evalExpr expr

evalVarDecl :: Ident -> Expr -> Interpreter Expr
evalVarDecl ident expr = do
  env <- get
  expr' <- evalExpr expr
  put $ Env.bind ident expr' env
  return (Var ident)

evalExpr :: Expr -> Interpreter Expr
evalExpr = (etaReduce <$>) . normalForm []

normalForm :: [Ident] -> Expr -> Interpreter Expr
normalForm bound (Var ident)
  | ident `elem` bound = return (Var ident)
  | otherwise = deltaReduce ident
normalForm bound (Abst param body) = Abst param <$> normalForm (param : bound) body
normalForm bound (App e1 e2) =
  weakNormalForm bound e1 >>= \case
    Abst param body -> normalForm (param : bound) (substitute param e2 body)
    e1' -> do
      e1'' <- normalForm bound e1'
      if e1' == e1''
        then return $ App e1' e2
        else normalForm bound $ App e1'' e2

weakNormalForm :: [Ident] -> Expr -> Interpreter Expr
weakNormalForm bound (Var ident)
  | ident `elem` bound = return $ Var ident
  | otherwise = deltaReduce ident
weakNormalForm bound (Abst param body) = return $ Abst param body
weakNormalForm bound (App e1 e2) = case e1 of
  Abst param body -> weakNormalForm (param : bound) (substitute param e2 body)
  _ -> return $ App e1 e2

deltaReduce :: Ident -> Interpreter Expr
deltaReduce ident = get <&> fromMaybe (Var ident) . Env.lookup ident

etaReduce :: Expr -> Expr
etaReduce (Abst param (App left (Var ident)))
  | param == ident && isFree param left = left
etaReduce expr = expr

isFree :: Ident -> Expr -> Bool
isFree ident (Var ident') = True
isFree ident (Abst param body) = ident == param || isFree ident body
isFree ident (App left right) = isFree ident left && isFree ident right

substitute :: Ident -> Expr -> Expr -> Expr
substitute ident subs expr = case expr of
  (Var ident') | ident == ident' -> subs
  (Abst param body)
    | ident /= param ->
      let subsFreeVars = freeVars subs
       in if param `elem` subsFreeVars
            then
              let renamed = renameWhile (not . flip elem subsFreeVars) param
               in Abst renamed $ substitute ident subs (substitute param (Var renamed) body)
            else Abst param (substitute ident subs body)
  (App left right) -> App (substitute ident subs left) (substitute ident subs right)
  _ -> expr

freeVars :: Expr -> [Ident]
freeVars = go []
 where
  go bound (Var ident) = [ident | ident `notElem` bound]
  go bound (Abst param body) = go (param : bound) body
  go bound (App e1 e2) = go bound e1 ++ go bound e2

renameWhile :: (Ident -> Bool) -> Ident -> Ident
renameWhile pred ident = if pred renamed then renamed else renameWhile pred renamed
 where
  renamed = rename ident

rename :: Ident -> Ident
rename (Ident ident) = Ident $ ident ++ "'"
