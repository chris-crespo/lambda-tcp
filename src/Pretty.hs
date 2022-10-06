module Pretty where

import Ast
import Utils

pretty :: Expr -> String
pretty (Var ident) = show ident
pretty (Abst param body) = "\\" ++ show param ++ "." ++ pretty body
pretty (App left right) = case (left, right) of
  (Abst{}, App{}) -> parenthesize (pretty left) ++ " " ++ parenthesize (pretty right)
  (Abst{}, _) -> parenthesize (pretty left) ++ " " ++ pretty right
  (_, App{}) -> pretty left ++ " " ++ parenthesize (pretty right)
  _ -> pretty left ++ " " ++ pretty right
