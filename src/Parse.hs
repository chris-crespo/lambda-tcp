module Parse (
  parse,
  decl,
  expr,
) where

import Ast
import Control.Applicative ((<|>))
import Parse.Core
import Parse.Lex

decl :: Parser Decl
decl = varDecl <|> exprDecl

varDecl :: Parser Decl
varDecl = VarDecl <$> try (ident <* equals) <*> expr

exprDecl :: Parser Decl
exprDecl = ExprDecl <$> expr

expr :: Parser Expr
expr = app

app :: Parser Expr
app = primary >>= go
 where
  go :: Expr -> Parser Expr
  go acc = (primary >>= go . App acc) <|> return acc

primary :: Parser Expr
primary = group <|> var <|> abst

group :: Parser Expr
group = parens expr

var :: Parser Expr
var = Var <$> ident

abst :: Parser Expr
abst = Abst <$> (lambda *> ident <* dot) <*> expr
