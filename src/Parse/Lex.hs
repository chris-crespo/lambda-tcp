module Parse.Lex (
  lambda,
  dot,
  parens,
  equals,
  ident,
) where

import Ast
import Control.Applicative ((<|>))
import Parse.Core

lambda :: Parser String
lambda = backslash

dot :: Parser String
dot = symbol "."

parens :: Parser a -> Parser a
parens p = leftParen *> p <* rightParen

leftParen :: Parser String
leftParen = symbol "("

rightParen :: Parser String
rightParen = symbol ")"

equals :: Parser String
equals = symbol "="

backslash :: Parser String
backslash = symbol "\\"

ident :: Parser Ident
ident = Ident <$> lexeme (some $ alphaNum <|> question)

question :: Parser Char
question = char '?'
