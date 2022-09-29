module Lex where

import Parse.Core

lambda :: Parser Char
lambda = backslash

dot :: Parser Char
dot = char '.'

leftParen :: Parser Char
leftParen = char '('

rightParen :: Parser Char
rightParen = char ')'

equals :: Parser Char
equals = char '='

backslash :: Parser Char
backslash = char '\\'
