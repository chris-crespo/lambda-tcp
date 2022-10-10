{-# LANGUAGE LambdaCase #-}

module Parse.Core (
  Parser,
  ParseError (..),
  parse,
  string,
  some,
  many,
  spaces,
  lexeme,
  symbol,
  digit,
  alpha,
  alphaNum,
  char,
  label,
  try,
  satisfy,
) where

import Control.Applicative (liftA2, (<|>))
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isAlpha, isDigit)
import Data.Functor.Identity
import Parse.Error
import Utils (backticks)

type Parser a = ExceptT ParseError (State String) a

parse :: Parser a -> String -> Either ParseError a
parse parser = fst . runParser parser

runParser :: Parser a -> String -> (Either ParseError a, String)
runParser parser = runIdentity . runStateT (runExceptT parser)

string :: String -> Parser String
string = traverse char

some :: Parser a -> Parser [a]
some p = liftA2 (:) p (many p)

many :: Parser a -> Parser [a]
many p = liftA2 (:) p (many p) <|> return []

spaces :: Parser ()
spaces = void $ many space

lexeme :: Parser a -> Parser a
lexeme p = spaces >> p

symbol :: String -> Parser String
symbol str = spaces >> string str

digit :: Parser Char
digit = label "digit" $ satisfy isDigit

alpha :: Parser Char
alpha = label "alpha" $ satisfy isAlpha

alphaNum :: Parser Char
alphaNum = label "alphaNum" $ digit <|> alpha

space :: Parser ()
space = void . label "space" $ char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

char :: Char -> Parser Char
char c = label ("char " ++ backticks [c]) $ satisfy (== c)

label :: String -> Parser a -> Parser a
label str p = flip withExceptT p $ \case
  Empty -> Empty
  Unexpected value _ -> Unexpected value [str]

try :: Parser a -> Parser a
try p =
  get >>= \input -> case runParser p input of
    (Left err, _) -> put input >> throwError err
    (Right value, input') -> put input' >> return value

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred =
  get >>= \case
    "" -> throwError $ Unexpected "end of input" []
    (c : cs)
      | pred c -> put cs >> return c
      | otherwise -> throwError $ Unexpected ("char " ++ backticks [c]) []
