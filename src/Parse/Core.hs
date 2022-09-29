{-# LANGUAGE LambdaCase #-}

module Parse.Core (
  Parser,
  ParseError (..),
  parse,
  string,
  digit,
  letter,
  char,
  satisfy,
) where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (isAlpha, isDigit)
import Data.Functor.Identity
import Utils (backticks)

type Parser a = ExceptT ParseError (State String) a

data ParseError = Unexpected String deriving (Eq, Show)

parse :: Parser a -> String -> (Either ParseError a, String)
parse parser = runIdentity . runStateT (runExceptT parser)

string :: String -> Parser String
string = traverse char

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

char :: Char -> Parser Char
char c = satisfy (== c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
  input <- get
  case input of
    "" -> throwError $ Unexpected "end of input"
    (c : cs)
      | pred c -> put cs >> return c
      | otherwise -> throwError . Unexpected $ "char " ++ backticks [c]
