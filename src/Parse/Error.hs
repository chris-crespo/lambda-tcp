module Parse.Error (ParseError (..)) where

data ParseError = Unexpected ErrorItem [ErrorItem] | Empty deriving (Eq, Show)

type ErrorItem = String

instance Semigroup ParseError where
  Empty <> err = err
  err <> Empty = err
  Unexpected str strs <> Unexpected _ strs' = Unexpected str (strs <> strs')

instance Monoid ParseError where
  mempty = Empty
