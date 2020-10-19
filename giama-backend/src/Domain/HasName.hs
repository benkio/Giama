module Domain.HasName where

class HasName a where
  getName :: a -> String
