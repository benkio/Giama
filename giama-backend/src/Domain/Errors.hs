module Domain.Errors (DomainError(..)) where

import           Domain.Identifiers (SceneName)

newtype DomainError = SceneNotFound SceneName deriving (Show)










