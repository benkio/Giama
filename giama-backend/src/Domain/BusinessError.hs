module Domain.BusinessError (BusinessError(..)) where

import           Control.Exception (Exception)

data BusinessError =
  ProjectNotFound
  | SceneNotFound
  | ActNotFound

instance Show BusinessError where
  show ProjectNotFound = "Project not found"
  show SceneNotFound   = "Scene not found"
  show ActNotFound     = "Act not found"

instance Exception BusinessError --I can combine it with all other exceptions
