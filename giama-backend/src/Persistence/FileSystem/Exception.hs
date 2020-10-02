module Persistence.FileSystem.Exception (FileSystemException(..)) where

import           Control.Exception (Exception)

newtype FileSystemException = ProjectNotFound String deriving (Show)

instance Exception FileSystemException
