module Persistence.FileSystem.DirectoryFunctions (applyDirWithResult) where

import           Control.Exception (Exception, try)
import           System.Path       (Absolute, Path)

applyDirWithResult :: Exception e => a -> (Path Absolute -> IO ()) -> Path Absolute -> IO (Either e a)
applyDirWithResult x f = fmap (fmap (const x)) . try . f
