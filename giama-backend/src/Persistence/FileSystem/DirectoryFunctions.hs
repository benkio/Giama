module Persistence.FileSystem.DirectoryFunctions (applyDirWithResult) where

import           System.Path (Absolute, Path)

applyDirWithResult :: a -> (Path Absolute -> IO ()) -> Path Absolute -> IO a
applyDirWithResult x f p = do
  f p
  return x
