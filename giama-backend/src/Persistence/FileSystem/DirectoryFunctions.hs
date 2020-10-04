module Persistence.FileSystem.DirectoryFunctions (applyDirWithResult) where

import           Control.Exception (Exception, try)

applyDirWithResult :: Exception e => a -> (FilePath -> IO ()) -> FilePath -> IO (Either e a)
applyDirWithResult x f = fmap (fmap (const x)) . try . f
