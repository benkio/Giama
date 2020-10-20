{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Removable (Removable(..)) where

import           Control.Exception                         (Exception)
import           Domain.Act                                (Act (..))
import           Domain.Project                            (Project (..))
import           Domain.Scene                              (Scene (..))
import           Persistence.FileSystem.DirectoryFunctions (applyDirWithResult)
import           Persistence.FileSystem.HasFilePath        (HasFilePath (..))
import           System.Path.IO                            (removeDirectory)

class (HasFilePath a, Exception e) => Removable a e where
  remove :: a -> IO (Either e a)

instance Exception e => Removable Project e where
  remove p = applyDirWithResult p removeDirectory (getFilePath p)

instance Exception e => Removable Scene e where
  remove s = applyDirWithResult s removeDirectory (getFilePath s)

instance Exception e => Removable Act e where
  remove a = applyDirWithResult a removeDirectory (getFilePath a)
