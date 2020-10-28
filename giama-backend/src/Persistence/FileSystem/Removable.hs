{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Removable (Removable(..)) where

import           Domain.Act                                (Act (..))
import           Domain.Project                            (Project (..))
import           Domain.Scene                              (Scene (..))
import           Persistence.FileSystem.DirectoryFunctions (applyDirWithResult)
import           Persistence.FileSystem.HasFilePath        (HasFilePath (..))
import           System.Path.IO                            (removeDirectory,
                                                            removeFile)

class (HasFilePath a) => Removable a where
  remove :: a -> IO a

instance Removable Project where
  remove p = applyDirWithResult p removeDirectory (getFilePath p)

instance Removable Scene where
  remove s = applyDirWithResult s removeDirectory (getFilePath s)

instance Removable Act where
  remove a = applyDirWithResult a removeFile (getFilePath a)
