module Persistence.FileSystem.Project(createProject) where

import           Control.Exception             (SomeException, try)
import           Domain.Project                (Project (..))
import           Domain.Scene                  (Scene (..))
import           Persistence.FileSystem.Config (rootPath)
import           System.Directory              (createDirectory)
import           System.FilePath

createDirWithResult :: a -> FilePath -> IO (Either SomeException a)
createDirWithResult x = fmap (fmap (const x)) . try . createDirectory

createProject :: Project -> IO (Either SomeException Project)
createProject p =  createDirWithResult p $ rootPath </> projectName p

addSceneToProject :: Project -> Scene -> IO (Either SomeException Project)
addSceneToProject p s = undefined
