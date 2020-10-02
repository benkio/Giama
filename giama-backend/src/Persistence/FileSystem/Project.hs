module Persistence.FileSystem.Project(createProject) where

import           Control.Exception             (SomeException, try)
import           Domain.Project                (Project (..))
import           Domain.Scene                  (Scene (..))
import           Persistence.FileSystem.Config (rootPath)
import           System.Directory              (createDirectory)
import           System.FilePath

createProject :: Project -> IO (Either SomeException Project)
createProject p = (fmap (fmap (const p)) . try . createDirectory) $ rootPath </> projectName p

addSceneToProject :: Project -> Scene -> IO (Either SomeException Project)
addSceneToProject = undefined
