module Persistence.FileSystem.Project(createProject) where

import           Control.Exception             (SomeException, try)
import           Domain.Project                (Project (..))
import           Persistence.FileSystem.Config (rootPath)
import           System.Directory              (createDirectory)
import           System.FilePath

createProject :: Project -> IO (Either SomeException ())
createProject p = (try . createDirectory) $ rootPath </> projectName p
