module Persistence.FileSystem.Project(createProject, loadProjects) where

import           Persistence.FileSystem.Config             (rootPath)

import           Control.Exception                         (Exception)
import           Control.Monad.Trans.Except                (ExceptT (..),
                                                            runExceptT)
import           Domain.Project                            (Project (..))
import           Persistence.FileSystem.Classes            (HasFilePath (..))
import           Persistence.FileSystem.DirectoryFunctions (applyDirWithResult)
import           Persistence.FileSystem.Scene              (createScene,
                                                            loadScenes)
import           System.Directory                          (createDirectory,
                                                            listDirectory,
                                                            removeDirectory)

removeProject :: Exception e => Project -> IO (Either e Project)
removeProject p = applyDirWithResult p removeDirectory (getFilePath p)

createProject :: Exception e => Project -> IO (Either e Project)
createProject p = runExceptT $
  do
    let projectPath = getFilePath p
    project <- ExceptT $ applyDirWithResult p createDirectory projectPath
    scenes <- ExceptT $ sequence <$> traverse createScene (projectScenes p)
    return $ project { projectScenes = scenes }

-- load projects from dir

loadProjects :: IO [Project]
loadProjects = do
  projectFilePaths <- listDirectory rootPath
  traverse (\p -> loadScenes (Project { projectName = p, projectScenes = [] })) projectFilePaths

-- appendSceneToProject :: Project -> Scene -> IO (Either SomeException Project)
-- appendSceneToProject p s = undefined
