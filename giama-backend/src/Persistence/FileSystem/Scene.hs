module Persistence.FileSystem.Scene(createScene, loadScenes) where

import           Control.Exception                         (Exception)
import           Control.Monad.Trans.Except                (ExceptT (..),
                                                            runExceptT)
import           Data.List                                 (sortOn)
import           Domain.Project                            (Project (..))
import           Domain.Scene                              (Scene (..))
import           Persistence.FileSystem.Act                (createAct, loadActs)
import           Persistence.FileSystem.Classes            (HasFilePath (..))
import           Persistence.FileSystem.DirectoryFunctions (applyDirWithResult)
import           System.Directory                          (createDirectory,
                                                            listDirectory)
createScene :: Exception e => Scene -> IO (Either e Scene)
createScene s = runExceptT $
  do
    let scenePath = getFilePath s
    scene <- ExceptT $ applyDirWithResult s createDirectory scenePath
    acts <- ExceptT $ sequence <$> traverse createAct (sceneActs s)
    return $ scene { sceneActs = acts }

loadScenes :: Project -> IO Project
loadScenes p = do
  let projectPath = getFilePath p
  sceneFilePaths <- listDirectory projectPath
  scenes <- traverse (\s ->
                       let (sp, sn) = span (/= '_') s
                       in
                       loadActs (Scene {
                        sceneParentProjectName = projectName p
                        ,scenePosition         = read sp :: Int
                        ,sceneName             = tail sn
                        ,sceneActs             = []         })) sceneFilePaths
  return $ p { projectScenes = sortOn scenePosition scenes}
