module Persistence.FileSystem.Classes (HasFilePath(..)) where

import           Control.Monad                 (join)
import           Data.List                     (find)
import           Domain.Act
import           Domain.Identifiers            (ActName, ProjectName, SceneName)
import           Domain.Project
import           Domain.Scene
import           Persistence.FileSystem.Config (rootPath)
import           System.Directory              (listDirectory)
import           System.FilePath
import           System.IO

class HasFilePath a where
  getFilePath :: a -> IO (Maybe FilePath)

searchFilePathByName :: String -> FilePath ->  IO (Maybe FilePath)
searchFilePathByName name dir = do
    filePaths <- listDirectory dir
    return $ find (\fp -> name == takeFileName fp) filePaths

instance HasFilePath Project where
  getFilePath p = searchFilePathByName (projectName p) rootPath

instance HasFilePath Scene where
  getFilePath s = do
    projectFilePath <- searchFilePathByName (sceneParentProjectName s) rootPath
    join <$> traverse (searchFilePathByName (sceneName s)) projectFilePath

instance HasFilePath Act where
  getFilePath a = do
    sceneFilePath <- searchFilePathByName (actParentSceneName a) rootPath
    join <$> traverse (searchFilePathByName (actName a)) sceneFilePath
