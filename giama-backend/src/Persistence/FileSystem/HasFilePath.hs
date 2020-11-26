module Persistence.FileSystem.HasFilePath (HasFilePath(..)) where

import           Domain.Act                    (Act (..), actProjectName, actSceneName, actScenePosition, actPosition, actName)
import           Domain.Project                (Project (..))
import           Domain.Scene                  (Scene (..), scenePosition, sceneName, sceneProjectId)
import           Persistence.FileSystem.Config (rootPath)
import           System.Path                   (Absolute, Path, fragment,
                                                fragments, (</>))

class HasFilePath a where
  getFilePath :: a -> Path Absolute

instance HasFilePath Project where
  getFilePath p = rootPath </> fragment (show (projectId p))

instance HasFilePath Scene where
  getFilePath s =
    rootPath </> fragments [sceneProjectId s,
                            (show . scenePosition) s ++ "_" ++ sceneName s]

instance HasFilePath Act where
  getFilePath a =
    rootPath </> fragments [
    actProjectName a,
    actScenePosition a ++ "_" ++ actSceneName a,
    (show . actPosition) a ++ "_" ++ actName a ++ ".txt"]
