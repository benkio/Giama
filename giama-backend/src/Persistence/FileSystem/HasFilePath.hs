module Persistence.FileSystem.HasFilePath (HasFilePath(..)) where

import           Domain.Act                    (Act (..))
import           Domain.Project                (Project (..))
import           Domain.Scene                  (Scene (..))
import           Persistence.FileSystem.Config (rootPath)
import           System.Path                   (Absolute, Path, fragment,
                                                fragments, (</>))

class HasFilePath a where
  getFilePath :: a -> Path Absolute

instance HasFilePath Project where
  getFilePath p = rootPath </> fragment (show (projectName p))

instance HasFilePath Scene where
  getFilePath s =
    rootPath </> fragments [sceneParentProjectName s, show (scenePosition s) ++ "_" ++ sceneName s]

instance HasFilePath Act where
  getFilePath a =
    rootPath </> fragments [actParentProjectName a, show (actParentScenePosition a) ++ "_" ++ actParentSceneName a,
    show (actPosition a) ++ "_" ++ actName a ++ ".txt"]
