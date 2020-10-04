module Persistence.FileSystem.Classes (HasFilePath(..)) where

import           Domain.Act                    (Act (..))
import           Domain.Project                (Project (..))
import           Domain.Scene                  (Scene (..))
import           Persistence.FileSystem.Config (rootPath)
import           System.FilePath               ((<.>), (</>))

class HasFilePath a where
  getFilePath :: a -> FilePath

instance HasFilePath Project where
  getFilePath p = rootPath </> projectName p

instance HasFilePath Scene where
  getFilePath s =
    rootPath </> sceneParentProjectName s </>
    (show (scenePosition s) ++ "_" ++ sceneName s)

instance HasFilePath Act where
  getFilePath a =
    rootPath </> actParentProjectName a </> actParentSceneName a </>
    (show (actPosition a) ++ "_" ++ actName a <.> "txt")
