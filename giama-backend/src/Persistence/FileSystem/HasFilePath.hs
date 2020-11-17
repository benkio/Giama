module Persistence.FileSystem.HasFilePath (HasFilePath(..)) where

import           Domain.Identifiers                 (SceneId(..), ActId(..))

import           Domain.Act                    (Act (..))
import           Domain.Project                (Project (..))
import           Domain.Scene                  (Scene (..))
import           Persistence.FileSystem.Config (rootPath)
import           System.Path                   (Absolute, Path, fragment,
                                                fragments, (</>))

class HasFilePath a where
  getFilePath :: a -> Path Absolute

instance HasFilePath Project where
  getFilePath p = rootPath </> fragment (show (projectId p))

instance HasFilePath Scene where
  getFilePath s =
    rootPath </> fragments [projectId s,
                            scenePosition s ++ "_" ++ sceneName s]
    where projectId :: Scene -> String
          projectId Scene {sceneId=(MkSceneId prjn _)} = prjn
          sceneName :: Scene -> String
          sceneName Scene {sceneId=(MkSceneId _ (scnn, _))} = scnn
          scenePosition :: Scene -> String
          scenePosition Scene {sceneId=(MkSceneId _ (_, pos))} = show pos

instance HasFilePath Act where
  getFilePath a =
    rootPath </> fragments [
    projectName a,
    scenePosition a ++ "_" ++ sceneName a,
    actPosition a ++ "_" ++ actName a ++ ".txt"]
    where projectName :: Act -> String
          projectName Act {actId=(MkActId prjn _ _)} = prjn
          sceneName :: Act -> String
          sceneName Act {actId=(MkActId _ (scnn, _) _)} = scnn
          actName :: Act -> String
          actName Act {actId=(MkActId _ _ (actn, _))} = actn
          scenePosition :: Act -> String
          scenePosition Act {actId=(MkActId _ (_, pos) _)} = show pos
          actPosition :: Act -> String
          actPosition Act {actId=(MkActId _ _ (_, pos))} = show pos
