module Domain.Scene (Scene(..)) where

import           Domain.Act         (Act)
import           Domain.Identifiers (ProjectName, SceneName)

data Scene = Scene {
  sceneParentProjectName :: ProjectName
  ,sceneName             :: SceneName
  ,sceneActs             :: [Act]
  }
