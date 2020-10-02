module Domain.Scene (Scene(..), sceneAddPosition) where

import           Domain.Act         (Act)
import           Domain.Identifiers (ProjectName, SceneName)

data Scene = Scene {
  sceneParentProjectName :: ProjectName
  ,scenePosition         :: Int
  ,sceneName             :: SceneName
  ,sceneActs             :: [Act]
  } deriving (Show)

sceneAddPosition :: Int -> Scene -> Scene
sceneAddPosition x s = let currentPosition = scenePosition s
                           newPosition = currentPosition + x
                       in s { scenePosition = newPosition }
