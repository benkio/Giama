module Domain.Scene (Scene(..), sceneAddPosition) where

import           Data.Time.Clock        (UTCTime)
import           Domain.Act             (Act)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ProjectName, SceneName)

data Scene = Scene {
  sceneParentProjectName :: ProjectName
  , sceneModifiedDate    :: UTCTime
  ,scenePosition         :: Int
  ,sceneName             :: SceneName
  ,sceneActs             :: [Act]
  }

sceneAddPosition :: Int -> Scene -> Scene
sceneAddPosition x s = let currentPosition = scenePosition s
                           newPosition = currentPosition + x
                       in s { scenePosition = newPosition }

instance Show Scene where
  show Scene{
           sceneParentProjectName = sppn
           ,scenePosition         = sp
           ,sceneName             = sn
           ,sceneActs             = as} =
    "  |- (" ++ sppn ++ ") - " ++ show sp ++ " " ++ sn ++ foldl (\acc a -> acc ++ "\n" ++ show a) "" as

instance HasName Scene where
  getName = sceneName

instance HasModifiedDate Scene where
  getModifiedDate = sceneModifiedDate
