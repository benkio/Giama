module Domain.Scene (Scene(..), createEmptyScene) where

import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.Act             (Act)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ProjectName, SceneName)

data Scene = Scene {
  sceneParentProjectName :: ProjectName
  , sceneModifiedDate    :: UTCTime
  , scenePosition        :: Int
  , sceneName            :: SceneName
  , sceneActs            :: [Act]
  }

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

createEmptyScene :: ProjectName -> SceneName -> Int -> IO Scene
createEmptyScene projectName name position = getCurrentTime >>= \time ->
  return Scene {
  sceneParentProjectName = projectName
  , sceneModifiedDate    = time
  , scenePosition        = position
  , sceneName            = name
  , sceneActs            = []
  }
