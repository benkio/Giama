{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.Scene (Scene(..), createEmptyScene, extractAct) where

import           Data.List              (find)
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.Act             (Act (..))
import           Domain.BusinessError   (BusinessError (..))
import           Domain.HasChild        (HasChild (..))
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ActName, ProjectName, SceneName)
import           LanguageExtensions     (maybeToEither)

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

instance HasChild Scene Act where
  getChilds = sceneActs

instance HasModifiedDate Scene where
  getModifiedDate = sceneModifiedDate

extractAct :: ActName -> Scene -> Either BusinessError Act
extractAct sn = maybeToEither ActNotFound . find (\s -> actName s == sn) . sceneActs

createEmptyScene :: ProjectName -> SceneName -> Int -> IO Scene
createEmptyScene projectName name position = getCurrentTime >>= \time ->
  return Scene {
  sceneParentProjectName = projectName
  , sceneModifiedDate    = time
  , scenePosition        = position
  , sceneName            = name
  , sceneActs            = []
  }
