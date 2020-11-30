{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.Scene (Scene(..), createEmptyScene, extractAct, scenePosition, sceneName, sceneProjectId) where

import           Data.List              (find)
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.Act             (Act (..))
import           Domain.BusinessError   (BusinessError (..))
import           Domain.HasChild        (HasChild (..))
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ActId, SceneId, projectIdFromSceneId,
                                         sceneIdPosition)
import           LanguageExtensions     (maybeToEither)

data Scene = Scene {
  sceneId             :: SceneId
  , sceneModifiedDate :: UTCTime
  , sceneActs         :: [Act]
  }

instance Show Scene where
  show Scene{
    sceneId                = sId
    ,sceneActs             = as
    } =
    show sId ++ foldl (\acc a -> acc ++ "\n" ++ show a) "" as

instance HasName Scene where
  getName = getName . sceneId

instance HasChild Scene Act where
  getChilds = sceneActs

instance HasModifiedDate Scene where
  getModifiedDate = sceneModifiedDate

extractAct :: ActId -> Scene -> Either BusinessError Act
extractAct sn = maybeToEither ActNotFound . find (\s -> show (actId s) == show sn) . sceneActs

createEmptyScene :: SceneId -> IO Scene
createEmptyScene scnId = getCurrentTime >>= \time ->
  return Scene {
  sceneId            = scnId
  , sceneModifiedDate    = time
  , sceneActs            = []
  }

sceneProjectId :: Scene -> String
sceneProjectId = getName . projectIdFromSceneId . sceneId
scenePosition :: Scene -> Int
scenePosition = sceneIdPosition . sceneId
sceneName :: Scene -> String
sceneName = getName . sceneId
