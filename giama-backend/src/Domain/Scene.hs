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
  sceneName            :: SceneName
  , sceneModifiedDate    :: UTCTime
  , scenePosition        :: Int
  , sceneActs            :: [Act]
  }

instance Show Scene where
  show Scene{
    sceneName             = sn
    ,scenePosition         = sp
    ,sceneActs             = as
    } =
    show sn ++ show sp ++ foldl (\acc a -> acc ++ "\n" ++ show a) "" as

instance HasName Scene where
  getName = getName . sceneName

instance HasChild Scene Act where
  getChilds = sceneActs

instance HasModifiedDate Scene where
  getModifiedDate = sceneModifiedDate

extractAct :: ActName -> Scene -> Either BusinessError Act
extractAct sn = maybeToEither ActNotFound . find (\s -> show (actName s) == show sn) . sceneActs

createEmptyScene :: SceneName -> Int -> IO Scene
createEmptyScene name position = getCurrentTime >>= \time ->
  return Scene {
  sceneName            = name
  , sceneModifiedDate    = time
  , scenePosition        = position
  , sceneActs            = []
  }
