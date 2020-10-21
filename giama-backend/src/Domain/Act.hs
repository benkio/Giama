module Domain.Act (Act(..), createEmptyAct) where

import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ActName, ProjectName, SceneName)

data Act = Act {
  actParentProjectName     :: ProjectName
  , actParentSceneName     :: SceneName
  , actParentScenePosition :: Int
  , actModifiedDate        :: UTCTime
  , actPosition            :: Int
  , actName                :: ActName
  , actContent             :: String
  }

instance Show Act where
  show Act{
         actParentProjectName = appn
         ,actParentSceneName  = apsn
         ,actPosition         = ap
         ,actName             = an
         } =
      "       |- (" ++ appn ++ " - " ++ apsn ++ ") - " ++ show ap ++ " " ++ an

instance HasName Act where
  getName = actName

instance HasModifiedDate Act where
  getModifiedDate = actModifiedDate

createEmptyAct :: ProjectName -> SceneName -> Int -> ActName -> Int -> IO Act
createEmptyAct projectName sceneName scenePosition name position = getCurrentTime >>= \time ->
  return Act {
  actParentProjectName      = projectName
  , actParentSceneName      = sceneName
  , actParentScenePosition  = scenePosition
  , actModifiedDate         = time
  , actPosition             = position
  , actName                 = name
  , actContent              = "Empty Act"
  }
