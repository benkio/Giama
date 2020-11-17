module Domain.Act (Act(..), createEmptyAct) where

import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ActName, ProjectName, SceneName)

data Act = Act {
  actName                :: ActName
  , actParentScenePosition :: Int
  , actModifiedDate        :: UTCTime
  , actPosition            :: Int
  , actContent             :: String
  }

instance Show Act where
  show Act{
    actName             = an
    ,actPosition        = ap
    } =
    show an ++ show ap

instance HasName Act where
  getName = getName . actName

instance HasModifiedDate Act where
  getModifiedDate = actModifiedDate

createEmptyAct :: Int -> ActName -> Int -> IO Act
createEmptyAct scenePosition name position = getCurrentTime >>= \time ->
  return Act {
  actName                 = name
  , actParentScenePosition  = scenePosition
  , actModifiedDate         = time
  , actPosition             = position
  , actContent              = "Empty Act"
  }
