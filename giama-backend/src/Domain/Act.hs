module Domain.Act (Act(..), createEmptyAct, actPosition, actProjectName , actSceneName , actName , actScenePosition) where

import Domain.Identifiers
      ( projectIdFromActId,
        sceneIdFromActId,
        sceneIdPosition,
        sceneIdFromActId,
        ActId,
        actIdPosition )
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))

data Act = Act {
  actId                :: ActId
  , actModifiedDate        :: UTCTime
  , actContent             :: String
  }

instance Show Act where
  show Act{actId = aId} = show aId

instance HasName Act where
  getName = getName . actId

instance HasModifiedDate Act where
  getModifiedDate = actModifiedDate

createEmptyAct :: ActId -> IO Act
createEmptyAct aId = getCurrentTime >>= \time ->
                                          return Act {
  actId                 = aId
  , actModifiedDate         = time
  , actContent              = "Empty Act"
  }

actPosition      :: Act -> Int
actPosition       = actIdPosition .  actId
actProjectName   :: Act -> String
actProjectName    = getName . projectIdFromActId . actId
actSceneName     :: Act -> String
actSceneName      = getName . sceneIdFromActId . actId
actName          :: Act -> String
actName           = getName . actId
actScenePosition :: Act -> String
actScenePosition  = show . sceneIdPosition . sceneIdFromActId . actId
