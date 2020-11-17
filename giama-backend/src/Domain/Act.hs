module Domain.Act (Act(..), createEmptyAct) where

import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ActId)

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
