module Domain.Act (Act(..)) where

import           Data.Time.Clock        (UTCTime)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ActName, ProjectName, SceneName)

data Act = Act {
  actParentProjectName :: ProjectName
  , actParentSceneName :: SceneName
  , actModifiedDate    :: UTCTime
  , actPosition        :: Int
  , actName            :: ActName
  , actContent         :: String
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
