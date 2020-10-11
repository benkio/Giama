module Domain.Act (Act(..)) where

import           Domain.Identifiers (ActName, ProjectName, SceneName)

data Act = Act {
  actParentProjectName :: ProjectName
  ,actParentSceneName  :: SceneName
  ,actPosition         :: Int
  ,actName             :: ActName
  ,actContent          :: String
  }

instance Show Act where
  show Act{
         actParentProjectName = appn
         ,actParentSceneName  = apsn
         ,actPosition         = ap
         ,actName             = an
         } =
      "       |- (" ++ appn ++ " - " ++ apsn ++ ") - " ++ show ap ++ " " ++ an
