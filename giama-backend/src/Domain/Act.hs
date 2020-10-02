module Domain.Act (Act(..)) where

import           Domain.Identifiers (ActName, ProjectName, SceneName)

data Act = Act {
  actParentProjectName :: ProjectName
  ,actParentSceneName  :: SceneName
  ,actName             :: ActName
  ,actContent          :: String
  }
