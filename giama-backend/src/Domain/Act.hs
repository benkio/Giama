module Domain.Act (Act(..)) where

import           Domain.Identifiers (ActName, ProjectName, SceneName)

data Act = Act {
  actParentProjectName :: ProjectName
  ,actParentSceneName  :: SceneName
  ,actPosition         :: Int
  ,actName             :: ActName
  ,actContent          :: String
  } deriving (Show)
