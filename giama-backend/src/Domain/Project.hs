module Domain.Project (Project(..), ProjectName) where

import           Domain.Identifiers (ProjectName)
import           Domain.Scene       (Scene (..))

data Project = Project {
    projectName   :: ProjectName
  , projectScenes :: [Scene]
  } deriving (Show)
