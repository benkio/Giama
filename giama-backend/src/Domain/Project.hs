module Domain.Project (Project(..), ProjectName, showProjects, createEmptyProject) where

import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ProjectName)
import           Domain.Scene           (Scene (..))

data Project = Project {
    projectName           :: ProjectName
    , projectModifiedDate :: UTCTime
    , projectScenes       :: [Scene]
  }

instance Show Project where
  show Project { projectName=pn, projectScenes=ps} =
    pn ++ foldl (\acc s -> acc ++ "\n" ++ show s) "" ps

showProjects :: [Project] -> String
showProjects = foldl (\acc p-> acc ++ "\n" ++ show p) ""

instance HasName Project where
  getName = projectName

instance HasModifiedDate Project where
  getModifiedDate = projectModifiedDate

createEmptyProject :: ProjectName -> IO Project
createEmptyProject name = getCurrentTime >>= \time ->
  return Project {
  projectName         = name
  , projectModifiedDate = time
  , projectScenes       = []
  }
