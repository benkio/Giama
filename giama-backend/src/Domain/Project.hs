module Domain.Project (Project(..), ProjectName, showProjects) where

import           Data.Time.Clock        (UTCTime)
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

