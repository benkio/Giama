module Domain.Project (Project(..), ProjectName, showProjects) where

import           Domain.Identifiers (ProjectName)
import           Domain.Scene       (Scene (..))

data Project = Project {
    projectName   :: ProjectName
  , projectScenes :: [Scene]
  }

instance Show Project where
  show Project { projectName=pn, projectScenes=ps} =
    pn ++ foldl (\acc s -> acc ++ "\n" ++ show s) "" ps

showProjects :: [Project] -> String
showProjects = foldl (\acc p-> acc ++ "\n" ++ show p) ""
