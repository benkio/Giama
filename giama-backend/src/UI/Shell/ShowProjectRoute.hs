module UI.Shell.ShowProjectRoute (showProjectRoute, showProjectByModifiedDateRoute) where

import           Data.List                       (intersperse)
import           Domain.HasModifiedDate          (getModifiedDate)
import           Domain.Project                  (showProjects)
import           Domain.Sort                     (sortByModifiedDate)
import           Persistence.FileSystem.Loadable (loadProjects)

showProjectRoute :: IO ()
showProjectRoute = do
  projects <- loadProjects
  putStrLn $ showProjects projects

showProjectByModifiedDateRoute :: IO ()
showProjectByModifiedDateRoute = do
    projects <- loadProjects
    let projectSorted = sortByModifiedDate projects
    putStrLn $ showProjects projectSorted
