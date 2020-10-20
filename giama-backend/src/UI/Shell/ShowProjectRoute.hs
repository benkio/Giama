module UI.Shell.ShowProjectRoute (showProjectRoute, showProjectByModifiedDateRoute) where

import           Data.List                       (intersperse)
import           Domain.HasModifiedDate          (getModifiedDate)
import           Domain.Sort                     (sortByModifiedDate)
import           Persistence.FileSystem.Loadable (loadProjects)

showProjectRoute :: IO ()
showProjectRoute = do
  projects <- loadProjects
  mapM_ (\p -> print p >> putStrLn "") projects

showProjectByModifiedDateRoute :: IO ()
showProjectByModifiedDateRoute = do
    projects <- loadProjects
    let projectSorted = sortByModifiedDate projects
    mapM_ (\p -> print p >> putStrLn "") projectSorted
