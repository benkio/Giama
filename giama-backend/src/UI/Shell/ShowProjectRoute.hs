module UI.Shell.ShowProjectRoute (showProjectRoute, showProjectByModifiedDateRoute) where

import           Data.List                       (intersperse)
import           Persistence.FileSystem.Loadable (loadProjects)

showProjectRoute :: IO ()
showProjectRoute = do
  projects <- loadProjects
  mapM_ (\p -> print p >> putStrLn "") projects

showProjectByModifiedDateRoute :: IO ()
showProjectByModifiedDateRoute = undefined
