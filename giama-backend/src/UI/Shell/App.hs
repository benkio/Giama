module UI.Shell.App (app) where

import           Domain.Act                      (Act (..))
import           Domain.Project                  (Project (..), showProjects)
import           Domain.Scene                    (Scene (..))
import           Persistence.FileSystem.Loadable (loadProjects)

app :: IO ()
app = do
  x <- loadProjects
  putStrLn $ showProjects x
  return ()
