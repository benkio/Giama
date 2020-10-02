module UI.Shell.App (app) where

import           Domain.Project                 (Project (..))
import           Persistence.FileSystem.Project (createProject)

app :: IO ()
app = do
  x <- createProject $ Project { projectName = "Izi Project", projectScenes = []}
  print x
  return ()
