module UI.Shell.App (app) where

import           Domain.Act                     (Act (..))
import           Domain.Project                 (Project (..))
import           Domain.Scene                   (Scene (..))
import           Persistence.FileSystem.Loadable (loadProjects)

app :: IO ()
app = do
  --createProject p
  x <- loadProjects -- p
  print x
  return ()


s = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 0,sceneName = "Scene A" ,sceneActs = [a] }
s' = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 1  ,sceneName = "Scene B" ,sceneActs = [] }
s'' = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 2  ,sceneName = "Scene C" ,sceneActs = [a'] }
a = Act {
  actParentProjectName = "Izi Project"
  ,actParentSceneName  = "0_Scene A"
  ,actPosition         = 0
  ,actName             = "First Act - Scene A"
  ,actContent          = "FAGIO GAY"
        }
a' = Act {
  actParentProjectName = "Izi Project"
  ,actParentSceneName  = "2_Scene C"
  ,actPosition         = 0
  ,actName             = "First Act - Scene C"
  ,actContent          = "FAGIO GAY FOREVER AFTER! <3"
        }

p = Project { projectName = "Izi Project", projectScenes = [s, s', s'']}
