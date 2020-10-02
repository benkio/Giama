module UI.Shell.App (app) where

import           Domain.Project (Project (..), appendScene, removeScene)
import           Domain.Scene   (Scene (..))
-- import           Persistence.FileSystem.Project (createProject)

app :: IO ()
app = do
  let p = Project { projectName = "Izi Project", projectScenes = []}
  let s = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 1 ,sceneName = "Scene A" ,sceneActs = [] }
  let s' = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 10 ,sceneName = "Scene B" ,sceneActs = [] }
  let p' = appendScene s p
  print $ "p1 - " ++ show p'
  let p'' = appendScene s' p'
  print $ "p2 - " ++ show p''
  let p''' = removeScene (sceneName s) p''
  print p'''
  return ()
