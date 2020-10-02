module UI.Shell.App (app) where

import           Domain.Project (Project (..), appendScene, insertScene,
                                 moveScene, removeScene)
import           Domain.Scene   (Scene (..))
-- import           Persistence.FileSystem.Project (createProject)

app :: IO ()
app = do
  let p = Project { projectName = "Izi Project", projectScenes = []}
  let s = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 1 ,sceneName = "Scene A" ,sceneActs = [] }
  let s' = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 10 ,sceneName = "Scene B" ,sceneActs = [] }
  let s'' = Scene { sceneParentProjectName = "Izi Project" ,scenePosition = 10 ,sceneName = "Scene C" ,sceneActs = [] }
  let p' = appendScene s p
  print $ "p1 - " ++ show p'
  let p'' = appendScene s' p'
  print $ "p2 - " ++ show p''
  let p''' = insertScene s'' 1 p''
  print p'''
  let p'''' = moveScene (sceneName s'') 0 p'''
  print p''''
  return ()
