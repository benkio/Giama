{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Movable where

import           Data.Foldable                      (traverse_)
import           Data.List                          (find)
import           Domain.Act                         (Act (..))
import           Domain.BusinessError               (BusinessError (..))
import           Domain.HasChild                    (HasChild (..))
import           Domain.HasName                     (HasName (..), getName)
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..))
import           Persistence.FileSystem.HasFilePath (getFilePath)
import           Persistence.FileSystem.Loadable    (loadProject, loadScene)
import           System.Directory                   (renameDirectory)
import           System.Path                        (toFilePath)

class Movable a b where
  move :: a -> Int -> b -> IO (Either BusinessError b)

instance Movable Scene Project where
  move = moveScene

instance Movable Act Scene where
  move = moveAct

positionCheck :: (HasChild a b) => Int -> a -> (b -> Int) -> Int
positionCheck position parent childExtractPosition =
  min position $ ((+1) . maximum . fmap childExtractPosition . getChilds) parent

moveScene :: Scene -> Int -> Project -> IO (Either BusinessError Project)
moveScene sceneToMove position targetProject = do
    let pos = positionCheck position targetProject scenePosition
        sceneToMovePath = getFilePath sceneToMove
        targetSceneToMove = sceneToMove { scenePosition = pos, sceneParentProjectName = projectName targetProject}
        targetSceneToMovePath = getFilePath targetSceneToMove
        followingProjectScene = (find (\s -> scenePosition s >= pos && getName s /= getName sceneToMove) . projectScenes) targetProject
        filteredScenes = (filter ((/=) (getName sceneToMove) . getName) . projectScenes) targetProject
    renameDirectory (toFilePath sceneToMovePath) (toFilePath targetSceneToMovePath)
    print followingProjectScene
    (\s -> move s (pos + 1) (targetProject { projectScenes = filteredScenes })) `traverse_` followingProjectScene
    loadProject (getName targetProject)

moveAct :: Act -> Int -> Scene -> IO (Either BusinessError Scene)
moveAct actToMove position targetScene = undefined -- do
    -- let pos = positionCheck position targetScene actPosition
    --     actToMovePath = getFilePath actToMove
    --     targetActToMove = actToMove { actPosition = pos, actParentSceneName = sceneName targetScene}
    --     targetActToMovePath = getFilePath targetActToMove
    --     followingSceneActs = (filter (\a -> actPosition a >= pos && getName a /= getName actToMove) . sceneActs) targetScene
    -- renameDirectory (toFilePath actToMovePath) (toFilePath targetActToMovePath)
    -- traverse_ (\s -> move s (actPosition s + 1) targetScene) followingSceneActs
    -- loadScene (sceneParentProjectName targetScene) (getName targetScene)
