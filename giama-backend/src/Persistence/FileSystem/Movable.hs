{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Movable where

import           Data.Foldable                      (traverse_)
import           System.Path                        (toFilePath)

import           Persistence.FileSystem.Loadable    (loadProject)

import           Domain.BusinessError               (BusinessError (..))
import           Persistence.FileSystem.HasFilePath (getFilePath)
import           System.Directory                   (renameDirectory)

import           Domain.Act                         (Act (..))
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..))

class Movable a b where
  move :: a -> Int -> b -> IO (Either BusinessError b)

instance Movable Scene Project where
  move = moveScene

instance Movable Act Scene where
  move actToMove position targetScene = undefined

positionCheck :: Int -> Project -> Int
positionCheck position project =
  min position $ (maximum . fmap scenePosition . projectScenes) project

moveScene :: Scene -> Int -> Project -> IO (Either BusinessError Project)
moveScene sceneToMove position targetProject = do
    let pos = positionCheck position targetProject
        sceneToMovePath = getFilePath sceneToMove
        targetSceneToMove = sceneToMove { scenePosition = pos, sceneParentProjectName = projectName targetProject}
        targetSceneToMovePath = getFilePath targetSceneToMove
        followingProjectScenes = (filter (\s -> scenePosition s >= pos) . projectScenes) targetProject
    renameDirectory (toFilePath sceneToMovePath) (toFilePath targetSceneToMovePath)
    traverse_ (\s -> move s (scenePosition s + 1) targetProject) followingProjectScenes
    loadProject (projectName targetProject)
