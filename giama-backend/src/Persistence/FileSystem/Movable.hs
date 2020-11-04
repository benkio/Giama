{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Movable where

import           Control.Monad.IO.Class             (liftIO)

import           Domain.Identifiers                 (ProjectName, SceneName)

import qualified Control.Monad.Trans.Except         as E (ExceptT (..), except,
                                                          runExceptT)
import           Data.Foldable                      (traverse_)
import           Data.List                          (find)
import           Domain.Act                         (Act (..))
import           Domain.BusinessError               (BusinessError (..))
import           Domain.HasChild                    (HasChild (..))
import           Domain.HasName                     (HasName (..), getName)
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..))
import           Persistence.FileSystem.HasFilePath (getFilePath)
import           Persistence.FileSystem.Loadable    (loadProject)
import           System.Directory                   (renameDirectory)
import           System.Path                        (toFilePath)

class Movable a b where
  move :: a -> Int -> b -> b -> IO (Either BusinessError b)

instance Movable Scene ProjectName where
  move = moveScene

instance Movable Act Scene where
  move = moveAct

positionCheck :: (HasChild a b) => Int -> a -> (b -> Int) -> Int
positionCheck position parent childExtractPosition =
  min position $ ((+1) . maximum . fmap childExtractPosition . getChilds) parent

moveScene :: Scene -> Int -> ProjectName -> ProjectName -> IO (Either BusinessError ProjectName)
moveScene sceneToMove position targetProjectName sourceProjectName = E.runExceptT $ do
  sourceProject <- E.ExceptT (loadProject sourceProjectName)
  let followingSourceProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {scenePosition = scenePosition s - 1}))) .
                                      filter (\s -> scenePosition s > scenePosition sceneToMove) .
                                      projectScenes) sourceProject

  liftIO $ uncurry renameDirectory `traverse_` followingSourceProjectScenes

  targetProject <- E.ExceptT (loadProject targetProjectName)
  let pos = positionCheck position targetProject scenePosition
      sceneToMovePath = getFilePath sceneToMove
      targetSceneToMovePath = getFilePath $ sceneToMove { scenePosition = pos, sceneParentProjectName = targetProjectName }
      followingTargetProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {scenePosition = scenePosition s + 1}))) .
                                      filter (\s -> scenePosition s >= pos && sceneName s /= sceneName sceneToMove) .
                                      projectScenes) targetProject

  liftIO $ renameDirectory (toFilePath sceneToMovePath) (toFilePath targetSceneToMovePath)
  liftIO $ uncurry renameDirectory `traverse_` followingTargetProjectScenes
  return targetProjectName

moveAct :: Act -> Int -> Scene -> Scene -> IO (Either BusinessError Scene)
moveAct actToMove position targetScene sourceScene = undefined -- do
    -- let pos = positionCheck position targetScene actPosition
    --     actToMovePath = getFilePath actToMove
    --     targetActToMove = actToMove { actPosition = pos, actParentSceneName = sceneName targetScene}
    --     targetActToMovePath = getFilePath targetActToMove
    --     followingSceneActs = (filter (\a -> actPosition a >= pos && getName a /= getName actToMove) . sceneActs) targetScene
    -- renameDirectory (toFilePath actToMovePath) (toFilePath targetActToMovePath)
    -- traverse_ (\s -> move s (actPosition s + 1) targetScene) followingSceneActs
    -- loadScene (sceneParentProjectName targetScene) (getName targetScene)
