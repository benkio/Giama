{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Movable where

import           Control.Monad.IO.Class             (liftIO)

import           Domain.Identifiers                 (ProjectId, SceneId)

import qualified Control.Monad.Trans.Except         as E (ExceptT (..), except,
                                                          runExceptT)
import           Data.Foldable                      (traverse_)
import           Data.List                          (find)
import           Domain.Act                         (Act (..))
import           Domain.BusinessError               (BusinessError (..))
import           Domain.HasChild                    (HasChild (..))
import           Domain.HasName                     (HasName (..), getName)
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..), scenePosition)
import           Persistence.FileSystem.HasFilePath (getFilePath)
import           Persistence.FileSystem.Loadable    (loadProject)
import           System.Directory                   (renameDirectory)
import           System.Path                        (toFilePath)

class Movable a b where
  move :: a -> Int -> b -> b -> IO (Either BusinessError b)

instance Movable Scene ProjectId where
  move = moveScene

instance Movable Act SceneId where
  move = moveAct

positionCheck :: (HasChild a b) => Int -> a -> (b -> Int) -> Int
positionCheck position parent childExtractPosition =
  min position $ ((+1) . maximum . fmap childExtractPosition . getChilds) parent

moveScene :: Scene -> Int -> ProjectId -> ProjectId -> IO (Either BusinessError ProjectId)
moveScene sceneToMove position targetProjectId sourceProjectId = E.runExceptT $ do
  sourceProject <- E.ExceptT (loadProject sourceProjectId)
  let followingSourceProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {scenePosition = scenePosition s - 1}))) .
                                      filter (\s -> scenePosition s > scenePosition sceneToMove) .
                                      projectScenes) sourceProject

  liftIO $ uncurry renameDirectory `traverse_` followingSourceProjectScenes

  targetProject <- E.ExceptT (loadProject targetProjectId)
  let pos = positionCheck position targetProject scenePosition
      sceneToMovePath = getFilePath sceneToMove
      targetSceneToMovePath = getFilePath $ sceneToMove { scenePosition = pos, sceneParentProjectId = targetProjectId }
      followingTargetProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {scenePosition = scenePosition s + 1}))) .
                                      filter (\s -> scenePosition s >= pos && sceneName s /= sceneName sceneToMove) .
                                      projectScenes) targetProject

  liftIO $ renameDirectory (toFilePath sceneToMovePath) (toFilePath targetSceneToMovePath)
  liftIO $ uncurry renameDirectory `traverse_` followingTargetProjectScenes
  return targetProjectId

moveAct :: Act -> Int -> SceneId -> SceneId -> IO (Either BusinessError SceneId)
moveAct actToMove position targetSceneId sourceSceneId = undefined -- E.runExceptT $ do
  -- sourceProject <- E.ExceptT (loadProject sourceProjectId)
  -- let followingSourceProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {scenePosition = scenePosition s - 1}))) .
  --                                     filter (\s -> scenePosition s > scenePosition sceneToMove) .
  --                                     projectScenes) sourceProject

  -- liftIO $ uncurry renameDirectory `traverse_` followingSourceProjectScenes

  -- targetProject <- E.ExceptT (loadProject targetProjectId)
  -- let pos = positionCheck position targetProject scenePosition
  --     sceneToMovePath = getFilePath sceneToMove
  --     targetSceneToMovePath = getFilePath $ sceneToMove { scenePosition = pos, sceneParentProjectId = targetProjectId }
  --     followingTargetProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {scenePosition = scenePosition s + 1}))) .
  --                                     filter (\s -> scenePosition s >= pos && sceneName s /= sceneName sceneToMove) .
  --                                     projectScenes) targetProject

  -- liftIO $ renameDirectory (toFilePath sceneToMovePath) (toFilePath targetSceneToMovePath)
  -- liftIO $ uncurry renameDirectory `traverse_` followingTargetProjectScenes
  -- return targetProjectId
