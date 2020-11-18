{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Movable where

import Domain.Identifiers
      ( ActId,
        ProjectId,
        SceneId,
        sceneIdPosition,
        projectIdFromSceneId,
        ProjectId,
        SceneId,
        sceneIdModifyPosition,
        sceneIdModifyProject )
import           Control.Monad.IO.Class             (liftIO)
import qualified Control.Monad.Trans.Except         as E (ExceptT (..), except,
                                                          runExceptT)
import           Data.Foldable                      (traverse_)
import           Data.List                          (find)
import           Domain.Act                         (Act (..))
import           Domain.BusinessError               (BusinessError (..))
import           Domain.HasChild                    (HasChild (..))
import           Domain.HasName                     (HasName (..), getName)
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..), scenePosition, sceneName)
import           Persistence.FileSystem.HasFilePath (getFilePath)
import           Persistence.FileSystem.Loadable    (loadProject)
import           System.Directory                   (renameDirectory)
import           System.Path                        (toFilePath)

class Movable a b where
  move :: a -> Int -> b -> IO (Either BusinessError b)

instance Movable Scene ProjectId where
  move = moveScene

instance Movable Act SceneId where
  move = moveAct

positionCheck :: (HasChild a b) => Int -> a -> (b -> Int) -> Int
positionCheck position parent childExtractPosition =
  min position $ ((+1) . maximum . fmap childExtractPosition . getChilds) parent

moveScene :: Scene -> Int -> ProjectId -> IO (Either BusinessError ProjectId)
moveScene sceneToMove position targetProjectId = E.runExceptT $ do
  sourceProject <- E.ExceptT ((loadProject . projectIdFromSceneId . sceneId) sceneToMove)
  let followingSourceProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {sceneId = sceneIdModifyPosition (\x -> x - 1) (sceneId s)}))) .
                                      filter (\s -> scenePosition s > scenePosition sceneToMove) .
                                      projectScenes) sourceProject

  liftIO $ uncurry renameDirectory `traverse_` followingSourceProjectScenes

  targetProject <- E.ExceptT (loadProject targetProjectId)
  let pos = positionCheck position targetProject scenePosition
      sceneToMovePath = getFilePath sceneToMove
      targetSceneToMovePath = getFilePath $ sceneToMove {sceneId=(sceneIdModifyProject targetProjectId . sceneIdModifyPosition (const pos)) (sceneId sceneToMove)}
      followingTargetProjectScenes = (fmap (\s -> ((toFilePath . getFilePath) s, (toFilePath . getFilePath) (s {sceneId = sceneIdModifyPosition (+1) (sceneId s)}))) .
                                      filter (\s -> scenePosition s >= pos && sceneName s /= sceneName sceneToMove) .
                                      projectScenes) targetProject

  liftIO $ renameDirectory (toFilePath sceneToMovePath) (toFilePath targetSceneToMovePath)
  liftIO $ uncurry renameDirectory `traverse_` followingTargetProjectScenes
  return targetProjectId

moveAct :: Act -> Int -> SceneId -> IO (Either BusinessError SceneId)
moveAct actToMove position targetSceneId = undefined -- E.runExceptT $ do
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
