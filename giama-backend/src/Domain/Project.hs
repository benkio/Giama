{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.Project (Project(..), ProjectId, showElements, showElementsName, createEmptyProject, flatten, extractScene) where

import           Domain.Scene                  (Scene (..), scenePosition, sceneName, sceneProjectId)

import           Data.List              (find)
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.BusinessError   (BusinessError (..))
import           Domain.Element         (Element, elementPack)
import           Domain.HasChild        (HasChild (..))
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ProjectId, SceneId)
import           Domain.Scene           (Scene (..))
import           LanguageExtensions     (maybeToEither)

data Project = Project {
    projectId           :: ProjectId
    , projectModifiedDate :: UTCTime
    , projectScenes       :: [Scene]
  }

instance Show Project where
  show Project { projectId=pn, projectScenes=ps} =
    show pn ++ foldl (\acc s -> acc ++ "\n" ++ show s) "" ps

extractScene :: String -> Project -> Either BusinessError Scene
extractScene sn = maybeToEither SceneNotFound . find (\s -> sceneName s == sn) . projectScenes

showElementsPattern :: (a -> String) -> [a]-> String
showElementsPattern showF = foldl (\acc p-> acc ++ "\n" ++ showF p) ""

showElements :: Show a => [a] -> String
showElements = showElementsPattern show

showElementsName :: (Show a, HasName a) => [a] -> String
showElementsName = showElementsPattern (show . getName)

flatten :: [Project] -> [Element]
flatten ps = projectElements ps ++ projectSceneElements ps ++ projectActsElements ps
  where
    projectElements :: [Project] -> [Element]
    projectElements = fmap elementPack
    projectSceneElements :: [Project] -> [Element]
    projectSceneElements = fmap elementPack . concatMap projectScenes
    projectActsElements :: [Project] -> [Element]
    projectActsElements = fmap elementPack . concatMap sceneActs . concatMap projectScenes


instance HasName Project where
  getName = getName . projectId

instance HasChild Project Scene where
  getChilds = projectScenes

instance HasModifiedDate Project where
  getModifiedDate = projectModifiedDate

createEmptyProject :: ProjectId -> IO Project
createEmptyProject name = getCurrentTime >>= \time ->
  return Project {
  projectId         = name
  , projectModifiedDate = time
  , projectScenes       = []
  }
