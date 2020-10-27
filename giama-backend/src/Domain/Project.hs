module Domain.Project (Project(..), ProjectName, showElements, showElementsName, createEmptyProject, flatten) where

import           Data.List              (concat)
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Domain.Element         (Element, elementPack)
import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))
import           Domain.Identifiers     (ProjectName)
import           Domain.Scene           (Scene (..))

data Project = Project {
    projectName           :: ProjectName
    , projectModifiedDate :: UTCTime
    , projectScenes       :: [Scene]
  }

instance Show Project where
  show Project { projectName=pn, projectScenes=ps} =
    pn ++ foldl (\acc s -> acc ++ "\n" ++ show s) "" ps

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
  getName = projectName

instance HasModifiedDate Project where
  getModifiedDate = projectModifiedDate

createEmptyProject :: ProjectName -> IO Project
createEmptyProject name = getCurrentTime >>= \time ->
  return Project {
  projectName         = name
  , projectModifiedDate = time
  , projectScenes       = []
  }
