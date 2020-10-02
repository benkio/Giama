module Domain.Project (Project(..), ProjectName, appendScene, removeScene, moveScene, insertScene) where

import           Data.Either.Combinators (maybeToRight)
import           Data.List               (find)
import           Domain.Errors           (DomainError (..))
import           Domain.Identifiers      (ProjectName, SceneName)
import           Domain.Scene            (Scene (..), sceneAddPosition)

data Project = Project {
    projectName   :: ProjectName
  , projectScenes :: [Scene]
  } deriving (Show)

appendScene :: Scene -> Project -> Project
appendScene s p = let currentScenes = projectScenes p
                      newScenes = currentScenes ++ [s { scenePosition = length currentScenes}]
                  in p { projectScenes = newScenes}

removeScene :: SceneName -> Project -> Either DomainError Project
removeScene s p = do
  let currentScenes = projectScenes p
  sceneToRemove <- maybeToRight (SceneNotFound s) $ find (\x -> sceneName x == s) currentScenes
  let (previousScenes, nextScenes) = splitAt (scenePosition sceneToRemove) currentScenes
  let newScenes = previousScenes ++ fmap (sceneAddPosition (-1)) (tail nextScenes)
  return $ p { projectScenes = newScenes }

insertScene :: Scene -> Int -> Project -> Project
insertScene s i p =
  let currentScenes = projectScenes p
      (previousScenes, nextScenes) = splitAt i currentScenes
      newScenes = previousScenes ++ [s { scenePosition = i }] ++ fmap (sceneAddPosition 1) nextScenes
  in p { projectScenes = newScenes }


moveScene :: SceneName -> Int -> Project -> Either DomainError Project
moveScene s i p = do
  sceneToMove <- maybeToRight (SceneNotFound s) $ find (\x -> sceneName x == s) (projectScenes p)
  newProject <- removeScene (sceneName sceneToMove) p
  return $ insertScene sceneToMove i newProject
