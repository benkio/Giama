module Domain.Project (Project(..), ProjectName, appendScene, removeScene) where

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

moveScene :: SceneName -> Int -> Project -> Either DomainError Project
moveScene s i p = undefined
