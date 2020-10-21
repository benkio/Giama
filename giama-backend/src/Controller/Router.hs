module Controller.Router (
  createProjectRoute
  , createSceneRoute
  , createActRoute
  , showProjectsRoute
  , showProjectsByModifiedDateRoute
  , removeProjectRoute
  , removeSceneRoute
  , removeActRoute
  , searchByNameRoute) where

import           Control.Exception                 (SomeException)
import           Data.Bifoldable                   (bifoldMap)
import           Data.Maybe                        (fromMaybe)
import           Domain.Act                        (Act, createEmptyAct)
import           Domain.HasName                    (HasName (..))
import           Domain.Project                    (Project, createEmptyProject,
                                                    showProjects)
import           Domain.Scene                      (Scene, createEmptyScene)
import           Domain.Sort                       (sortByModifiedDate)
import           LanguageExtensions                (readMaybe)
import           Persistence.FileSystem.Createable (Createable (..))
import           Persistence.FileSystem.Loadable   (loadProjects)
import           Persistence.FileSystem.Removable  (Removable (..))

getEmptyProject :: IO Project
getEmptyProject = do
  putStrLn "> Please insert Project name: "
  projectName <- getLine
  createEmptyProject projectName

getEmptyScene :: IO Scene
getEmptyScene = do
  putStrLn "> Please insert Scene Project's name: "
  projectName <- getLine
  putStrLn "> Please insert Scene's name: "
  sceneName <- getLine
  putStrLn "> Please insert Scene position(default 0): "
  position <- (\s -> fromMaybe 0 (readMaybe s :: Maybe Int)) <$> getLine
  createEmptyScene projectName sceneName position

getEmptyAct :: IO Act
getEmptyAct = do
  putStrLn "> Please insert Act Project's name: "
  projectName <- getLine
  putStrLn "> Please insert Act Scene's name: "
  sceneName <- getLine
  putStrLn "> Please insert Scene's position(default 0): "
  scenePosition <- (\s -> fromMaybe 0 (readMaybe s :: Maybe Int)) <$> getLine
  putStrLn "> Please insert Act's name: "
  actName <- getLine
  putStrLn "> Please insert Act position(default 0): "
  actPosition <- (\s -> fromMaybe 0 (readMaybe s :: Maybe Int)) <$> getLine
  createEmptyAct projectName sceneName scenePosition actName actPosition

createProjectRoute :: IO ()
createProjectRoute = do
  newEmptyProject <- getEmptyProject
  eitherProjectCreated <- create newEmptyProject :: IO (Either SomeException Project)
  let result = bifoldMap (\e -> "An error occurred into the project creation: " ++ show e) (\p -> "Project " ++ getName p ++ " Created Successfully!!") eitherProjectCreated
  putStrLn result

createSceneRoute :: IO ()
createSceneRoute = do
  newEmptyScene <- getEmptyScene
  eitherSceneCreated <- create newEmptyScene :: IO (Either SomeException Scene)
  let result = bifoldMap (\e -> "An error occurred into the scene creation: " ++ show e) (\p -> "Scene " ++ getName p ++ " Created Successfully!!") eitherSceneCreated
  putStrLn result

createActRoute :: IO ()
createActRoute = do
  newEmptyAct <- getEmptyAct
  eitherActCreated <- create newEmptyAct :: IO (Either SomeException Act)
  let result = bifoldMap (\e -> "An error occurred into the act creation: " ++ show e) (\p -> "Act " ++ getName p ++ " Created Successfully!!") eitherActCreated
  putStrLn result

removeProjectRoute :: IO ()
removeProjectRoute = do
  newEmptyProject <- getEmptyProject
  eitherProjectRemoved <- remove newEmptyProject :: IO (Either SomeException Project)
  let result = bifoldMap (\e -> "An error occurred into the project removal: " ++ show e) (\p -> "Project " ++ getName p ++ " Removed Successfully!!") eitherProjectRemoved
  putStrLn result


removeSceneRoute :: IO ()
removeSceneRoute = do
  newEmptyScene <- getEmptyScene
  eitherSceneRemoved <- remove newEmptyScene :: IO (Either SomeException Scene)
  let result = bifoldMap (\e -> "An error occurred into the scene removal: " ++ show e) (\p -> "Scene " ++ getName p ++ " Removed Successfully!!") eitherSceneRemoved
  putStrLn result


removeActRoute :: IO ()
removeActRoute = do
  newEmptyAct <- getEmptyAct
  eitherActRemoved <- remove newEmptyAct :: IO (Either SomeException Act)
  let result = bifoldMap (\e -> "An error occurred into the act removal: " ++ show e) (\p -> "Act " ++ getName p ++ " Removed Successfully!!") eitherActRemoved
  putStrLn result


searchByNameRoute :: IO ()
searchByNameRoute = undefined

showProjectsRoute :: IO ()
showProjectsRoute = do
  projects <- loadProjects
  putStrLn $ showProjects projects

showProjectsByModifiedDateRoute :: IO ()
showProjectsByModifiedDateRoute = do
    projects <- loadProjects
    let projectSorted = sortByModifiedDate projects
    putStrLn $ showProjects projectSorted
