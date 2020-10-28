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

import           Domain.Identifiers                (ActName, ProjectName,
                                                    SceneName)

import           Control.Exception                 (SomeException, try)
import qualified Control.Monad.Trans.Except        as E (ExceptT (..),
                                                         runExceptT)
import           Data.Bifoldable                   (bifoldMap)
import           Data.List                         (maximumBy)
import           Data.Maybe                        (fromMaybe)
import           Domain.Act                        (Act, createEmptyAct)
import           Domain.BusinessError              (BusinessError)
import           Domain.HasName                    (HasName (..))
import           Domain.Project                    (Project (..),
                                                    createEmptyProject, flatten,
                                                    showElements,
                                                    showElementsName)
import           Domain.Scene                      (Scene (..),
                                                    createEmptyScene)
import           Domain.Search                     (searchByName)
import           Domain.Sort                       (sortByModifiedDate)
import           LanguageExtensions                (readMaybe)
import           Persistence.FileSystem.Createable (Createable (..))
import           Persistence.FileSystem.Loadable   (loadProject, loadProjects,
                                                    loadScene)
import           Persistence.FileSystem.Removable  (Removable (..))

getProjectName :: IO ProjectName
getProjectName =   do
  putStrLn "> Please insert Project name: "
  getLine

getEmptyProject :: IO Project
getEmptyProject = do
  prjName <- getProjectName
  createEmptyProject prjName

getSceneName :: IO (ProjectName, SceneName)
getSceneName = do
  prjName <- getProjectName
  putStrLn "> Please insert Scene's name: "
  srnName <- getLine
  return (prjName, srnName)

getActName :: IO (ProjectName, SceneName, ActName)
getActName = do
  (prjName, srnName) <- getSceneName
  putStrLn "> Please insert Act's name: "
  actName <- getLine
  return (prjName, srnName, actName)

getNewEmptyScene :: IO (Either BusinessError Scene)
getNewEmptyScene = do
  (prjName, srnName) <- getSceneName
  eitherProject <- loadProject prjName
  let eitherNewScenePosition = (+1) . scenePosition . maximumBy (\s s' -> compare (scenePosition s) (scenePosition s')) . projectScenes <$> eitherProject
  createEmptyScene prjName srnName `traverse` eitherNewScenePosition

getElementPosition :: String -> IO Int
getElementPosition element = do
  putStrLn ("> Please insert "++ element ++"'s position(default 0): ")
  (\s -> fromMaybe 0 (readMaybe s :: Maybe Int)) <$> getLine

getEmptyAct :: IO Act
getEmptyAct = do
  (prjName, srnName, actName) <- getActName
  -- TODO Remove the request of position as for the scene
  scenePosition <- getElementPosition "Scene"
  -- TODO Remove the request of position as for the scene
  actPosition <- getElementPosition "Act"
  createEmptyAct prjName srnName scenePosition actName actPosition

createProjectRoute :: IO ()
createProjectRoute = do
  newEmptyProject <- getEmptyProject
  eitherProjectCreated <- (try (create newEmptyProject) :: IO (Either SomeException Project))
  let result = bifoldMap (\e -> "An error occurred into the project creation: " ++ show e) (\p -> "Project " ++ getName p ++ " Created Successfully!!") eitherProjectCreated
  putStrLn result

createSceneRoute :: IO ()
createSceneRoute = do
  eitherSceneCreated <- E.runExceptT (do
                                       newEmptyScene <- E.ExceptT getNewEmptyScene
                                       E.ExceptT (try (create newEmptyScene)))
  let result = bifoldMap (\e -> "An error occurred into the scene creation: " ++ show e) (\p -> "Scene " ++ getName p ++ " Created Successfully!!") eitherSceneCreated
  putStrLn result

createActRoute :: IO ()
createActRoute = do
  newEmptyAct <- getEmptyAct
  eitherActCreated <- (try (create newEmptyAct) :: IO (Either SomeException Act))
  let result = bifoldMap (\e -> "An error occurred into the act creation: " ++ show e) (\p -> "Act " ++ getName p ++ " Created Successfully!!") eitherActCreated
  putStrLn result

removeProjectRoute :: IO ()
removeProjectRoute = do
  newEmptyProject <- getEmptyProject
  eitherProjectRemoved <- (try (remove newEmptyProject) :: IO (Either SomeException Project))
  let result = bifoldMap (\e -> "An error occurred into the project removal: " ++ show e) (\p -> "Project " ++ getName p ++ " Removed Successfully!!") eitherProjectRemoved
  putStrLn result


removeSceneRoute :: IO ()
removeSceneRoute = do
  eitherSceneRemoved <- E.runExceptT (do
                                       (prjName, srnName) <- E.ExceptT $ fmap Right getSceneName
                                       scene <- E.ExceptT $ loadScene prjName srnName
                                       E.ExceptT (try (remove scene)))
  let result = bifoldMap (\e -> "An error occurred into the scene removal: " ++ show e) (\p -> "Scene " ++ getName p ++ " Removed Successfully!!") eitherSceneRemoved
  putStrLn result


removeActRoute :: IO ()
removeActRoute = do
  newEmptyAct <- getEmptyAct
  eitherActRemoved <- (try (remove newEmptyAct) :: IO (Either SomeException Act))
  let result = bifoldMap (\e -> "An error occurred into the act removal: " ++ show e) (\p -> "Act " ++ getName p ++ " Removed Successfully!!") eitherActRemoved
  putStrLn result


searchByNameRoute :: IO ()
searchByNameRoute = do
  putStrLn "Insert the name to search, or part of it: "
  searchTerm <- getLine
  projects <- loadProjects
  let results = (searchByName searchTerm . flatten) projects
  putStrLn $  showElementsName results

showProjectsRoute :: IO ()
showProjectsRoute = do
  projects <- loadProjects
  putStrLn $ showElements projects

showProjectsByModifiedDateRoute :: IO ()
showProjectsByModifiedDateRoute = do
    projects <- loadProjects
    let projectSorted = sortByModifiedDate projects
    putStrLn $ showElements projectSorted
