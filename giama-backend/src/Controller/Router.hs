module Controller.Router (
  createProjectRoute
  , createSceneRoute
  , createActRoute
  , showProjectsRoute
  , showProjectsByModifiedDateRoute
  , removeProjectRoute
  , removeSceneRoute
  , removeActRoute
  , moveSceneRoute
  , moveActRoute
  , searchByNameRoute) where

import           Control.Applicative               (liftA2)
import           Control.Exception                 (SomeException, try)
import           Control.Monad.IO.Class            (liftIO)
import qualified Control.Monad.Trans.Except        as E (ExceptT (..), except,
                                                         runExceptT)
import           Data.Bifoldable                   (bifoldMap)
import           Data.List                         (maximumBy)
import           Data.Maybe                        (fromMaybe)
import           Domain.Act                        (Act (..), createEmptyAct)
import           Domain.BusinessError              (BusinessError)
import           Domain.HasName                    (HasName (..))
import           Domain.Identifiers                (ActId, ProjectId,
                                                    SceneId)
import           Domain.Project                    (Project (..),
                                                    createEmptyProject, flatten,
                                                    showElements,
                                                    showElementsName)
import           Domain.Scene                      (Scene (..),
                                                    createEmptyScene,
                                                    extractAct)
import           Domain.Search                     (searchByName)
import           Domain.Sort                       (sortByModifiedDate)
import           LanguageExtensions                (readMaybe)
import           Persistence.FileSystem.Createable (Createable (..))
import           Persistence.FileSystem.Loadable   (loadAct, loadProject,
                                                    loadProjects, loadScene)
import           Persistence.FileSystem.Movable    (Movable (..))
import           Persistence.FileSystem.Removable  (Removable (..))

getProjectId :: IO ProjectId
getProjectId =   do
  putStrLn "> Please insert Project name: "
  getLine

getEmptyProject :: IO Project
getEmptyProject = do
  prjName <- getProjectId
  createEmptyProject prjName

getSceneId :: IO (ProjectId, SceneId)
getSceneId = do
  prjName <- getProjectId
  putStrLn "> Please insert Scene's name: "
  srnName <- getLine
  return (prjName, srnName)

getActId :: IO (ProjectId, SceneId, ActId)
getActId = do
  (prjName, srnName) <- getSceneId
  putStrLn "> Please insert Act's name: "
  aName <- getLine
  return (prjName, srnName, aName)

getElementPosition :: String -> IO Int
getElementPosition element = do
  putStrLn ("> Please insert "++ element ++"'s position(default 0): ")
  (\s -> fromMaybe 0 (readMaybe s :: Maybe Int)) <$> getLine

getNewEmptyScene :: IO (Either BusinessError Scene)
getNewEmptyScene = do
  (prjName, srnName) <- getSceneId
  eitherProject <- loadProject prjName
  let eitherNewScenePosition = (+1) . scenePosition . maximumBy (\s s' -> compare (scenePosition s) (scenePosition s')) . projectScenes <$> eitherProject
  createEmptyScene prjName srnName `traverse` eitherNewScenePosition

getNewEmptyAct :: IO (Either BusinessError Act)
getNewEmptyAct = do
  (prjName, srnName, aName) <- getActId
  eitherScene <- loadScene prjName srnName
  let eitherNewActPosition = (+1) . actPosition . maximumBy (\a a' -> compare (actPosition a) (actPosition a')) . sceneActs <$> eitherScene
  traverse (\positions -> createEmptyAct prjName srnName (fst positions) aName (snd positions)) (liftA2 (\s ap -> (scenePosition s, ap)) eitherScene eitherNewActPosition)

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
  eitherActCreated <- E.runExceptT (do
                                       newEmptyAct <- E.ExceptT getNewEmptyAct
                                       E.ExceptT (try (create newEmptyAct)))
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
                                       (prjName, srnName) <- E.ExceptT $ fmap Right getSceneId
                                       scene <- E.ExceptT $ loadScene prjName srnName
                                       E.ExceptT (try (remove scene)))
  let result = bifoldMap (\e -> "An error occurred into the scene removal: " ++ show e) (\p -> "Scene " ++ getName p ++ " Removed Successfully!!") eitherSceneRemoved
  putStrLn result


removeActRoute :: IO ()
removeActRoute = do
  eitherActRemoved <- E.runExceptT (do
                                       (prjName, srnName, aName) <- E.ExceptT $ fmap Right getActId
                                       act <- E.ExceptT $ loadAct prjName srnName aName
                                       E.ExceptT (try (remove act)))
  let result = bifoldMap (\e -> "An error occurred into the act removal: " ++ show e) (\p -> "Act " ++ getName p ++ " Removed Successfully!!") eitherActRemoved
  putStrLn result

moveSceneRoute :: IO ()
moveSceneRoute = do
  eitherSceneMoved <- E.runExceptT (do
                                       (prjName, srnName) <- E.ExceptT $ fmap Right getSceneId
                                       liftIO $ putStrLn "> Please insert Target Project name: "
                                       targetPrjName <- liftIO getLine
                                       scene <- E.ExceptT $ loadScene prjName srnName
                                       targetPosition <- liftIO $ getElementPosition "Scene"
                                       E.ExceptT (move scene targetPosition targetPrjName prjName))
  let result = bifoldMap (\e -> "An error occurred into the scene movement: " ++ show e) (\p -> "Scene " ++ p ++ " Moved Successfully!!") eitherSceneMoved
  putStrLn result

moveActRoute :: IO ()
moveActRoute = do
  eitherActMoved <- E.runExceptT (do
                                       (prjName, srnName, aName) <- E.ExceptT $ fmap Right getActId
                                       (targetPrjName, targetScnName) <- E.ExceptT $ fmap Right getSceneId
                                       targetScene <- E.ExceptT $ loadScene targetPrjName targetScnName
                                       sourceScene <- E.ExceptT $ loadScene prjName srnName
                                       act <- E.except $ extractAct aName sourceScene
                                       targetPosition <- liftIO $ getElementPosition "Act"
                                       E.ExceptT (move act targetPosition targetScene sourceScene))
  let result = bifoldMap (\e -> "An error occurred into the act movement: " ++ show e) (\p -> "Act " ++ getName p ++ " Moved Successfully!!") eitherActMoved
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
