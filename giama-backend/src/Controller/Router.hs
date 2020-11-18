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
import           Domain.Act                        (Act (..), createEmptyAct, actPosition)
import           Domain.BusinessError              (BusinessError)
import           Domain.HasName                    (HasName (..))
import           Domain.Identifiers                (ActId, ProjectId,
                                                    SceneId, projectIdConstructor, sceneIdConstructor, actIdConstructor, sceneIdFromActId)
import           Domain.Project                    (Project (..),
                                                    createEmptyProject, flatten,
                                                    showElements,
                                                    showElementsName)
import           Domain.Scene                      (Scene (..),
                                                    createEmptyScene,
                                                    extractAct,
                                                    scenePosition)
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
  prjn <- getLine
  return $ projectIdConstructor prjn

getSceneName :: IO String
getSceneName = do
  putStrLn "> Please insert Scene's name: "
  getLine

getActName :: IO String
getActName = do
  putStrLn "> Please insert Act's name: "
  getLine

getElementPosition :: String -> IO Int
getElementPosition element = do
  putStrLn ("> Please insert "++ element ++"'s position(default 0): ")
  (\s -> fromMaybe 0 (readMaybe s :: Maybe Int)) <$> getLine

getNewEmptyProject :: IO Project
getNewEmptyProject = do
  prjName <- getProjectId
  createEmptyProject prjName

getNewEmptyScene :: IO (Either BusinessError Scene)
getNewEmptyScene = do
  prjId <- getProjectId
  eitherProject <- loadProject prjId
  let eitherNewScenePosition = (+1) . scenePosition . maximumBy (\s s' -> compare (scenePosition s) (scenePosition s')) . projectScenes <$> eitherProject
  srnName <- getSceneName
  (createEmptyScene .  sceneIdConstructor prjId srnName) `traverse` eitherNewScenePosition

getNewEmptyAct :: IO (Either BusinessError Act)
getNewEmptyAct = do
  eitherScene <- getNewEmptyScene
  let eitherNewActPosition = (+1) . actPosition . maximumBy (\a a' -> compare (actPosition a) (actPosition a')) . sceneActs <$> eitherScene
  aName <- getActName
  traverse createEmptyAct (liftA2 (\s ap -> actIdConstructor (sceneId s) aName ap) eitherScene eitherNewActPosition)

createProjectRoute :: IO ()
createProjectRoute = do
  newEmptyProject <- getNewEmptyProject
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
  newEmptyProject <- getNewEmptyProject
  eitherProjectRemoved <- (try (remove newEmptyProject) :: IO (Either SomeException Project))
  let result = bifoldMap (\e -> "An error occurred into the project removal: " ++ show e) (\p -> "Project " ++ getName p ++ " Removed Successfully!!") eitherProjectRemoved
  putStrLn result


removeSceneRoute :: IO ()
removeSceneRoute = do
  eitherSceneRemoved <- E.runExceptT (do
                                       emptyScene <- E.ExceptT $ getNewEmptyScene
                                       scene <- E.ExceptT $ loadScene (sceneId emptyScene)
                                       E.ExceptT (try (remove scene)))
  let result = bifoldMap (\e -> "An error occurred into the scene removal: " ++ show e) (\p -> "Scene " ++ getName p ++ " Removed Successfully!!") eitherSceneRemoved
  putStrLn result


removeActRoute :: IO ()
removeActRoute = do
  eitherActRemoved <- E.runExceptT (do
                                       emptyAct <- E.ExceptT $ getNewEmptyAct
                                       act <- E.ExceptT $ loadAct (actId emptyAct)
                                       E.ExceptT (try (remove act)))
  let result = bifoldMap (\e -> "An error occurred into the act removal: " ++ show e) (\p -> "Act " ++ getName p ++ " Removed Successfully!!") eitherActRemoved
  putStrLn result

moveSceneRoute :: IO ()
moveSceneRoute = do
  eitherSceneMoved <- E.runExceptT (do
                                       emptyScene <- E.ExceptT $ getNewEmptyScene
                                       liftIO $ putStrLn "> Please insert Target Project name: "
                                       targetPrjId <- liftIO (projectIdConstructor <$> getLine)
                                       scene <- E.ExceptT $ loadScene (sceneId emptyScene)
                                       targetPosition <- liftIO $ getElementPosition "Scene"
                                       E.ExceptT (move scene targetPosition targetPrjId))
  let result = bifoldMap (\e -> "An error occurred into the scene movement: " ++ show e) (\p -> "Scene " ++ getName p ++ " Moved Successfully!!") eitherSceneMoved
  putStrLn result

moveActRoute :: IO ()
moveActRoute = do
  eitherActMoved <- E.runExceptT (do
                                       emptyAct <- E.ExceptT $ getNewEmptyAct
                                       sourceScene <- E.ExceptT $ loadScene (sceneIdFromActId (actId emptyAct))
                                       act <- E.except $ extractAct (actId emptyAct) sourceScene

                                       liftIO $ putStrLn "> Please insert Target Project name: "
                                       targetPrjName <- liftIO getLine
                                       liftIO $ putStrLn "> Please insert Target Scene name: "
                                       targetSceneName <- liftIO getLine
                                       let targetSceneId = sceneIdConstructor (projectIdConstructor targetPrjName) targetSceneName 0
                                       targetPosition <- liftIO $ getElementPosition "Act"
                                       E.ExceptT (move act targetPosition targetSceneId))
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
