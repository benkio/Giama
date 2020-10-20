module Controller.Router (
  createProjectRoute
  , createSceneRoute
  , createActRoute
  , showProjects
  , showProjectsByModifiedDate
  , removeElement
  , searchByName) where

import           Control.Exception                 (SomeException)
import           Data.Bifoldable                   (bifoldMap)
import           Domain.HasName                    (HasName (..))
import           Domain.Project                    (Project, createEmptyProject)
import           Persistence.FileSystem.Createable (Createable (..))

createProjectRoute :: IO ()
createProjectRoute = do
  putStrLn "> Please insert Project Name: "
  projectName <- getLine
  newEmptyProject <- createEmptyProject projectName
  eitherProjectCreated <- create newEmptyProject :: IO (Either SomeException Project)
  let result = bifoldMap (\e -> "An error occurred into the project creation: " ++ show e) (\p -> "Project " ++ getName p ++ " Created Successfully!!") eitherProjectCreated
  putStrLn result

createSceneRoute :: IO ()
createSceneRoute = undefined

createActRoute :: IO ()
createActRoute = undefined


showProjects :: IO ()
showProjects = undefined

showProjectsByModifiedDate :: IO ()
showProjectsByModifiedDate  = undefined

removeElement :: IO ()
removeElement = undefined

searchByName :: IO ()
searchByName = undefined
