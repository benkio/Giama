module Persistence.FileSystem.Loadable (Loadable(..), loadProjects, loadProject, loadScene, loadAct) where

import           Control.Monad.Trans.Except         (ExceptT (..), except,
                                                     runExceptT)
import           Data.List                          (find, sortOn)
import           Domain.Act                         (Act (..))
import           Domain.BusinessError               (BusinessError (..))
import           Domain.HasName                     (HasName (..))
import           Domain.Identifiers                 (ActName, ProjectName,
                                                     SceneName)
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..))
import           LanguageExtensions                 (maybeToEither)
import           Persistence.FileSystem.Config      (rootPath)
import           Persistence.FileSystem.HasFilePath (HasFilePath (..))
import           System.Path                        (Absolute, Path,
                                                     takeBaseName, toFilePath,
                                                     toUnrootedFilePath, (</>))
import           System.Path.IO                     (getDirectoryContents,
                                                     getModificationTime)

loadPattern :: HasFilePath a => (a -> Path Absolute -> IO b) -> ([b] -> a -> a) -> a -> IO a
loadPattern childBuilder parentBulder parent = do
    let parentPath = getFilePath parent
    childFilePaths <- getDirectoryContents parentPath
    let childFileAbsolutePaths = (parentPath </> ) <$> childFilePaths
    childs <- traverse (childBuilder parent) childFileAbsolutePaths
    return $ parentBulder childs parent

class HasFilePath a => Loadable a where
  load :: a -> IO a

instance Loadable Project where
  load p = loadPattern (\x s->
                           do
                             let (sp, sn) = span (/= '_') (toUnrootedFilePath (takeBaseName s))
                             modifiedDate <- getModificationTime s
                             load (Scene {
                                      sceneParentProjectName = projectName x
                                      , sceneModifiedDate    = modifiedDate
                                      ,scenePosition         = read sp :: Int
                                      ,sceneName             = tail sn
                                      ,sceneActs             = []         })) (\ss x -> x { projectScenes = sortOn scenePosition ss}) p


instance Loadable Scene where
  load s = loadPattern (\x a->
          do
            ac <- (readFile . toFilePath) a
            modifiedDate <- getModificationTime a
            let (ap, an) = span (/= '_') (toUnrootedFilePath (takeBaseName a))
            return (Act {
                       actParentProjectName      = sceneParentProjectName x
                       , actParentSceneName      = sceneName x
                       , actParentScenePosition  = scenePosition x
                       , actModifiedDate         = modifiedDate
                       , actPosition             = read ap :: Int
                       , actName                 = tail an
                       , actContent              = ac
                       })) (\as x -> x { sceneActs = sortOn actPosition as}) s

loadProjects :: IO [Project]
loadProjects = do
  projectFilePaths <- getDirectoryContents rootPath
  let projectAbsolutePaths = (rootPath </>) <$> projectFilePaths
  traverse (\p -> do
               prjModifiedDate <- getModificationTime p
               let prjName = (toUnrootedFilePath . takeBaseName) p
               load (Project {
                        projectName = prjName,
                        projectModifiedDate =
                        prjModifiedDate, projectScenes = [] })) projectAbsolutePaths

loadProject :: ProjectName -> IO (Either BusinessError Project)
loadProject prjName = do
  maybeToEither ProjectNotFound
       . find (\ p -> getName p == prjName)
       <$> loadProjects

loadScene :: ProjectName -> SceneName -> IO (Either BusinessError Scene)
loadScene prjName scnName = runExceptT $ do
  project <- ExceptT $ loadProject prjName
  except $ maybeToEither SceneNotFound (find (\s -> getName s == scnName) (projectScenes project))

loadAct :: ProjectName -> SceneName -> ActName ->  IO (Either BusinessError Act)
loadAct prjName scnName aName = runExceptT $ do
  scene <- ExceptT $ loadScene prjName scnName
  except $ maybeToEither ActNotFound (find (\a -> getName a == aName) (sceneActs scene))
