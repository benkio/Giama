module Persistence.FileSystem.Loadable (Loadable(..), loadProjects, loadProject, loadScene, loadAct) where

import           Control.Monad.Trans.Except         (ExceptT (..), except,
                                                     runExceptT)
import           Data.List                          (find, sortOn)
import           Domain.Act                         (Act (..), actPosition)
import           Domain.BusinessError               (BusinessError (..))
import           Domain.HasName                     (HasName (..))
import           Domain.Identifiers                 (ActId, ProjectId,
                                                     SceneId, projectIdConstructor, sceneIdConstructor, actIdConstructor, projectIdFromSceneId,  sceneIdFromActId)
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..), scenePosition)
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
                                      sceneId            = sceneIdConstructor (projectId x) (tail sn) (read sp :: Int)
                                      , sceneModifiedDate    = modifiedDate
                                      ,sceneActs             = []         })) (\ss x -> x { projectScenes = sortOn scenePosition ss}) p


instance Loadable Scene where
  load s = loadPattern (\x a->
          do
            ac <- (readFile . toFilePath) a
            modifiedDate <- getModificationTime a
            let (ap, an) = span (/= '_') (toUnrootedFilePath (takeBaseName a))
                scnId = sceneId x
                aId = actIdConstructor scnId (tail an) (read ap :: Int)
            return (Act {
                       actId                 = aId
                       , actModifiedDate         = modifiedDate
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
                        projectId = projectIdConstructor prjName
                        ,projectModifiedDate = prjModifiedDate
                        , projectScenes = [] })) projectAbsolutePaths

loadProject :: ProjectId -> IO (Either BusinessError Project)
loadProject prjId = do
  maybeToEither ProjectNotFound
       . find (\ p -> getName p == getName prjId)
       <$> loadProjects

loadScene :: SceneId -> IO (Either BusinessError Scene)
loadScene scnId = runExceptT $ do
  project <- ExceptT $ loadProject (projectIdFromSceneId scnId)
  except $ maybeToEither SceneNotFound (find (\s -> getName s == getName scnId) (projectScenes project))

loadAct :: ActId ->  IO (Either BusinessError Act)
loadAct aId = runExceptT $ do
  scene <- ExceptT $ loadScene (sceneIdFromActId aId)
  except $ maybeToEither ActNotFound (find (\a -> getName a == getName aId) (sceneActs scene))
