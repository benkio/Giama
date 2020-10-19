module Persistence.FileSystem.Loadable (Loadable(..), loadProjects) where

import           Data.List                          (sortOn)
import           Domain.Act                         (Act (..))
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..))
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
                                      , sceneModifiedDate = modifiedDate
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
                       actParentProjectName  = sceneParentProjectName x
                       , actParentSceneName  = sceneName x
                       , actModifiedDate     = modifiedDate
                       , actPosition         = read ap :: Int
                       , actName             = tail an
                       , actContent          = ac
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
