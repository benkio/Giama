module Persistence.FileSystem.Loadable (Loadable(..), loadProjects) where

import           Data.List                          (sortOn)
import           Domain.Act                         (Act (..))
import           Domain.Project                     (Project (..))
import           Domain.Scene                       (Scene (..))
import           Persistence.FileSystem.Config      (rootPath)
import           Persistence.FileSystem.HasFilePath (HasFilePath (..))
import           System.Directory                   (listDirectory)
import           System.FilePath                    (takeBaseName, (</>))

loadPattern :: HasFilePath a => (a -> FilePath -> IO b) -> ([b] -> a -> a) -> a -> IO a
loadPattern childBuilder parentBulder parent = do
    let parentPath = getFilePath parent
    childFilePaths <- listDirectory parentPath
    childs <- traverse (childBuilder parent) childFilePaths
    return $ parentBulder childs parent

class HasFilePath a => Loadable a where
  load :: a -> IO a

instance Loadable Project where
  load p = loadPattern (\x s->
                          let (sp, sn) = span (/= '_') s
                          in
                            load (Scene {
                                     sceneParentProjectName = projectName x
                                     ,scenePosition         = read sp :: Int
                                     ,sceneName             = tail sn
                                     ,sceneActs             = []         })) (\ss x -> x { projectScenes = sortOn scenePosition ss}) p


instance Loadable Scene where
  load s = loadPattern (\x a->
          do
            ac <- readFile $ getFilePath x </> a
            let (ap, an) = span (/= '_') $ takeBaseName a
            return (Act {
                       actParentProjectName = sceneParentProjectName x
                       ,actParentSceneName  = sceneName x
                       ,actPosition         = read ap :: Int
                       ,actName             = tail an
                       ,actContent          = ac
                       })) (\as x -> x { sceneActs = sortOn actPosition as}) s

loadProjects :: IO [Project]
loadProjects = do
  projectFilePaths <- listDirectory rootPath
  traverse (\p -> load (Project { projectName = p, projectScenes = [] })) projectFilePaths
