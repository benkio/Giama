module Persistence.FileSystem.Act(createAct, loadActs) where

import           Control.Exception                         (Exception)
import           Data.List                                 (sortOn)
import           Domain.Act                                (Act (..))
import           Domain.Scene                              (Scene (..))
import           Persistence.FileSystem.Classes            (HasFilePath (..))
import           Persistence.FileSystem.DirectoryFunctions (applyDirWithResult)
import           System.Directory                          (listDirectory)
import           System.FilePath                           ((</>))

createAct :: Exception e => Act -> IO (Either e Act)
createAct a = do
  let actPath = getFilePath a
  applyDirWithResult a (`writeFile` (actName a ++ "\n\n" ++ actContent a)) actPath

loadActs :: Scene -> IO Scene
loadActs s = do
  let scenePath = getFilePath s
  actFilePaths <- listDirectory scenePath
  acts <- traverse (\a ->
                          do
                            ac <- readFile $ getFilePath s </> a
                            let (ap, an) = span (/= '_') a
                            return (Act {
                                       actParentProjectName = sceneParentProjectName s
                                       ,actParentSceneName  = sceneName s
                                       ,actPosition         = read ap :: Int
                                       ,actName             = tail an
                                       ,actContent          = ac
                                       })
                      ) actFilePaths
  return $ s { sceneActs = sortOn actPosition acts }
