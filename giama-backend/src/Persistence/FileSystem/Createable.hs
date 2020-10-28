{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Createable (Createable(..)) where

import           Control.Exception                         (Exception)
import           Control.Monad.Trans.Except                (ExceptT (..),
                                                            runExceptT)
import           Domain.Act                                (Act (..))
import           Domain.Project                            (Project (..))
import           Domain.Scene                              (Scene (..))
import           LanguageExtensions                        (writeFileIfNotExists)
import           Persistence.FileSystem.DirectoryFunctions (applyDirWithResult)
import           Persistence.FileSystem.HasFilePath        (HasFilePath (..))
import           System.Path                               (toFilePath)
import           System.Path.IO                            (createDirectoryIfMissing)

createParent :: (HasFilePath a, Createable b) => a -> (a -> [b]) -> ([b] -> a -> a) -> IO a
createParent p extractChild buildParent = do
    let parentPath = getFilePath p
    parent <- applyDirWithResult p (createDirectoryIfMissing False) parentPath
    childs <- traverse create (extractChild parent)
    return $ buildParent childs parent

class (HasFilePath a) => Createable a where
  create :: a -> IO a

instance Createable Project where
  create p = createParent p projectScenes (\s p -> p { projectScenes = s })

instance Createable Scene where
  create s = createParent s sceneActs (\a s -> s { sceneActs = a })

instance Createable Act where
  create a = do
    let actPath = getFilePath a
    applyDirWithResult a (writeFileIfNotExists (actName a ++ "\n\n" ++ actContent a)) actPath
