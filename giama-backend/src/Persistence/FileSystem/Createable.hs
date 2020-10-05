{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.FileSystem.Createable (Createable(..)) where

import           Control.Exception                         (Exception)
import           Control.Monad.Trans.Except                (ExceptT (..),
                                                            runExceptT)
import           Domain.Act                                (Act (..))
import           Domain.Project                            (Project (..))
import           Domain.Scene                              (Scene (..))
import           Persistence.FileSystem.DirectoryFunctions (applyDirWithResult)
import           Persistence.FileSystem.HasFilePath        (HasFilePath (..))
import           System.Directory                          (createDirectory)

createParent :: (HasFilePath a, Exception c, Createable b c) => a -> (a -> [b]) -> ([b] -> a -> a) -> IO (Either c a)
createParent p extractChild buildParent = runExceptT $
  do
    let parentPath = getFilePath p
    parent <- ExceptT $ applyDirWithResult p createDirectory parentPath
    childs <- ExceptT $ sequence <$> traverse create (extractChild parent)
    return $ buildParent childs parent

class (HasFilePath a, Exception e) => Createable a e where
  create :: a -> IO (Either e a)

instance (Exception e) => Createable Project e where
  create p = createParent p projectScenes (\s p -> p { projectScenes = s })

instance Exception e => Createable Scene e where
  create s = createParent s sceneActs (\a s -> s { sceneActs = a })

instance Exception e => Createable Act e where
  create a = do
    let actPath = getFilePath a
    applyDirWithResult a (`writeFile` (actName a ++ "\n\n" ++ actContent a)) actPath
