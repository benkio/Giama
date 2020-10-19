module Persistence.FileSystem.Config (rootPath) where

import           System.Path (Absolute, Path, fromAbsoluteFilePath)

rootPath :: Path Absolute
rootPath = fromAbsoluteFilePath "/home/benkio/temp/giamaProjects/"
