module LanguageExtensions (readMaybe, writeFileIfNotExists, maybeToEither) where

import           System.Path    (toFilePath)
import           System.Path.IO (Absolute, Path, doesFileExist)
--
-- Haskell is not perfect, I don't want to add a library just for silly defaults.
--

--https://stackoverflow.com/questions/8066850/why-doesnt-haskells-prelude-read-return-a-maybe
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _         -> Nothing

writeFileIfNotExists :: String -> Path Absolute -> IO ()
writeFileIfNotExists content filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then return ()
    else writeFile (toFilePath filePath) content

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a (Just b) = Right b
maybeToEither a Nothing  = Left a
