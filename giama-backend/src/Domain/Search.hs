module Domain.Search (searchByName) where

import           Data.List      (isInfixOf)
import           Domain.HasName (HasName (..))

searchByName :: HasName a => String -> [a] -> [a]
searchByName searchTerm = filter (isInfixOf searchTerm . getName)
