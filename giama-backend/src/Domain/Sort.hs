module Domain.Sort (sortByModifiedDate) where

import           Data.List              (sortOn)
import           Domain.HasModifiedDate (HasModifiedDate (..))

sortByModifiedDate :: HasModifiedDate a => [a] -> [a]
sortByModifiedDate = sortOn getModifiedDate
