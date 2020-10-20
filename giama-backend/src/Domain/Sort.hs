module Domain.Sort (sortByModifiedDate) where

import           Data.List              (sortOn)
import           Data.Ord               (Down (..))
import           Domain.HasModifiedDate (HasModifiedDate (..))

sortByModifiedDate :: HasModifiedDate a => [a] -> [a]
sortByModifiedDate = sortOn (Down . getModifiedDate)
