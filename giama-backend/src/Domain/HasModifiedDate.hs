module Domain.HasModifiedDate where

import           Data.Time.Clock (UTCTime)

class HasModifiedDate a where
  getModifiedDate :: a -> UTCTime
