{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.HasChild where

class HasChild a b where
  getChilds :: a -> [b]
