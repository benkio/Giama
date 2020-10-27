{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
module Domain.Element (Element, elementPack) where

import           Domain.HasModifiedDate (HasModifiedDate (..))
import           Domain.HasName         (HasName (..))

data Element
  where MkElement :: forall a . (HasName a, HasModifiedDate a, Show a) => a -> Element

elementPack :: forall a . (HasName a, HasModifiedDate a, Show a) => a -> Element
elementPack = MkElement

instance HasName Element where
  getName (MkElement a) = getName a

instance HasModifiedDate Element where
  getModifiedDate (MkElement a) = getModifiedDate a

instance Show Element where
  show (MkElement a) = show a
