{-# LANGUAGE LambdaCase #-}

module Hedgehog.Classes.Common.Bottom
  ( Bottom(..), genBottom
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
 
data Bottom a = BottomUndefined | BottomValue a
  deriving (Eq)

instance Show a => Show (Bottom a) where
  show = \case
    BottomUndefined -> "undefined"
    BottomValue a   -> show a

genBottom :: Gen a -> Gen (Bottom a)
genBottom = fmap maybeToBottom . Gen.maybe

maybeToBottom :: Maybe a -> Bottom a
maybeToBottom = \case { Nothing -> BottomUndefined; Just a -> BottomValue a }
