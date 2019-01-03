{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Foldable (foldableLaws) where

import Hedgehog
import Hedgehog.Classes.Common

--import qualified Data.Foldable as Foldable

foldableLaws ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Laws
foldableLaws _gen = Laws "Foldable"
  [
  ]

--foldableFold ::
--  ( Foldable f
--  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
--  ) => Gen (f a) -> Property
--foldableFold gen = property $ do
--  a <- forAll gen
--  Foldable.fold a === Foldable.foldMap id a


