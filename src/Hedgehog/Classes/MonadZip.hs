{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.MonadZip (monadZipLaws) where

import Control.Arrow (Arrow(..))
import Control.Monad.Zip (MonadZip(mzip))

import Hedgehog
import Hedgehog.Classes.Common

monadZipLaws ::
  ( MonadZip f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadZipLaws gen = Laws "Monad"
  [ ("Naturality", monadZipNaturality gen)
  ]

monadZipNaturality :: forall f.
  ( MonadZip f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadZipNaturality fgen = property $ do
  f' <- forAll genLinearEquation
  g' <- forAll genLinearEquation
  let f = runLinearEquation f'
      g = runLinearEquation g'
  ma <- forAll $ fgen genSmallInteger
  mb <- forAll $ fgen genSmallInteger
  (fmap (f *** g) (mzip ma mb)) === (mzip (fmap f ma) (fmap g mb))
