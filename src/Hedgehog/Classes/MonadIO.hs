{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.MonadIO (monadIOLaws) where

import Control.Monad.IO.Class (MonadIO(..))

import Hedgehog
import Hedgehog.Classes.Common

monadIOLaws ::
  ( MonadIO f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadIOLaws gen = Laws "MonadIO"
  [ ("Return", monadIOReturn gen)
  , ("Lift", monadIOLift gen)
  ]

type MonadIOProp f =
  ( MonadIO f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

monadIOReturn :: forall f. MonadIOProp f
monadIOReturn _fgen = property $ do
  x <- forAll genSmallInteger
  liftIO (pure x) === (pure x :: f Integer)

monadIOLift :: forall f. MonadIOProp f
monadIOLift _fgen = property $ do
  m <- forAllWith showIO $ genIO genSmallInteger
  f' <- forAll genLinearEquation
  let f = pure . runLinearEquation f'
  let x = liftIO (m >>= f) :: f Integer
  let y = liftIO m >>= (liftIO . f) :: f Integer
  x === y

