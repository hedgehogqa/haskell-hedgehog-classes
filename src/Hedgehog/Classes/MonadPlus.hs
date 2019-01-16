{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.MonadPlus (monadPlusLaws) where

import Control.Monad (MonadPlus(..))

import Hedgehog
import Hedgehog.Classes.Common

monadPlusLaws ::
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadPlusLaws gen = Laws "Monad"
  [ ("Left Identity", monadPlusLeftIdentity gen)
  , ("Right Identity", monadPlusRightIdentity gen)
  , ("Associativity", monadPlusAssociativity gen)
  , ("Left Zero", monadPlusLeftZero gen)
  , ("Right Zero", monadPlusRightZero gen)
  ]

monadPlusLeftIdentity :: forall f.
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadPlusLeftIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  mplus mzero x `heq1` x

monadPlusRightIdentity :: forall f.
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadPlusRightIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  mplus x mzero `heq1` x

monadPlusAssociativity :: forall f.
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadPlusAssociativity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  b <- forAll $ fgen genSmallInteger
  c <- forAll $ fgen genSmallInteger
  mplus a (mplus b c) `heq1` mplus (mplus a b) c

monadPlusLeftZero :: forall f.
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadPlusLeftZero _ = property $ do
  k' :: LinearEquationM f <- forAll genLinearEquationM
  (mzero >>= runLinearEquationM k') `heq1` mzero

monadPlusRightZero :: forall f.
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadPlusRightZero fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  (a >> (mzero :: f Integer)) `heq1` mzero 


