{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Monad (monadLaws) where

import Control.Monad (ap)

import Hedgehog
import Hedgehog.Classes.Common

monadLaws ::
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadLaws gen = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity gen)
  , ("Right Identity", monadRightIdentity gen)
  , ("Associativity", monadAssociativity gen)
  , ("Return", monadReturn gen)
  , ("Ap", monadAp gen)
  ]

monadLeftIdentity :: forall f.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadLeftIdentity _ = property $ do
  k' :: LinearEquationM f <- forAll genLinearEquationM
  a <- forAll $ genSmallInteger
  let k = runLinearEquationM k'
  (pure a >>= k) === (k a)

monadRightIdentity :: forall f.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadRightIdentity fgen = property $ do
  m <- forAll $ fgen genSmallInteger
  (m >>= pure) === m

monadAssociativity :: forall f.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadAssociativity fgen = property $ do
  m <- forAll $ fgen genSmallInteger
  k' :: LinearEquationM f <- forAll genLinearEquationM
  h' :: LinearEquationM f <- forAll genLinearEquationM
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
  (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

monadReturn :: forall f.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadReturn _ = property $ do
  x <- forAll genSmallInteger
  return x === (pure x :: f Integer)

monadAp :: forall f.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
monadAp _ = property $ do
  f' :: f QuadraticEquation <- forAll $ pure <$> genQuadraticEquation
  x :: f Integer <- forAll $ pure <$> genSmallInteger
  let f = fmap runQuadraticEquation f'
  (ap f x) === (f <*> x)
