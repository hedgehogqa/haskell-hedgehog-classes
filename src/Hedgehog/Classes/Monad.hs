{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Monad (monadLaws) where

import Control.Monad (ap)

import Hedgehog
import Hedgehog.Classes.Common

monadLaws ::
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  , Eq a, Show a
  ) => Gen (f a) -> Laws
monadLaws gen = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity gen)
  , ("Right Identity", monadRightIdentity gen)
  , ("Associativity", monadAssociativity gen)
  , ("Return", monadReturn gen)
  , ("Ap", monadAp gen)
  ]

monadLeftIdentity :: forall f a.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
monadLeftIdentity _ = property $ do
  k' :: LinearEquationM f <- forAll genLinearEquationM
  a <- forAll $ genSmallInteger
  let k = runLinearEquationM k'
  (pure a >>= k) === (k a)

monadRightIdentity :: forall f a.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  , Eq a, Show a
  ) => Gen (f a) -> Property
monadRightIdentity gen = property $ do
  m <- forAll gen
  (m >>= pure) === m

monadAssociativity :: forall f a.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
monadAssociativity gen = property $ do
  m <- forAll $ hackReplace genSmallInteger gen
  k' :: LinearEquationM f <- forAll genLinearEquationM
  h' :: LinearEquationM f <- forAll genLinearEquationM
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
  (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

monadReturn :: forall f a.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
monadReturn _ = property $ do
  x <- forAll genSmallInteger
  return x === (pure x :: f Integer)

monadAp :: forall f a.
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => Gen (f a) -> Property
monadAp _ = property $ do
  f' :: f QuadraticEquation <- forAll $ pure <$> genQuadraticEquation
  x :: f Integer <- forAll $ pure <$> genSmallInteger
  let f = fmap runQuadraticEquation f'
  (ap f x) === (f <*> x)
