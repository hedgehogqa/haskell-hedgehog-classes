{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Applicative (applicativeLaws) where

import Control.Applicative (Applicative(..))

import Hedgehog
import Hedgehog.Classes.Common

applicativeLaws ::
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
applicativeLaws gen = Laws "Applicative"
  [ ("Identity", applicativeIdentity gen)
  , ("Composition", applicativeComposition gen)
  , ("Homomorphism", applicativeHomomorphism gen)
  , ("Interchange", applicativeInterchange gen)
  , ("LiftA2 Part 1", applicativeLiftA2_1 gen)
  , ("LiftA2 Part 2", applicativeLiftA2_2 gen) 
  ]

applicativeIdentity :: forall f.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
applicativeIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  (pure id <*> a) === a

applicativeComposition :: forall f.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
applicativeComposition fgen = property $ do
  u' <- forAll $ fgen genQuadraticEquation
  v' <- forAll $ fgen genQuadraticEquation
  w' <- forAll genSmallInteger

  let u = runQuadraticEquation <$> u'
      v = runQuadraticEquation <$> v'
      w = pure w'
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

applicativeHomomorphism :: forall f.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
applicativeHomomorphism _ = property $ do
  e <- forAll genQuadraticEquation
  a <- forAll genSmallInteger
  let f = runQuadraticEquation e
  (pure f <*> pure a) === (pure (f a) :: f Integer)

applicativeInterchange :: forall f.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
applicativeInterchange fgen = property $ do
  u' <- forAll $ fgen genQuadraticEquation
  y <- forAll genSmallInteger
  let u = fmap runQuadraticEquation u'
  (u <*> pure y) === (pure ($ y) <*> u)

applicativeLiftA2_1 :: forall f.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
applicativeLiftA2_1 fgen = property $ do
  f' <- forAll $ fgen genQuadraticEquation
  x <- forAll $ fgen genSmallInteger
  let f = fmap runQuadraticEquation f'
  (liftA2 id f x) === (f <*> x)

applicativeLiftA2_2 :: forall f.
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
applicativeLiftA2_2 fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  y <- forAll $ fgen genSmallInteger
  let f a b = a * a - b
  (liftA2 f x y) === (f <$> x <*> y)
