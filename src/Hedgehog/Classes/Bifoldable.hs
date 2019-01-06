{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Bifoldable (bifoldableLaws, bifoldableFunctorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Monoid (Endo(..), Sum(..), Product(..))

bifoldableLaws :: forall f.
  ( Bifoldable f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bifoldableLaws gen = Laws "Bifoldable"
  [ ("Identity", bifoldableIdentity gen)
  , ("FoldMap", bifoldableFoldMap gen)
  , ("Foldr", bifoldableFoldr gen)
  ]

bifoldableFunctorLaws :: forall f.
  ( Bifoldable f, Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bifoldableFunctorLaws gen = Laws "Bifoldable/Bifunctor"
  [ ("Composition", bifoldableFunctorComposition gen)
  , ("FoldMap", bifoldableFunctorFoldMap gen)
  ]

type BifoldableProp f =
  ( Bifoldable f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

bifoldableIdentity :: forall f. BifoldableProp f
bifoldableIdentity fgen = property $ do
  x <- forAll $ fgen genSmallSum genSmallSum
  bifold x === bifoldMap id id x

bifoldableFoldMap :: forall f. BifoldableProp f
bifoldableFoldMap fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  bifoldMap Sum Sum x === bifoldr (mappend . Sum) (mappend . Sum) mempty x

bifoldableFoldr :: forall f. BifoldableProp f
bifoldableFoldr fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  f' <- forAll genLinearEquationTwo
  g' <- forAll genLinearEquationTwo
  let f = runLinearEquationTwo f'
  let g = runLinearEquationTwo g'
  let z0 = 0
  bifoldr f g z0 x === appEndo (bifoldMap (Endo . f) (Endo . g) x) z0

type BifoldableFunctorProp f =
  ( Bifoldable f, Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

bifoldableFunctorComposition :: forall f. BifoldableFunctorProp f
bifoldableFunctorComposition fgen = property $ do
  x <- forAll $ fgen genSmallSum genSmallSum
  bifoldMap Product Product x === bifold (bimap Product Product x)

bifoldableFunctorFoldMap :: forall f. BifoldableFunctorProp f
bifoldableFunctorFoldMap fgen = property $ do
  x <- forAll $ fgen genSmallSum genSmallSum
  let h (Sum s) = s * s
  let i (Sum s) = s + s
  bifoldMap Sum Sum (bimap h i x) === bifoldMap (Sum . h) (Sum . i) x

