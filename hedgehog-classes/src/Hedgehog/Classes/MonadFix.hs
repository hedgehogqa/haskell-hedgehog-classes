{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.MonadFix (monadFixLaws) where

import Control.Monad.Fix (MonadFix(..))
import Data.Function (fix)

import Hedgehog
import Hedgehog.Classes.Common

monadFixLaws ::
  ( MonadFix f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadFixLaws gen = Laws "MonadFix"
  [ ("Purity", monadFixPurity gen)
  , ("Left Shrinking (or Tightening)", monadFixLeftShrinking gen)
  , ("Sliding", monadFixSliding gen)
  , ("Nesting", monadFixNesting gen)
  ]

type MonadFixProp f =
  ( MonadFix f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

monadFixPurity :: forall f. MonadFixProp f
monadFixPurity _ = property $ do
  h' <- forAll genQuadraticEquation
  let h = runQuadraticEquation h'
  let x = mfix (pure . h) :: f Integer
  let y = pure (fix h) :: f Integer
  x === y  

monadFixLeftShrinking :: forall f. MonadFixProp f
monadFixLeftShrinking fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  f' <- forAll genLinearEquationTwo
  let f a' b' = pure $ runLinearEquationTwo f' a' b'
  let x' = mfix (\x -> a >>= \y -> f x y) :: f Integer
  let y' = a >>= \y -> mfix (\x -> f x y) :: f Integer
  x' === y'

monadFixSliding :: forall f. MonadFixProp f
monadFixSliding _ = property $ do
  f' <- forAll genQuadraticEquation
  let f = pure . runQuadraticEquation f'
  let h !i = let !x = i*i + 7 in x
  let x' = mfix (fmap h . f) :: f Integer
  let y' = fmap h (mfix (f . h)) :: f Integer 

  x' === y'

monadFixNesting :: forall f. MonadFixProp f
monadFixNesting _ = property $ do
  f' <- forAll genLinearEquationTwo
  let f a' b' = pure $ runLinearEquationTwo f' a' b'
  let x' = mfix (\x -> mfix (\y -> f x y)) :: f Integer
  let y' = mfix (\x -> f x x) :: f Integer
  x' === y'

