{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Hedgehog.Classes

import Spec.Alternative
import Spec.Applicative
import Spec.Arrow
import Spec.Bifoldable
import Spec.Bifunctor
import Spec.Bitraversable
import Spec.Bits
import Spec.Category
import Spec.Contravariant
import Spec.Enum
import Spec.Eq
import Spec.Foldable
import Spec.Functor
import Spec.Generic
import Spec.Integral
import Spec.Ix
import Spec.Json
import Spec.Monad
import Spec.MonadIO
import Spec.MonadPlus
import Spec.MonadZip
import Spec.Monoid
import Spec.Ord
import Spec.Semigroup
import Spec.Show
import Spec.ShowRead
import Spec.Storable
import Spec.Traversable

main :: IO Bool
main = lawsCheckMany allLaws

allNullaryLaws :: [(String, [Laws])]
allNullaryLaws = testBits
  ++ testEnum
  ++ testBoundedEnum
  ++ testEq
  ++ testGeneric
  ++ testIntegral
  ++ testIx
  ++ testJson
  ++ testMonoid
  ++ testCommutativeMonoid
  ++ testOrd
  ++ testSemigroup
  ++ testCommutativeSemigroup
  ++ testExponentialSemigroup
  ++ testIdempotentSemigroup
  ++ testRectangularBandSemigroup
  ++ testShow
  ++ testShowRead
  ++ testStorable

allUnaryLaws :: [(String, [Laws])]
allUnaryLaws = testAlternative
  ++ testApplicative
  ++ testContravariant
  ++ testFoldable
  ++ testFunctor
  ++ testMonad
  ++ testMonadIO
  ++ testMonadPlus
  ++ testMonadZip
  ++ testTraversable

allBinaryLaws :: [(String, [Laws])]
allBinaryLaws = testArrow
  ++ testBifoldable
  ++ testBifoldableFunctor
  ++ testBifunctor
  ++ testBitraversable
  ++ testCategory
  ++ testCommutativeCategory

allLaws :: [(String, [Laws])]
allLaws = testFoldable --allNullaryLaws ++ allUnaryLaws ++ allBinaryLaws

{-
tightenTypes :: [(String, [Laws])] -> [(String, [Laws])]
tightenTypes = map tighten . List.groupBy (\(a,_) (c,_) -> a == c)
  where
    tighten :: [(String, [Laws])] -> (String, [Laws])
    tighten xs = case foldMap (\(a,b) -> (Monoid.First (Just a), b)) xs of
      (Monoid.First a, b) -> (fromMaybe mempty a, b) 
-}

