{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Semigroup
  ( semigroupLaws
  , commutativeSemigroupLaws
  , exponentialSemigroupLaws
  , idempotentSemigroupLaws
  , rectangularBandSemigroupLaws
  ) where

import Data.Semigroup (Semigroup(..))
import Hedgehog
import Hedgehog.Classes.Common
import Data.List.NonEmpty
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Foldable as Foldable

semigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
semigroupLaws gen = Laws "Semigroup"
  [ ("Associative", semigroupAssociative gen)
  , ("Concatenation", semigroupConcatenation gen)
  , ("Times", semigroupTimes gen)
  ]

commutativeSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
commutativeSemigroupLaws gen = Laws "Commutative Semigroup"
  [ ("Commuative", semigroupCommutative gen)
  ]

exponentialSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
exponentialSemigroupLaws gen = Laws "Exponential Semigroup"
  [ ("Exponential", semigroupExponential gen)
  ]

idempotentSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
idempotentSemigroupLaws gen = Laws "Idempotent Semigroup"
  [ ("Idempotent", semigroupIdempotent gen)
  ]

rectangularBandSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
rectangularBandSemigroupLaws gen = Laws "Rectangular Band Semigroup"
  [ ("Rectangular Band", semigroupRectangularBand gen)
  ]

semigroupAssociative :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupAssociative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  a <> (b <> c) === (a <> b) <> c

semigroupCommutative :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupCommutative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  a <> b === b <> a

semigroupConcatenation :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupConcatenation gen = property $ do
  a <- forAll gen 
  as <- forAll $ genSmallList gen
  let ne = a :| as
  sconcat ne === Foldable.foldr1 (<>) ne

semigroupTimes :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupTimes gen = property $ do
  a <- forAll gen
  n <- forAll (Gen.int Range.constantBounded)
  stimes n a === Foldable.foldr1 (<>) (replicate n a)

semigroupExponential :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupExponential gen = property $ do
  a <- forAll gen
  b <- forAll gen
  n <- forAll (Gen.int Range.constantBounded)
  stimes n (a <> b) === stimes n a <> stimes n b

semigroupIdempotent :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupIdempotent gen = property $ do
  a <- forAll gen
  a <> a === a

semigroupRectangularBand :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupRectangularBand gen = property $ do
  a <- forAll gen
  b <- forAll gen
  a <> b <> a === a
