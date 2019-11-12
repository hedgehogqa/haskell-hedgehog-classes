module Spec.Semigroup
  ( testSemigroup
  , testCommutativeSemigroup
  , testExponentialSemigroup
  , testIdempotentSemigroup
  , testRectangularBandSemigroup
  ) where

import Hedgehog.Classes

import Data.Monoid (Sum(..))
import Data.Semigroup (Last(..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testSemigroup :: [(String, [Laws])]
testSemigroup =
  [ ("Last", lawsLast)
  , ("Maybe", lawsMaybe)
  ]

testCommutativeSemigroup :: [(String, [Laws])]
testCommutativeSemigroup =
  [ ("Maybe", commutativeLawsMaybe)
  ]

testExponentialSemigroup :: [(String, [Laws])]
testExponentialSemigroup =
  [ ("Last", exponentialLawsLast)
  , ("Maybe", exponentialLawsMaybe)
  ]

testIdempotentSemigroup :: [(String, [Laws])]
testIdempotentSemigroup =
  [ ("Last", idempotentLawsLast)
  ]

testRectangularBandSemigroup :: [(String, [Laws])]
testRectangularBandSemigroup =
  [ ("Last", rectangularBandLawsLast)
  ]

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-3) 20)

lawsLast, exponentialLawsLast, idempotentLawsLast, rectangularBandLawsLast :: [Laws]
lawsLast = [semigroupLaws genLast]
exponentialLawsLast = [exponentialSemigroupLaws genLast]
idempotentLawsLast = [idempotentSemigroupLaws genLast]
rectangularBandLawsLast = [rectangularBandSemigroupLaws genLast]

genLast :: Gen (Last Integer)
genLast = Last <$> genInteger

lawsMaybe, commutativeLawsMaybe, exponentialLawsMaybe :: [Laws]
lawsMaybe = [semigroupLaws genMaybe]
commutativeLawsMaybe = [commutativeSemigroupLaws genMaybe]
exponentialLawsMaybe = [exponentialSemigroupLaws genMaybe]

genMaybe :: Gen (Maybe (Sum Integer))
genMaybe = Gen.maybe (Sum <$> genInteger)
