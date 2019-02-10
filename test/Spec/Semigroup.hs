module Spec.Semigroup
  ( testSemigroup
  , testCommutativeSemigroup
  , testExponentialSemigroup
  , testIdempotentSemigroup
  , testRectangularBandSemigroup
  ) where

import Hedgehog.Classes

import Data.Monoid (Sum(..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testSemigroup :: [(String, [Laws])]
testSemigroup = [("Maybe", listMaybe)]

listMaybe :: [Laws]
listMaybe = map ($ genMaybe) [semigroupLaws, commutativeSemigroupLaws]

genMaybe :: Gen (Maybe (Sum Int))
genMaybe = Gen.maybe (fmap Sum $ Gen.int Range.constantBounded)

testCommutativeSemigroup :: [(String, [Laws])]
testCommutativeSemigroup = []

testExponentialSemigroup :: [(String, [Laws])]
testExponentialSemigroup = []

testIdempotentSemigroup :: [(String, [Laws])]
testIdempotentSemigroup = []

testRectangularBandSemigroup :: [(String, [Laws])]
testRectangularBandSemigroup = []
