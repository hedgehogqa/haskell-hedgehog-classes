module Main (main) where

import Hedgehog.Classes

import Spec.Alternative
import Spec.Applicative
import Spec.Arrow
import Spec.Bifoldable
import Spec.Bifunctor
import Spec.Binary
import Spec.Bitraversable
import Spec.Bits
import Spec.Category
import Spec.Comonad
import Spec.Contravariant
import Spec.Enum
import Spec.Eq
import Spec.Foldable
import Spec.Functor
import Spec.Generic
import Spec.Integral
--import Spec.Ix
import Spec.Json
import Spec.Monad
import Spec.Monoid
import Spec.Ord
import Spec.Prim
import Spec.Profunctor
import Spec.Semigroup
import Spec.Semiring
import Spec.Show
import Spec.Storable
import Spec.Traversable

main :: IO Bool
main = lawsCheckMany allLaws

allNullaryLaws :: [(String, [Laws])]
allNullaryLaws = testBits
  ++ testEnum
  ++ testBoundedEnum
  ++ testBinary
  ++ testEq
  ++ testGeneric
  ++ testIntegral
--  ++ testIx
  ++ testJson
  ++ testMonoid
  ++ testCommutativeMonoid
  ++ testOrd
  ++ testPrim
  ++ testSemigroup
  ++ testCommutativeSemigroup
  ++ testExponentialSemigroup
  ++ testIdempotentSemigroup
  ++ testRectangularBandSemigroup
  ++ testSemiring
  ++ testRing
  ++ testStar
  ++ testShow
  ++ testShowRead
  ++ testStorable

allUnaryLaws :: [(String, [Laws])]
allUnaryLaws = testAlternative
  ++ testApplicative
  ++ testComonad
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
  ++ testProfunctor

allLaws :: [(String, [Laws])]
allLaws = allNullaryLaws ++ allUnaryLaws ++ allBinaryLaws
