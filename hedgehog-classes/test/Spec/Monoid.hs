module Spec.Monoid (testMonoid, testCommutativeMonoid) where

import Hedgehog (Gen)
import Hedgehog.Classes

import Data.Coerce (coerce)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Monoid

testMonoid :: [(String, [Laws])]
testMonoid =
  [ ("Sum Integer", lawsSum)
  , ("Product Integer", lawsProduct)
  , ("Maybe Integer", lawsMaybe)
  , ("Ap Maybe Integer", lawsAp)
  ]

testCommutativeMonoid :: [(String, [Laws])]
testCommutativeMonoid =
  [ ("Sum Integer", commutativeLawsSum)
  , ("Product Integer", commutativeLawsProduct)
  , ("Maybe Integer", commutativeLawsMaybe)
  ]

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-3) 20)

lawsSum, commutativeLawsSum :: [Laws]
lawsSum = [monoidLaws genSum]
commutativeLawsSum = [commutativeMonoidLaws genSum]

genSum :: Gen (Sum Integer)
genSum = fmap coerce genInteger

lawsProduct, commutativeLawsProduct :: [Laws]
lawsProduct = [monoidLaws genProduct]
commutativeLawsProduct = [commutativeMonoidLaws genProduct]

genProduct :: Gen (Product Integer)
genProduct = fmap coerce genInteger

lawsMaybe, commutativeLawsMaybe :: [Laws]
lawsMaybe = [monoidLaws genMaybe]
commutativeLawsMaybe = [commutativeMonoidLaws genMaybe]

genMaybe :: Gen (Maybe (Sum Integer))
genMaybe = Gen.maybe genSum

lawsAp :: [Laws]
lawsAp = [monoidLaws genAp]

genAp :: Gen (Ap Maybe (Sum Integer))
genAp = fmap coerce genMaybe
