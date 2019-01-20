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
  [ ("Sum Integer", lawsSum)
  , ("Product Integer", lawsProduct)
  , ("Maybe Integer", lawsMaybe)
  ]

lawsSum, lawsProduct, lawsMaybe, lawsAp :: [Laws]

lawsSum = [monoidLaws genSum]
lawsProduct = [monoidLaws genProduct]
lawsMaybe = [monoidLaws genMaybe]
lawsAp = [monoidLaws genAp]

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear (-3) 20)

genSum :: Gen (Sum Integer)
genSum = fmap coerce genInteger

genProduct :: Gen (Product Integer)
genProduct = fmap coerce genInteger

genMaybe :: Gen (Maybe (Sum Integer))
genMaybe = Gen.maybe genSum

genAp :: Gen (Ap Maybe (Sum Integer))
genAp = fmap coerce genMaybe
