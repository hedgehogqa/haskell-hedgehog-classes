{-# LANGUAGE DerivingVia #-}

module Spec.Contravariant (testContravariant) where

import Hedgehog
import Hedgehog.Classes

import Data.Functor.Contravariant
import Data.Functor.Const (Const(..))
import Data.Functor.Sum (Sum(..))
import Data.Functor.Product (Product(..))
import Data.Proxy (Proxy(..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testContravariant :: [(String, [Laws])]
testContravariant =
  [ ("Proxy", listProxy)
  , ("Const", listConst)
  , ("Sum", listSum)
  , ("Product", listProduct)
--  , ("Bad Contravariant", listBadContravariant) 
  ]

listProxy :: [Laws]
listProxy = [contravariantLaws genProxy]

listConst :: [Laws]
listConst = [contravariantLaws genConst]

listSum :: [Laws]
listSum = [contravariantLaws genSum]

listProduct :: [Laws]
listProduct = [contravariantLaws genProduct]

--listBadContravariant :: [Laws]
--listBadContravariant = [contravariantLaws genBadContravariant]

genProxy :: Gen a -> Gen (Proxy a)
genProxy = const (pure Proxy)

genConst :: Gen b -> Gen (Const Integer b)
genConst _ = fmap Const (Gen.integral (Range.linear 0 20))

genSum :: Gen a -> Gen (Sum (Const ()) (Const ()) a)
genSum _genA =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, pure $ InL (Const ()))
      , (1 + fromIntegral n, pure $ InR (Const ()))
      ]

genProduct :: Gen a -> Gen (Product (Const ()) (Const ()) a)
genProduct _genA = do
  pure (Pair (Const ()) (Const ()))

{-
newtype BadContravariant a = BadContravariant (a -> a)

instance Show (BadContravariant a) where
  show _ = "BadContravariant <<Endo>>"

instance Eq a => Eq (BadContravariant a) where
  BadContravariant f == BadContravariant g = False

instance Contravariant BadContravariant where
  contramap f _ = BadContravariant id

genBadContravariant :: Gen a -> Gen (BadContravariant a)
genBadContravariant = fmap (BadContravariant . const)
-}

