{-# LANGUAGE DeriveGeneric #-}

module Spec.Generic (testGeneric) where

import Hedgehog
import Hedgehog.Classes
import qualified Hedgehog.Gen as Gen

import GHC.Generics (Generic(..))

testGeneric :: [(String, [Laws])]
testGeneric =
  [ ("E", listE)
  , ("Bool", listBool) 
  , ("Maybe Bool", listMaybe) 
  ]

listE :: [Laws]
listE = [genericLaws genE (genRep genE)]

listBool :: [Laws]
listBool = [genericLaws Gen.bool (genRep Gen.bool)]

listMaybe :: [Laws]
listMaybe = [genericLaws (Gen.maybe Gen.bool) (genRep (Gen.maybe Gen.bool))]

data E = E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8
  deriving (Eq, Show, Generic)

genRep :: Generic a => Gen a -> Gen (Rep a ())
genRep gen = do
  x <- gen
  pure (from x)

genE :: Gen E
genE = Gen.frequency
  [ (1, pure E1)
  , (1, pure E2)
  , (1, pure E3)
  , (1, pure E4)
  , (1, pure E5)
  , (1, pure E6)
  , (1, pure E7)
  , (1, pure E8)
  ]
