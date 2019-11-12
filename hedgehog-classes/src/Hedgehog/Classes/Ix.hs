{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Ix (ixLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Ix (Ix(..))

ixLaws :: forall a. (Ix a, Eq a, Show a) => Gen a -> Laws
ixLaws gen = Laws "Ix"
  [ ("InRange", ixInRange gen)
  , ("RangeIndex", ixRangeIndex gen)
  , ("MapIndexRange", ixMapIndexRange gen)
  , ("RangeSize", ixRangeSize gen)
  ]

type IxProp a =
  ( Eq a
  , Ix a
  , Show a
  ) => Gen a -> Property

ixInRange :: IxProp a
ixInRange gen = property $ do
  (l,u) <- forAll $ genValidRange gen 
  i <- forAll gen
  inRange (l,u) i === elem i (range (l,u))

ixRangeIndex :: IxProp a
ixRangeIndex gen = property $ do
  (l,u,i) <- forAll $ genInRange gen  
  range (l,u) !! index (l,u) i === i

ixMapIndexRange :: IxProp a
ixMapIndexRange gen = property $ do
  (l,u) <- forAll $ genValidRange gen 
  map (index (l,u)) (range (l,u)) === [0 .. rangeSize (l,u) - 1]

ixRangeSize :: IxProp a
ixRangeSize gen = property $ do
  (l,u) <- forAll $ genValidRange gen 
  rangeSize (l,u) === length (range (l,u))
