{-# language TypeApplications #-}

module Spec.Ord (testOrd) where

import Hedgehog.Classes
import Hedgehog (Gen)
import GHC.Natural

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

testOrd :: [(String, [Laws])]
testOrd =
  [ ("Int", listInt)
  , ("Int8", listInt8)
  , ("Int16", listInt16)
  , ("Int32", listInt32)
  , ("Int64", listInt64)
  , ("Word", listWord)
  , ("Word8", listWord8)
  , ("Word16", listWord16)
  , ("Word32", listWord32)
  , ("Word64", listWord64)
  , ("Natural", listNatural)
  , ("Pair", listPair)
  ]

ranged :: (Integral a) => (Range.Range a -> b) -> b
ranged f = f (Range.linear 0 100)

listInt, listInt8, listInt16, listInt32, listInt64 :: [Laws]
listInt = [ordLaws (ranged Gen.int)]
listInt8 = [ordLaws (ranged Gen.int8)]
listInt16 = [ordLaws (ranged Gen.int16)]
listInt32 = [ordLaws (ranged Gen.int32)]
listInt64 = [ordLaws (ranged Gen.int64)]

listWord, listWord8, listWord16, listWord32, listWord64 :: [Laws]
listWord = [ordLaws (ranged Gen.word)]
listWord8 = [ordLaws (ranged Gen.word8)]
listWord16 = [ordLaws (ranged Gen.word16)]
listWord32 = [ordLaws (ranged Gen.word32)]
listWord64 = [ordLaws (ranged Gen.word64)]

listNatural :: [Laws]
listNatural = [ordLaws (ranged @Natural Gen.integral)]

listPair :: [Laws]
listPair = [ordLaws (genPair (ranged Gen.int) (ranged Gen.int8))]

data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

genPair :: Gen a -> Gen b -> Gen (Pair a b)
genPair genA genB = Pair <$> genA <*> genB
