module Spec.Semiring
  ( testSemiring
  , testRing
  , testStar
  ) where

import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

testSemiring :: [(String, [Laws])]
testSemiring =
  [ ("Bool", lawsBool)
  , ("Int", lawsInt)
  , ("Int8", lawsInt8)
  , ("Int16", lawsInt16)
  , ("Int32", lawsInt32)
  , ("Int64", lawsInt64)
  , ("Word", lawsWord)
  , ("Word8", lawsWord8)
  , ("Word16", lawsWord16)
  , ("Word32", lawsWord32)
  , ("Word64", lawsWord64)
  ]

testRing :: [(String, [Laws])]
testRing =
  [ ("Int", ringLawsInt)
  , ("Int8", ringLawsInt8)
  , ("Int16", ringLawsInt16)
  , ("Int32", ringLawsInt32)
  , ("Int64", ringLawsInt64)
  , ("Word", ringLawsWord)
  , ("Word8", ringLawsWord8)
  , ("Word16", ringLawsWord16)
  , ("Word32", ringLawsWord32)
  , ("Word64", ringLawsWord64)
  ]

testStar :: [(String, [Laws])]
testStar =
  [ ("Bool", starLawsBool)
  ]

ranged :: (Bounded a, Num a) => (Range.Range a -> b) -> b
ranged f = f Range.constantBounded

lawsBool, starLawsBool :: [Laws]
lawsBool = [semiringLaws Gen.bool]
starLawsBool = [starLaws Gen.bool]

lawsInt, lawsInt8, lawsInt16, lawsInt32, lawsInt64 :: [Laws]
lawsInt = [semiringLaws (ranged Gen.int)]
lawsInt8 = [semiringLaws (ranged Gen.int8)]
lawsInt16 = [semiringLaws (ranged Gen.int16)]
lawsInt32 = [semiringLaws (ranged Gen.int32)]
lawsInt64 = [semiringLaws (ranged Gen.int64)]

lawsWord, lawsWord8, lawsWord16, lawsWord32, lawsWord64 :: [Laws]
lawsWord = [semiringLaws (ranged Gen.word)]
lawsWord8 = [semiringLaws (ranged Gen.word8)]
lawsWord16 = [semiringLaws (ranged Gen.word16)]
lawsWord32 = [semiringLaws (ranged Gen.word32)]
lawsWord64 = [semiringLaws (ranged Gen.word64)]

ringLawsInt, ringLawsInt8, ringLawsInt16, ringLawsInt32, ringLawsInt64 :: [Laws]
ringLawsInt = [ringLaws (ranged Gen.int)]
ringLawsInt8 = [ringLaws (ranged Gen.int8)]
ringLawsInt16 = [ringLaws (ranged Gen.int16)]
ringLawsInt32 = [ringLaws (ranged Gen.int32)]
ringLawsInt64 = [ringLaws (ranged Gen.int64)]

ringLawsWord, ringLawsWord8, ringLawsWord16, ringLawsWord32, ringLawsWord64 :: [Laws]
ringLawsWord = [ringLaws (ranged Gen.word)]
ringLawsWord8 = [ringLaws (ranged Gen.word8)]
ringLawsWord16 = [ringLaws (ranged Gen.word16)]
ringLawsWord32 = [ringLaws (ranged Gen.word32)]
ringLawsWord64 = [ringLaws (ranged Gen.word64)]
