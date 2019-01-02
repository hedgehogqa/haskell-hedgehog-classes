{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Bits (bitsLaws) where

import Data.Bits
import Hedgehog
import Hedgehog.Classes.Common

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

bitsLaws :: (FiniteBits a, Show a) => Gen a -> Laws
bitsLaws gen = Laws "Bits"
  [ ("Conjunction Idempotence", bitsConjunctionIdempotence gen)
  , ("Disjunction Idempotence", bitsDisjunctionIdempotence gen)
  , ("Double Complement", bitsDoubleComplement gen) 
  , ("Set Bit", bitsSetBit gen) 
  , ("Clear Bit", bitsClearBit gen)
  , ("Complement Bit", bitsComplementBit gen)
  , ("Clear Zero", bitsClearZero gen)
  , ("Set Zero", bitsSetZero gen)
  , ("Test Zero", bitsTestZero gen)
  , ("Pop Zero", bitsPopZero gen)
  , ("Count Leading Zeros of Zero", bitsCountLeadingZeros gen)
  , ("Count Trailing Zeros of Zero", bitsCountTrailingZeros gen)
  ]

newtype BitIndex a = BitIndex Int
  deriving (Show)

genBitIndex :: forall a. FiniteBits a => Gen (BitIndex a)
genBitIndex = let n = finiteBitSize (undefined :: a) in if n > 0
  then fmap BitIndex (Gen.integral $ Range.linear 0 (n - 1))
  else pure (BitIndex 0)

bitsConjunctionIdempotence :: forall a. (Bits a, Show a) => Gen a -> Property
bitsConjunctionIdempotence gen = property $ do
  n <- forAll gen
  n .&. n === n

bitsDisjunctionIdempotence :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDisjunctionIdempotence gen = property $ do
  n <- forAll gen
  n .|. n === n

bitsDoubleComplement :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDoubleComplement gen = property $ do
  n <- forAll gen
  complement (complement n) === n

bitsSetBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsSetBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  setBit n i === n .|. bit i

bitsClearBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsClearBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  clearBit n i === n .&. complement (bit i)

bitsComplementBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsComplementBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  complementBit n i === xor n (bit i)

bitsClearZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsClearZero _ = property $ do
  (BitIndex n) :: BitIndex a <- forAll genBitIndex
  (clearBit zeroBits n :: a) === zeroBits

bitsSetZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsSetZero _ = property $ do
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  (setBit zeroBits i :: a) === bit i

bitsTestZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsTestZero _ = property $ do
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  testBit (zeroBits :: a) i === False

bitsPopZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsPopZero _ = property $ do
  popCount (zeroBits :: a) === 0

bitsCountLeadingZeros :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsCountLeadingZeros _ = property $ do
  countLeadingZeros (zeroBits :: a) === finiteBitSize (undefined :: a)

bitsCountTrailingZeros :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsCountTrailingZeros _ = property $ do
  countTrailingZeros (zeroBits :: a) === finiteBitSize (undefined :: a)
