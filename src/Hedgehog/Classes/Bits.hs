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

conjunctionIdempotence :: Show a => a -> LawContext
conjunctionIdempotence x = LawContext
  { lawContextLawName = "Conjunction Idempotence"
  , lawContextLawBody = "forall n. n .&. n == n"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showX = show x
      in showX ++ " .&. " ++ showX ++ " == " ++ showX
  }

bitsConjunctionIdempotence :: forall a. (Bits a, Show a) => Gen a -> Property
bitsConjunctionIdempotence gen = property $ do
  n <- forAll gen
  let ctx = showLawContext (conjunctionIdempotence n) 
  heqCtx (n .&. n) n ctx  

disjunctionIdempotence :: Show a => a -> LawContext
disjunctionIdempotence x = LawContext
  { lawContextLawName = "Conjunction Idempotence"
  , lawContextLawBody = "forall n. n .|. n == n"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showX = show x
      in showX ++ " .|. " ++ showX ++ " == " ++ showX
  }

bitsDisjunctionIdempotence :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDisjunctionIdempotence gen = property $ do
  n <- forAll gen
  let ctx = showLawContext (disjunctionIdempotence n)
  heqCtx (n .|. n) n ctx

doubleComplement :: Show a => a -> LawContext
doubleComplement x = LawContext
  { lawContextLawName = "Double Complement"
  , lawContextLawBody = "forall n. complement (complement n) == n"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showX = show x
      in "complement (complement " ++ showX ++ ") == " ++ showX
  }

bitsDoubleComplement :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDoubleComplement gen = property $ do
  n <- forAll gen
  let ctx = showLawContext (doubleComplement n) 
  heqCtx (complement (complement n)) n ctx

setBitLaw :: Show a => a -> Int -> LawContext
setBitLaw x i = LawContext
  { lawContextLawName = "Set Bit"
  , lawContextLawBody = "forall n. setBit n i == n .|. bit i"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showX = show x
          showI = show i
      in "setBit " ++ showX ++ " " ++ showI ++ " == " ++ showX ++ " .|. bit " ++ showI
  }

bitsSetBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsSetBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let ctx = showLawContext (setBitLaw n i) 
  heqCtx (setBit n i) (n .|. bit i) ctx

clearBitLaw :: Show a => a -> Int -> LawContext
clearBitLaw n i = LawContext
  { lawContextLawName = "Clear Bit"
  , lawContextLawBody = "forall n. clearBit n i == n .&. complement (bit i)"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showN = show n
          showI = show i
      in "clearBit " ++ showN ++ " " ++ showI ++ " == " ++ showN ++ " .&. complement (bit " ++ showI ++ ")"
  }

bitsClearBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsClearBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let ctx = showLawContext (clearBitLaw n i)
  heqCtx (clearBit n i) (n .&. complement (bit i)) ctx

complementBitLaw :: Show a => a -> Int -> LawContext
complementBitLaw n i = LawContext
  { lawContextLawName = "Complement Bit"
  , lawContextLawBody = "forall n. complement n i == xor n (bit i)"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showN = show n
          showI = show i
      in "complementBit " ++ showN ++ " " ++ showI ++ " == xor " ++ showN ++ " (bit " ++ showI ++ ")"
  }

bitsComplementBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsComplementBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let ctx = showLawContext (complementBitLaw n i) 
  heqCtx (complementBit n i) (xor n (bit i)) ctx

clearZeroLaw :: Show a => a -> Int -> LawContext
clearZeroLaw z i = LawContext
  { lawContextLawName = "Clear Zero"
  , lawContextLawBody = "clearBit zeroBits i == zeroBits"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showZ = show z
          showI = show i
      in "clearBit zeroBits " ++ showI ++ " == zeroBits," ++ "\n    where zeroBits = " ++ showZ
  }

bitsClearZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsClearZero _ = property $ do
  (BitIndex n) :: BitIndex a <- forAll genBitIndex
  let z = zeroBits :: a
  let ctx = showLawContext (clearZeroLaw z n)
  heqCtx (clearBit z n) z ctx

setZeroLaw :: Show a => a -> Int -> LawContext
setZeroLaw z i = LawContext
  { lawContextLawName = "Set Zero"
  , lawContextLawBody = "setBit zeroBits i == zeroBits"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showZ = show z
          showI = show i
      in "setBit zeroBits " ++ showI ++ " == bit " ++ showI ++ ", \n    where zeroBits = " ++ showZ
  }

bitsSetZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsSetZero _ = property $ do
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let z = zeroBits :: a
  let ctx = showLawContext (setZeroLaw z i) 
  heqCtx (setBit z i) (bit i) ctx

testZeroLaw :: Show a => a -> Int -> LawContext
testZeroLaw z i = LawContext
  { lawContextLawName = "Test Zero"
  , lawContextLawBody = "testBit zeroBits i == False"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showZ = show z
          showI = show i
      in "testBit zeroBits " ++ showI ++ " == False, \n    where zeroBits = " ++ showZ
  }

bitsTestZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsTestZero _ = property $ do
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let z = zeroBits :: a
  let ctx = showLawContext (testZeroLaw z i) 
  heqCtx (testBit z i) False ctx  

popZeroLaw :: Show a => a -> LawContext
popZeroLaw z = LawContext
  { lawContextLawName = "Pop Zero"
  , lawContextLawBody = "popZero zeroBits = 0"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showZ = show z
      in "popCount zeroBits == 0,\n    where zeroBits = " ++ showZ
  }

bitsPopZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsPopZero _ = property $ do
  let z = zeroBits :: a
  let ctx = showLawContext (popZeroLaw z)
  heqCtx (popCount z) 0 ctx

countLeadingZerosLaw :: (Show a, Show b) => a -> b -> LawContext
countLeadingZerosLaw z f = LawContext
  { lawContextLawName = "Leading Zeros"
  , lawContextLawBody = "countLeadingZeros zeroBits == finiteBitSize (undefined @a)"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showZ = show z
          showF = show f
      in "countLeadingZeros zeroBits == finiteBitSize (undefined @a),\n    where"
         ++ "\n        zeroBits = " ++ showZ
         ++ "\n        finiteBitSize = " ++ showF
  }

countTrailingZerosLaw :: (Show a, Show b) => a -> b -> LawContext
countTrailingZerosLaw z f = LawContext
  { lawContextLawName = "Trailing Zeros"
  , lawContextLawBody = "countTrailingZeros zeroBits == finiteBitSize (undefined :: a)"
  , lawContextTcName = "Bits"
  , lawContextTcProp =
      let showZ = show z
          showF = show f
      in "countTrailingZeros zeroBits == finiteBitSize (undefined :: a),\n    where"
         ++ "\n        zeroBits = " ++ showZ
         ++ "\n        finiteBitSize = " ++ showF
  }

bitsCountLeadingZeros :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsCountLeadingZeros _ = property $ do
  let z = zeroBits :: a
  let f = finiteBitSize (undefined :: a)
  let ctx = showLawContext (countLeadingZerosLaw z f)
  heqCtx (countLeadingZeros z) f ctx

bitsCountTrailingZeros :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsCountTrailingZeros _ = property $ do
  let z = zeroBits :: a
  let f = finiteBitSize (undefined :: a)
  let ctx = showLawContext (countTrailingZerosLaw z f)
  heqCtx (countTrailingZeros z) f ctx

