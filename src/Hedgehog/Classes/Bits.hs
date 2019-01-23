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
  let lhs = n .&. n; rhs = n; 
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Conjunction Idempotence"
        , lawContextLawBody = "n .&. n" ++ congruent ++ "n"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n;
            in concat [ "n .&. n", congruent, "n, where", newline, tab, "n = ", showN ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

bitsDisjunctionIdempotence :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDisjunctionIdempotence gen = property $ do
  n <- forAll gen
  let lhs = n .|. n; rhs = n; 
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Disjunction Idempotence"
        , lawContextLawBody = "n .|. n" ++ congruent ++ "n"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showX = show n
            in showX ++ " .|. " ++ showX ++ "" ++ congruent ++ "" ++ showX
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

bitsDoubleComplement :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDoubleComplement gen = property $ do
  n <- forAll gen
  let lhs = complement (complement n); rhs = n;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Double Complement"
        , lawContextLawBody = "complement (complement n)" ++ congruent ++ "n"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showX = show n
            in "complement (complement " ++ showX ++ ")" ++ congruent ++ "" ++ showX
        , lawContextReduced = reduced lhs rhs 
        }
  
  heqCtx lhs rhs ctx

bitsSetBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsSetBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let lhs = setBit n i; rhs = n .|. bit i;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Set Bit"
        , lawContextLawBody = "setBit n i" ++ congruent ++ "n .|. bit i"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showX = show n
                showI = show i
            in "setBit " ++ showX ++ " " ++ showI ++ "" ++ congruent ++ "" ++ showX ++ " .|. bit " ++ showI
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx

bitsClearBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsClearBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let lhs = clearBit n i; rhs = n .&. complement (bit i)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Clear Bit"
        , lawContextLawBody = "clearBit n i" ++ congruent ++ "n .&. complement (bit i)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n
                showI = show i
            in "clearBit " ++ showN ++ " " ++ showI ++ congruent ++ showN ++ " .&. complement (bit " ++ showI ++ ")"
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

bitsComplementBit :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsComplementBit gen = property $ do
  n <- forAll gen
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let lhs = complementBit n i; rhs = xor n (bit i);
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Complement Bit"
        , lawContextLawBody = "complement n i" ++ congruent ++ "xor n (bit i)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n
                showI = show i
            in "complementBit " ++ showN ++ " " ++ showI ++ congruent ++ "xor " ++ showN ++ " (bit " ++ showI ++ ")"
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx

bitsClearZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsClearZero _ = property $ do
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let z = zeroBits :: a
  let lhs = clearBit z i; rhs = z
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Clear Zero"
        , lawContextLawBody = "clearBit zeroBits i" ++ congruent ++ "zeroBits"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showI = show i
            in concat
              [ "clearBit zeroBits ", showI, congruent, "zeroBits, where"
              , newline, tab, "zeroBits = ", showZ
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

bitsSetZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsSetZero _ = property $ do
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let z = zeroBits :: a
  let lhs = setBit z i; rhs = bit i;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Set Zero"
        , lawContextLawBody = "setBit zeroBits i" ++ congruent ++ "zeroBits"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showI = show i
            in concat
              [ "setBit zeroBits ", showI, congruent, "bit ", showI, ", where"
              , newline, tab, "zeroBits = ", showZ
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

bitsTestZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsTestZero _ = property $ do
  (BitIndex i) :: BitIndex a <- forAll genBitIndex
  let z = zeroBits :: a
  let lhs = testBit z i; rhs = False;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Test Zero"
        , lawContextLawBody = "testBit zeroBits i" ++ congruent ++ "False"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showI = show i
            in concat [ "testBit zeroBits ", showI, congruent, "False, where", newline, tab, "zeroBits = ", showZ ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx  

bitsPopZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsPopZero _ = property $ do
  let z = zeroBits :: a
  let lhs = popCount z; rhs = 0;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Pop Zero"
        , lawContextLawBody = "popZero zeroBits = 0"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
            in concat [ "popCount zeroBits", congruent, "0, where", newline, tab, "zeroBits = ", showZ ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx

bitsCountLeadingZeros :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsCountLeadingZeros _ = property $ do
  let z = zeroBits :: a
  let f = finiteBitSize (undefined :: a)
  let lhs = countLeadingZeros z; rhs = f;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Leading Zeros"
        , lawContextLawBody = "countLeadingZeros zeroBits" ++ congruent ++ "finiteBitSize (undefined :: a)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showF = show f
            in concat
              [ "countLeadingZeros zeroBits", congruent, "finiteBitSize (undefined :: a), where"
              , newline, tab, "zeroBits = " ++ showZ
              , newline, tab, "finiteBitSize = " ++ showF
              ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx

bitsCountTrailingZeros :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsCountTrailingZeros _ = property $ do
  let z = zeroBits :: a
  let f = finiteBitSize (undefined :: a)
  let lhs = countTrailingZeros z; rhs = f;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Trailing Zeros"
        , lawContextLawBody = "countTrailingZeros zeroBits" ++ congruent ++ "finiteBitSize (undefined :: a)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showF = show f
            in concat
              [ "countTrailingZeros zeroBits", congruent, "finiteBitSize (undefined :: a), where"
              , newline, tab, "zeroBits = " ++ showZ
              , newline, tab, "finiteBitSize = " ++ showF
              ]           
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

