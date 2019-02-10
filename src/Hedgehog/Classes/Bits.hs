{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Bits (bitsLaws) where

import Data.Bits
import Hedgehog
import Hedgehog.Classes.Common

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Tests the following 'Bits' laws:
--
-- [__Conjunction Idempotence__]: @n '.&.' n@ ≡ @n@
-- [__Disjunction Idempotence__]: @n '.|.' n@ ≡ @n@
-- [__Double Complement__]: @'complement' '.' 'complement'@ ≡ @id@
-- [__Set Bit__]: @'setBit' n i ≡ n '.|.' 'bit' i@
-- [__Clear Bit__]: @'clearBit' n i@ ≡ @n '.&.' 'complement' ('bit' i)@
-- [__Complement Bit__]: @'complement' n i@ ≡ @'xor' n ('bit' i)@
-- [__Clear Zero__]: @'clearBit' 'zeroBits' i@ ≡ @'zeroBits'@
-- [__Set Zero__]: @'setBit' 'zeroBits' i@ ≡ @'zeroBits'@
-- [__Test Zero__]: @'testBit' 'zeroBits' i@ ≡ @'False'@
-- [__Pop Zero__]: @'popCount' 'zeroBits'@ ≡ @0@
-- [__Count Leading Zeros of Zero__]: @'countLeadingZeros' 'zeroBits'@ ≡ @'finiteBitSize' ('undefined' :: a)@
-- [__Count Trailing Zeros of Zero__]: @'countTrailingZeros' 'zeroBits'@ ≡ @'finiteBitSize' ('undefined' :: a)@
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
        , lawContextLawBody = "n .&. n" `congruency` "n"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n;
            in lawWhere [ "n .&. n" `congruency` "n, where", "n = " ++ showN ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

bitsDisjunctionIdempotence :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDisjunctionIdempotence gen = property $ do
  n <- forAll gen
  let lhs = n .|. n; rhs = n; 
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Disjunction Idempotence"
        , lawContextLawBody = "n .|. n" `congruency` "n"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n
            in lawWhere [ "n .|. n" `congruency` "n, where", "n = " ++ showN ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

bitsDoubleComplement :: forall a. (Bits a, Show a) => Gen a -> Property
bitsDoubleComplement gen = property $ do
  n <- forAll gen
  let lhs = complement (complement n); rhs = n;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Double Complement"
        , lawContextLawBody = "complement . complement" `congruency` "id"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n
            in lawWhere [ "complement . complement $ n" `congruency` "id n, where", "n = " ++ showN ]
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
        , lawContextLawBody = "setBit n i" `congruency` "n .|. bit i"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n
                showI = show i
            in lawWhere
                 [ "setBit n i" `congruency` "n .|. bit i, where"
                 , "n = " ++ showN
                 , "i = " ++ showI
                 ]
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
        , lawContextLawBody = "clearBit n i" `congruency` "n .&. complement (bit i)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n
                showI = show i
            in lawWhere
                 [ "clearBit n i" `congruency` "n .&. complement (bit i), where"
                 , "n = " ++ showN
                 , "i = " ++ showI
                 ]
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
        , lawContextLawBody = "complement n i" `congruency` "xor n (bit i)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showN = show n
                showI = show i
            in lawWhere
                 [ "complement n i" `congruency` "xor n (bit i), where"
                 , "n = " ++ showN
                 , "i = " ++ showI
                 ]
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
        , lawContextLawBody = "clearBit zeroBits i" `congruency` "zeroBits"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showI = show i
            in lawWhere
                 [ "clearBit zeroBits i" `congruency` "zeroBits, where"
                 , "zerBits = " ++ showZ
                 , "i = " ++ showI
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
        , lawContextLawBody = "setBit zeroBits i" `congruency` "zeroBits"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showI = show i
            in lawWhere
              [ "setBit zeroBits i" `congruency` "zeroBits, where"
              , "zeroBits = " ++ showZ
              , "i = " ++ showI
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
        , lawContextLawBody = "testBit zeroBits i" `congruency` "False"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showI = show i
            in lawWhere
              [ "testBit zeroBits i" `congruency` "False, where"
              , "zeroBits = " ++ showZ
              , "i = " ++ showI
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx  

bitsPopZero :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsPopZero _ = property $ do
  let z = zeroBits :: a
  let lhs = popCount z; rhs = 0;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Pop Zero"
        , lawContextLawBody = "popCount zeroBits" `congruency` "0"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
            in lawWhere
              [ "popCount zeroBits" `congruency` "0, where"
              , "zeroBits = " ++ showZ
              ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx

bitsCountLeadingZeros :: forall a. (FiniteBits a, Show a) => Gen a -> Property
bitsCountLeadingZeros _ = property $ do
  let z = zeroBits :: a
  let f = finiteBitSize (undefined :: a)
  let lhs = countLeadingZeros z; rhs = f;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Count Leading Zeros of Zero"
        , lawContextLawBody = "countLeadingZeros zeroBits" `congruency` "finiteBitSize (undefined :: a)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showF = show f
            in lawWhere
              [ "countLeadingZeros zeroBits" `congruency` "finiteBitSize (undefined :: a), where"
              , "zeroBits = " ++ showZ
              , "finiteBitSize = " ++ showF
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
        { lawContextLawName = "Count Trailing Zeros of Zero"
        , lawContextLawBody = "countTrailingZeros zeroBits" `congruency` "finiteBitSize (undefined :: a)"
        , lawContextTcName = "Bits"
        , lawContextTcProp =
            let showZ = show z
                showF = show f
            in lawWhere
              [ "countTrailingZeros zeroBits" `congruency` "finiteBitSize (undefined :: a), where"
              , "zeroBits = " ++ showZ
              , "finiteBitSize = " ++ showF
              ]           
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

