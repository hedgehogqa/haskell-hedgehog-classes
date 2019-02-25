{-# language CPP #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}

#ifndef HAVE_SEMIRINGS

module Hedgehog.Classes.Semiring () where

#else

module Hedgehog.Classes.Semiring (semiringLaws, ringLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Prelude hiding (Num(..))
import Data.Semiring

-- | Tests the following 'Semiring' laws:
--
-- [__Additive Left Identity__]: @'zero' '+' x@ ≡ @x@
-- [__Additive Right Identity__]: @x '+' 'zero'@ ≡ @x@
-- [__Additive Associativity__]: @x '+' (y '+' z)@ ≡ @(x '+' y) '+' z@
-- [__Additive Commutativity__]: @x '+' y@ ≡ @y '+' x@
-- [__Multiplicative Left Identity__]: @'one' '*' x@ ≡ @x@ 
-- [__Multiplicative Right Identity__]: @x '*' 'one'@ ≡ @x@ 
-- [__Multiplicative Associativity__]: @x '*' (y '*' z)@ ≡ @(x '*' y) '*' z@ 
-- [__Multiplicatiion Left-Distributes Over Addtion__]: @x '*' (y '+' z)@ ≡ @(x '*' y) '+' (x '*' z)@ 
-- [__Multiplication Right-Distibutes Over Addition__]: @(y '+' z) '*' x@ ≡ @(y '*' x) '+' (z '*' x)@
-- [__Multiplicative Left Annihilation__]: @'zero' '*' x@ ≡ @'zero'@ 
-- [__Multiplicative Right Annihilation__]: @x '*' 'zero'@ ≡ @'zero'@ 
semiringLaws :: (Semiring a, Eq a, Show a) => Gen a -> Laws
semiringLaws gen = Laws "Semiring"
  [ ("Additive Left Identity", semiringAdditiveLeftIdentity gen)
  , ("Additive Right Identity", semiringAdditiveRightIdentity gen)
  , ("Additive Associativity", semiringAdditiveAssociativity gen)
  , ("Additive Commutativity", semiringAdditiveCommutativity gen)
  , ("Multiplicative Left Identity", semiringMultiplicativeLeftIdentity gen)
  , ("Multiplicative Right Identity", semiringMultiplicativeRightIdentity gen)
  , ("Multiplicative Associativity", semiringMultiplicativeAssociativity gen)
  , ("Multiplication Left-Distributes Over Addition", semiringLeftMultiplicationDistributes gen)
  , ("Multiplication Right-Distributes Over Addition", semiringRightMultiplicationDistributes gen)
  , ("Multiplicative Left Annihilation", semiringLeftAnnihilation gen) 
  , ("Multiplicative Right Annihilation", semiringRightAnnihilation gen) 
  ]

-- | Tests the following 'Ring' laws:
--
-- [__Additive Inverse__]: @'negate' x '+' x@ ≡ @'zero'@
ringLaws :: (Ring a, Eq a, Show a) => Gen a -> Laws
ringLaws gen = Laws "Ring"
  [ ("Additive Inverse", ringAdditiveInverse gen)
  ]

type SemiringProp a = (Semiring a, Eq a, Show a) => Gen a -> Property
type RingProp a = (Ring a, Eq a, Show a) => Gen a -> Property

ringAdditiveInverse :: forall a. RingProp a
ringAdditiveInverse gen = property $ do
  a <- forAll gen
  let lhs = negate a + a
  let rhs = zero :: a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Additive Inverse", lawContextTcName = "Ring"
        , lawContextLawBody = "negate a + a" `congruency` "zero"
        , lawContextTcProp =
            let showA = show a; showZ = show (zero :: a);
            in lawWhere
              [ "negate a + a" `congruency` "zero, where"
              , "a = " ++ showA 
              , "zero = " ++ showZ
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

semiringAdditiveLeftIdentity :: forall a. SemiringProp a
semiringAdditiveLeftIdentity gen = property $ do
  x <- forAll gen
  let lhs = zero + x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Additive Left Identity", lawContextTcName = "Semiring"
        , lawContextLawBody = "zero + x" `congruency` "x"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showZ = show (zero :: a);
            in lawWhere
              [ "zero + x" `congruency` "x, where"
              , "x = " ++ showX
              , "zero = " ++ showZ
              ]
        }  
  heqCtx lhs rhs ctx

semiringAdditiveRightIdentity :: forall a. SemiringProp a
semiringAdditiveRightIdentity gen = property $ do
  x <- forAll gen
  let lhs = x + zero
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Additive Right Identity", lawContextTcName = "Semiring"
        , lawContextLawBody = "x + zero" `congruency` "x"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showZ = show (zero :: a);
            in lawWhere
              [ "x + zero" `congruency` "x, where"
              , "x = " ++ showX
              , "zero = " ++ showZ
              ]
        }  
  heqCtx lhs rhs ctx

semiringAdditiveAssociativity :: forall a. SemiringProp a
semiringAdditiveAssociativity gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = a + (b + c)
  let rhs = (a + b) + c
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Additive Associativity", lawContextTcName = "Semiring"
        , lawContextLawBody = "x + (y + z)" `congruency` "(x + y) + z"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show a; showY = show b; showZ = show c;
            in lawWhere
              [ "x + (y + z)" `congruency` "(x + y) + z, where"
              , "x = " ++ showX
              , "y = " ++ showY
              , "z = " ++ showZ
              ]
        }  
  heqCtx lhs rhs ctx

semiringAdditiveCommutativity :: forall a. SemiringProp a
semiringAdditiveCommutativity gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let lhs = a + b
  let rhs = b + a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Additive Commutativity", lawContextTcName = "Semiring"
        , lawContextLawBody = "x + y" `congruency` "y + x"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show a; showY = show b;
            in lawWhere
              [ "x + y" `congruency` "y + x, where"
              , "x = " ++ showX
              , "y = " ++ showY
              ]
        }  
  heqCtx lhs rhs ctx

semiringMultiplicativeLeftIdentity :: forall a. SemiringProp a
semiringMultiplicativeLeftIdentity gen = property $ do
  x <- forAll gen
  let lhs = one * x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Multiplicative Left Identity", lawContextTcName = "Semiring"
        , lawContextLawBody = "one * x" `congruency` "x"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showO = show (one :: a);
            in lawWhere
              [ "one * x" `congruency` "x, where"
              , "x = " ++ showX
              , "one = " ++ showO
              ] 
        }     
  heqCtx lhs rhs ctx
  
semiringMultiplicativeRightIdentity :: forall a. SemiringProp a
semiringMultiplicativeRightIdentity gen = property $ do
  x <- forAll gen
  let lhs = x * one
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Multiplicative Right Identity", lawContextTcName = "Semiring"
        , lawContextLawBody = "x * one" `congruency` "x"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showO = show (one :: a);
            in lawWhere
              [ "x * one" `congruency` "x, where"
              , "x = " ++ showX
              , "one = " ++ showO
              ]
        }    
  heqCtx lhs rhs ctx

semiringMultiplicativeAssociativity :: forall a. SemiringProp a
semiringMultiplicativeAssociativity gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = a * (b * c)
  let rhs = (a * b) * c
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Multiplicative Associativity", lawContextTcName = "Semiring"
        , lawContextLawBody = "x * (y * z)" `congruency` "(x * y) * z"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show a; showY = show b; showZ = show c;
            in lawWhere
              [ "x * (y * z)" `congruency` "(x * y) * z, where"
              , "x = " ++ showX
              , "y = " ++ showY
              , "z = " ++ showZ
              ] 
        }    
  heqCtx lhs rhs ctx

semiringLeftMultiplicationDistributes :: forall a. SemiringProp a
semiringLeftMultiplicationDistributes gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = a * (b + c)
  let rhs = (a * b) + (a * c)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Multiplication Left-Distributes Over Addition", lawContextTcName = "Semiring"
        , lawContextLawBody = "x * (y + z)" `congruency` "(x * y) + (x * z)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show a; showY = show b; showZ = show c;
            in lawWhere
              [ "x * (y + z)" `congruency` "(x * y) + (x * z), where"
              , "x = " ++ showX
              , "y = " ++ showY
              , "z = " ++ showZ
              ]     
        }    
  heqCtx lhs rhs ctx

semiringRightMultiplicationDistributes :: forall a. SemiringProp a
semiringRightMultiplicationDistributes gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = (a + b) * c
  let rhs = (a * c) + (b * c)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Multiplication Right-Distributes Over Addition", lawContextTcName = "Semiring"
        , lawContextLawBody = "(y + z) * x" `congruency` "(y * x) + (z * x)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show a; showY = show b; showZ = show c;
            in lawWhere
              [ "(y + z) * x" `congruency` "(y * x) + (z * x), where"
              , "x = " ++ showX
              , "y = " ++ showY
              , "z = " ++ showZ
              ]
        }    
  heqCtx lhs rhs ctx

semiringLeftAnnihilation :: forall a. SemiringProp a
semiringLeftAnnihilation gen = property $ do
  x <- forAll gen
  let lhs = zero * x
  let rhs = zero

  let ctx = contextualise $ LawContext
        { lawContextLawName = "Multiplicative Left Annihilation", lawContextTcName = "Semiring"
        , lawContextLawBody = "zero * x" `congruency` "zero"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showZ = show (zero :: a);
            in lawWhere
              [ "zero * x" `congruency` "zero, where"
              , "x = " ++ showX
              , "zero = " ++ showZ
              ]
        }    
  heqCtx lhs rhs ctx

semiringRightAnnihilation :: forall a. SemiringProp a
semiringRightAnnihilation gen = property $ do
  x <- forAll gen
  let lhs = x * zero
  let rhs = zero  
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Multiplicative Right Annihilation", lawContextTcName = "Semiring"
        , lawContextLawBody = "x * zero" `congruency` "zero"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showZ = show (zero :: a);
            in lawWhere
              [ "x * zero" `congruency` "zero, where"
              , "x = " ++ showX
              , "zero = " ++ showZ
              ]
        }    
  heqCtx lhs rhs ctx
#endif
