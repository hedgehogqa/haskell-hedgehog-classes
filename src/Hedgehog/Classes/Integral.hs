{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Integral (integralLaws) where

import Hedgehog
import Hedgehog.Classes.Common

integralLaws :: (Integral a, Show a) => Gen a -> Laws
integralLaws gen = Laws "Integral"
  [ ("Quotient Remainder", integralQuotientRemainder gen)
  , ("Division Modulus", integralDivisionModulus gen)
  , ("Integer Roundtrip", integralIntegerRoundtrip gen)
  ]

integralQuotientRemainder :: forall a. (Integral a, Show a) => Gen a -> Property
integralQuotientRemainder gen = property $ do
  x <- forAll gen
  y <- forAll gen
  let lhs = (quot x y) * y + (rem x y)
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Quotient Remainder", lawContextTcName = "Integral"
        , lawContextLawBody = "quot x y * y + (rem x y)" `congruency` "x"
        , lawContextTcProp =
            let showX = show x; showY = show y;
            in lawWhere
              [ "quot x y * y + (rem x y)" `congruency` "x, where"
              , "x = " ++ showX
              , "y = " ++ showY
              ]
        , lawContextReduced = reduced lhs rhs
        }  
  heqCtx lhs rhs ctx

integralDivisionModulus :: forall a. (Integral a, Show a) => Gen a -> Property
integralDivisionModulus gen = property $ do
  x <- forAll gen
  y <- forAll gen
  let lhs = (div x y) * y + (mod x y)
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Division Modulus", lawContextTcName = "Integral"
        , lawContextLawBody = "(div x y) * y + (mod x y)" `congruency` "x"
        , lawContextTcProp =
            let showX = show x; showY = show y;
            in lawWhere
              [ "(div x y) * y + (mod x y)" `congruency` "x, where"
              , "x = " ++ showX
              , "y = " ++ showY
              ]
        , lawContextReduced = reduced lhs rhs
        } 
  heqCtx lhs rhs ctx  

integralIntegerRoundtrip :: forall a. (Integral a, Show a) => Gen a -> Property
integralIntegerRoundtrip gen = property $ do
  x <- forAll gen
  let lhs = fromInteger (toInteger x)
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Integer Roundtrip", lawContextTcName = "Integral"
        , lawContextLawBody = "fromInteger . toInteger" `congruency` "id"
        , lawContextTcProp =
            let showX = show x;
            in lawWhere
              [ "fromInteger . toInteger $ x" `congruency` "id x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx
