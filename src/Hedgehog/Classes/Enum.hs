{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Enum (enumLaws, boundedEnumLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import qualified Hedgehog.Gen as Gen

-- | Tests the following 'Enum' laws:
--
-- [__Succ-Pred Identity__]: @'succ' '.' 'pred'@ ≡ @'id'@
-- [__Pred-Succ Identity__]: @'pred' '.' 'succ'@ ≡ @'id'@
enumLaws :: (Enum a, Eq a, Show a) => Gen a -> Laws
enumLaws gen = Laws "Enum"
  [ ("Succ Pred Identity", succPredIdentity gen)
  , ("Pred Succ Identity", predSuccIdentity gen)
  ]

-- | Tests the same laws as 'enumLaws', but uses the 'Bounded'
--   constraint to ensure that 'succ' and 'pred' behave as though
--   they are total. This should always be preferred if your type
--   has a 'Bounded' instance.
boundedEnumLaws :: (Bounded a, Enum a, Eq a, Show a) => Gen a -> Laws
boundedEnumLaws gen = Laws "Bounded Enum"
  [ ("Succ Pred Identity", succPredBoundedIdentity gen)
  , ("Pred Succ Identity", predSuccBoundedIdentity gen)
  ]

succPredIdentity :: forall a. (Enum a, Eq a, Show a) => Gen a -> Property
succPredIdentity gen = property $ do
  x <- forAll gen
  let lhs = succ (pred x); rhs = x;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Succ-Pred Identity"
        , lawContextLawBody = "succ . pred" `congruency` "id"
        , lawContextTcName = "Enum"
        , lawContextTcProp =
            let showX = show x
            in lawWhere
              [ "succ . pred $ x" `congruency` "id x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

predSuccIdentity :: forall a. (Enum a, Eq a, Show a) => Gen a -> Property
predSuccIdentity gen = property $ do
  x <- forAll gen
  let lhs = pred (succ x); rhs = x;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Pred-Succ Identity"
        , lawContextLawBody = "pred . succ" `congruency` "id"
        , lawContextTcName = "Enum"
        , lawContextTcProp =
            let showX = show x
            in lawWhere
              [ "pred . succ $ x" `congruency` "id x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

succPredBoundedIdentity :: forall a. (Bounded a, Enum a, Eq a, Show a) => Gen a -> Property
succPredBoundedIdentity gen = property $ do
  x <- forAll $ Gen.filter (/= minBound) gen
  let lhs = succ (pred x); rhs = x;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Succ-Pred Identity"
        , lawContextLawBody = "succ . pred" `congruency` "id"
        , lawContextTcName = "Enum"
        , lawContextTcProp =
            let showX = show x
            in lawWhere
              [ "succ . pred $ x" `congruency` "id x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

predSuccBoundedIdentity :: forall a. (Bounded a, Enum a, Eq a, Show a) => Gen a -> Property
predSuccBoundedIdentity gen = property $ do
  x <- forAll $ Gen.filter (/= maxBound) gen
  let lhs = pred (succ x); rhs = x;
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Pred-Succ Identity"
        , lawContextLawBody = "pred . succ" `congruency` "id"
        , lawContextTcName = "Enum"
        , lawContextTcProp =
            let showX = show x
            in lawWhere
              [ "pred . succ $ x" `congruency` "id x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx
  
