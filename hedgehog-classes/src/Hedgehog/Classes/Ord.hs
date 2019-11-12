{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Ord (ordLaws) where

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following 'Ord' laws:
--
-- [__Antisymmetry__]: @x '<=' y '&&' y '<=' x@ ≡ @x '==' y@
-- [__Transitivity__]: @x '<=' y '&&' y '<=' z@ ≡ @x '<=' z@
-- [__Reflexivity__]: @x '<=' x@ ≡ @'True'@
-- [__Totality__]: @x '<=' y '||' y '<=' x@ ≡ @'True'@
ordLaws :: forall a. (Ord a, Show a) => Gen a -> Laws
ordLaws gen = Laws "Ord"
  [ ("Antisymmetry", ordAntisymmetric gen)
  , ("Transitivity", ordTransitive gen)
  , ("Reflexivity", ordReflexive gen)
  , ("Totality", ordTotal gen)
  ]

ordAntisymmetric :: forall a. (Ord a, Show a) => Gen a -> Property
ordAntisymmetric gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let lhs = (a <= b) && (b <= a)
  let rhs = a == b
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Antisymmetry", lawContextTcName = "Ord"
        , lawContextLawBody = "x <= y && y <= x" `congruency` "x == y"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b;
            in lawWhere
              [ "x <= y && y <= x" `congruency` "x == y, where"
              , "x = " ++ showA
              , "y = " ++ showB
              ]
        }
  heqCtx lhs rhs ctx

ordTransitive :: forall a. (Ord a, Show a) => Gen a -> Property
ordTransitive gen = property $ do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen
  let lhs = x <= y && y <= z
  let rhs = x <= z
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Transitivity", lawContextTcName = "Ord"
        , lawContextLawBody = "x <= y && y <= z" `implies` "x <= z"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showY = show y; showZ = show z;
            in lawWhere
              [ "x <= y && y <= z" `implies` "x <= z, where"
              , "x = " ++ showX
              , "y = " ++ showY
              , "z = " ++ showZ
              ]
        }
  case (compare x y, compare y z) of
    (LT,LT) -> hLessThanCtx x z ctx
    (LT,EQ) -> hLessThanCtx x z ctx
    (LT,GT) -> success
    (EQ,LT) -> hLessThanCtx x z ctx
    (EQ,EQ) -> heqCtx x z ctx
    (EQ,GT) -> hGreaterThanCtx x z ctx
    (GT,LT) -> success
    (GT,EQ) -> hGreaterThanCtx x z ctx
    (GT,GT) -> hGreaterThanCtx x z ctx

ordTotal :: forall a. (Ord a, Show a) => Gen a -> Property
ordTotal gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let lhs = (a <= b) || (b <= a)
  let rhs = True
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Totality", lawContextTcName = "Ord"
        , lawContextLawBody = "x <= y || y <= x" `congruency` "True"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b;
            in lawWhere
              [ "(x <= y) || (y <= x)" `congruency` "True, where"
              , "x = " ++ showA
              , "y = " ++ showB
              ]
        }
  heqCtx lhs rhs ctx

ordReflexive :: forall a. (Ord a, Show a) => Gen a -> Property
ordReflexive gen = property $ do
  x <- forAll gen
  let lhs = x <= x
  let rhs = True
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Reflexivity", lawContextTcName = "Ord"
        , lawContextLawBody = "x <= x" `congruency` "True"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x;
            in lawWhere
              [ "x <= x" `congruency` "True, where"
              , "x = " ++ showX
              ]
        }
  heqCtx lhs rhs ctx
