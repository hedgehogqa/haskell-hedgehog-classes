{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Eq (eqLaws) where

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following 'Eq' laws:
--
-- [__Reflexivity__]: @x '==' x@ ≡ @'True'@
-- [__Symmetry__]: @x '==' y@ ≡ @y '==' x@
-- [__Transitivity__]: @x '==' y '&&' y '==' z@ ≡ @x '==' z@
-- [__Negation__]: @x '/=' y@ ≡ @'not' (x '==' y)@
eqLaws :: (Eq a, Show a) => Gen a -> Laws
eqLaws gen = Laws "Eq"
  [ ("Transitive", eqTransitive gen)
  , ("Symmetric", eqSymmetric gen)  
  , ("Reflexive", eqReflexive gen) 
  , ("Negation", eqNegation gen) 
  ]

eqTransitive :: forall a. (Eq a, Show a) => Gen a -> Property
eqTransitive gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = a == b && b == c; rhs = a == c
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Transitivity", lawContextLawBody = "a == b ∧ b == c" `congruency` "a == c"
        , lawContextTcName = "Eq", lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in lawWhere
              [ "a == b ∧ b == c" `congruency` "a == c, where"
              , "a = " ++ showA
              , "b = " ++ showB
              , "c = " ++ showC
              ]
        , lawContextReduced = reduced lhs rhs
        }
  case a == b of
    True -> case b == c of { True -> heqCtx a c ctx; False -> hneqCtx a c ctx }
    False -> case b == c of { True -> hneqCtx a c ctx; False -> success }

eqSymmetric :: forall a. (Eq a, Show a) => Gen a -> Property
eqSymmetric gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let lhs = a == b; rhs = b == a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Symmetry", lawContextLawBody = "a == b" `congruency` "b == a"
        , lawContextTcName = "Eq", lawContextTcProp =
            let showA = show a; showB = show b;
            in lawWhere
              [ "a == b" `congruency` "b == a, where"
              , "a = " ++ showA
              , "b = " ++ showB
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

eqReflexive :: forall a. (Eq a, Show a) => Gen a -> Property
eqReflexive gen = property $ do
  a <- forAll gen
  let lhs = a
  let rhs = a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Reflexivity", lawContextLawBody = "a" `congruency` "a"
        , lawContextTcName = "Eq"
        , lawContextTcProp = let showA = show a in lawWhere [ "a" `congruency` "a, where", "a = " ++ showA ]
        , lawContextReduced = reduced a a
        }
  heqCtx lhs rhs ctx

eqNegation :: forall a. (Eq a, Show a) => Gen a -> Property
eqNegation gen = property $ do
  x <- forAll gen
  y <- forAll gen
  let lhs = x /= y
  let rhs = not (x == y)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Negation", lawContextLawBody = "x /= y" `congruency` "not (x == y)"
        , lawContextTcName = "Eq"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x; showY = show y;
            in lawWhere
              [ "x /= y" `congruency` "not (x == y), where"
              , "x = " ++ showX
              , "y = " ++ showY
              ]
        }
  heqCtx lhs rhs ctx
