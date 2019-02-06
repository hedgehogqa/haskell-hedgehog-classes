{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Eq (eqLaws) where

import Hedgehog
import Hedgehog.Classes.Common

eqLaws :: (Eq a, Show a) => Gen a -> Laws
eqLaws gen = Laws "Eq"
  [ ("Transitive", eqTransitive gen)
  , ("Symmetric", eqSymmetric gen)  
  , ("Reflexive", eqReflexive gen) 
  ]

eqTransitive :: forall a. (Eq a, Show a) => Gen a -> Property
eqTransitive gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = a == b && b == c; rhs = a == c
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Transitivity", lawContextLawBody = "a == b ∧ b == c ⇒ a == c"
        , lawContextTcName = "Eq", lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in lawWhere
              [ "a == b ∧ b == c ⇒ a == c, where"
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
        { lawContextLawName = "Symmetry", lawContextLawBody = "a == b ⇒ b == a"
        , lawContextTcName = "Eq", lawContextTcProp =
            let showA = show a; showB = show b;
            in lawWhere
              [ "a == b ⇒  b == a, where"
              , "a = " ++ showA
              , "b = " ++ showB
              ]
        , lawContextReduced = reduced lhs rhs
        }
  case a == b of
    True -> heqCtx b a ctx
    False -> hneqCtx b a ctx

eqReflexive :: forall a. (Eq a, Show a) => Gen a -> Property
eqReflexive gen = property $ do
  a <- forAll gen
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Reflexivity", lawContextLawBody = "a == a"
        , lawContextTcName = "Eq"
        , lawContextTcProp = let showA = show a in lawWhere [ "a" ++ congruent ++ "a, where", "a = " ++ showA ]
        , lawContextReduced = reduced a a
        }
  heqCtx a a ctx
