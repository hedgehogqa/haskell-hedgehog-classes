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
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Transitivity", lawContextLawBody = "forall a b c. a == b ∧ b == c ⇒ a == c"
        , lawContextTcName = "Eq", lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in showA ++ " == " ++ showB ++ " ∧ " ++ showB ++ " == " ++
               showC ++ " ⇒ " ++ showA ++ " == " ++ showC
        }
  case a == b of
    True -> case b == c of
      True -> heqCtx a c ctx
      False -> hneqCtx a c ctx
    False -> case b == c of
      True -> hneqCtx a c ctx
      False -> success

eqSymmetric :: forall a. (Eq a, Show a) => Gen a -> Property
eqSymmetric gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Symmetry", lawContextLawBody = "forall a b. a == b ⇒ b == a"
        , lawContextTcName = "Eq", lawContextTcProp =
            let showX = show a; showY = show b;
            in showX ++ " == " ++ showY
               ++ " ⇒ "
               ++ showY ++ " == " ++ showX
        }
  case a == b of
    True -> heqCtx b a ctx
    False -> hneqCtx b a ctx

eqReflexive :: forall a. (Eq a, Show a) => Gen a -> Property
eqReflexive gen = property $ do
  a <- forAll gen
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Reflexivity", lawContextLawBody = "forall a. a == a"
        , lawContextTcName = "Eq"
        , lawContextTcProp = let showA = show a in showA ++ " == " ++ showA
        }
  heqCtx a a ctx
