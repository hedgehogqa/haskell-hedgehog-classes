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

transitiveLaw :: forall a. Show a => a -> a -> a -> LawContext a
transitiveLaw a b c = LawContext
  { lawContextLawName = "Transitivity"
  , lawContextLawBody = "forall a b c. a == b ∧ b == c ⇒ a == c" 
  , lawContextTcName  = "Eq"
  , lawContextTcMethod = \_ _ ->
      let showA = show a
          showB = show b
          showC = show c
      in showA ++ " == " ++ showB ++ " ∧ " ++ showB ++ " == " ++
         showC ++ " ⇒ " ++ showA ++ " == " ++ showC
  , lawContextLhs = a
  , lawContextRhs = a
  }

eqTransitive :: forall a. (Eq a, Show a) => Gen a -> Property
eqTransitive gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let ctx = showLawContext (transitiveLaw a b c)
  case a == b of
    True -> case b == c of
      True -> heqCtx a c ctx
      False -> hneqCtx a c ctx
    False -> case b == c of
      True -> hneqCtx a c ctx
      False -> success

symmetricLaw :: forall a. Show a => a -> a -> LawContext a
symmetricLaw lhs rhs = LawContext
  { lawContextLawName = "Symmetry"
  , lawContextLawBody = "forall a b. a == b ⇒ b == a"
  , lawContextTcName  = "Eq"
  , lawContextTcMethod = \x y -> show x ++ " == " ++ show y ++ " ⇒ " ++ show y ++ " == " ++ show x
  , lawContextLhs = lhs
  , lawContextRhs = rhs
--  , lawContextEquation = \_ _ -> mempty
--  , lawContextLhsReduced = Nothing
--  , lawContextRhsReduced = Nothing
  }

eqSymmetric :: forall a. (Eq a, Show a) => Gen a -> Property
eqSymmetric gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let ctx = showLawContext $ symmetricLaw a b 
  case a == b of
    True -> heqCtx b a ctx
    False -> hneqCtx b a ctx

reflexiveLaw :: Show a => a -> LawContext a
reflexiveLaw a = LawContext
  { lawContextLawName = "Reflexivity"
  , lawContextLawBody = "forall a. a == a"
  , lawContextTcName = "Eq"
  , lawContextTcMethod = \x y -> show x ++ " == " ++ show y
  , lawContextLhs = a
  , lawContextRhs = a
--  , lawContextEquation = \_ _ -> mempty
--  , lawContextLhsReduced = Nothing
--  , lawContextRhsReduced = Nothing
  }

eqReflexive :: forall a. (Eq a, Show a) => Gen a -> Property
eqReflexive gen = property $ do
  a <- forAll gen
  let lawCtx = reflexiveLaw a
  heqCtx a a (showLawContext lawCtx)

