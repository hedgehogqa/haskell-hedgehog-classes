{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Bifunctor (bifunctorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Bifunctor (Bifunctor(..))

-- | Tests the following 'Bifunctor' laws:
--
-- [__Identity__]: @'bimap' 'id' 'id'@ ≡ @'id'@
-- [__First Identity__]: @'first' 'id'@ ≡ @'id'@
-- [__Second Identity__]: @'second' 'id'@ ≡ @'id'@
-- [__Composition__]: @'bimap' 'id' 'id'@ ≡ @'first' 'id' '.' 'second' 'id'@
bifunctorLaws :: forall f.
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bifunctorLaws gen = Laws "Bifunctor"
  [ ("Identity", bifunctorIdentity gen)
  , ("First Identity", bifunctorFirstIdentity gen)
  , ("Second Identity", bifunctorSecondIdentity gen)
  , ("Composition", bifunctorComposition gen) 
  ]

type BifunctorProp f =
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

bifunctorIdentity :: forall f. BifunctorProp f
bifunctorIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = bimap id id x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "bimap id id" `congruency` "id"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
             let showX = show x;
             in lawWhere
                 [ "bimap id id x" `congruency` "x, where"
                 , "x = " ++ showX
                 ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx2 lhs rhs ctx 

bifunctorFirstIdentity :: forall f. BifunctorProp f
bifunctorFirstIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = first id x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "First Identity", lawContextLawBody = "first id" `congruency` "id"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
            let showX = show x;
            in lawWhere
              [ "first id x" `congruency` "x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx2 lhs rhs ctx

bifunctorSecondIdentity :: forall f. BifunctorProp f
bifunctorSecondIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = second id x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Second Identity", lawContextLawBody = "second id" `congruency` "id"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
            let showX = show x;
            in lawWhere
              [ "second id x" `congruency` "x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx2 lhs rhs ctx

bifunctorComposition :: forall f. BifunctorProp f
bifunctorComposition fgen = property $ do
  z <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = bimap id id z
  let rhs = (first id . second id) z
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "bimap id id" `congruency` "first id . second id"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
            let showX = show z;
            in lawWhere
              [ "bimap id id x" `congruency` "first id . second id $ x, where"
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs 
        }       
  heqCtx2 lhs rhs ctx
