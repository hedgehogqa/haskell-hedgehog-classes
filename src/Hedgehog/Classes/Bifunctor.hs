{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Bifunctor (bifunctorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Bifunctor (Bifunctor(..))

bifunctorLaws :: forall f.
  ( Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bifunctorLaws gen = Laws "Bifunctor"
  [ ("Identity", bifunctorIdentity gen)
  , ("First Identity", bifunctorFirstIdentity gen)
  , ("Second Identity", bifunctorSecondIdentity gen)
  , ("Bifunctor Composition", bifunctorComposition gen) 
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
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "forall x. bimap id id x == x"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx2 lhs rhs ctx 

bifunctorFirstIdentity :: forall f. BifunctorProp f
bifunctorFirstIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = first id x
  let rhs = x
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "First Identity", lawContextLawBody = "forall x. first id x == x"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx2 lhs rhs ctx

bifunctorSecondIdentity :: forall f. BifunctorProp f
bifunctorSecondIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = second id x
  let rhs = x
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Second Identity", lawContextLawBody = "forall x. second id x == x"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx2 lhs rhs ctx

bifunctorComposition :: forall f. BifunctorProp f
bifunctorComposition fgen = property $ do
  z <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = bimap id id z
  let rhs = (first id . second id) z
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "forall x. bimap id id x == (first id . second id) x"
        , lawContextTcName = "Bifunctor", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx2 lhs rhs ctx
