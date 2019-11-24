{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

#if !HAVE_PROFUNCTORS
module Hedgehog.Classes.Profunctor () where
#else

module Hedgehog.Classes.Profunctor (profunctorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Profunctor

type ProfunctorProp f =
  ( Profunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

dimapIdentity :: forall f. ProfunctorProp f
dimapIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = dimap id id x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Dimap Identity", lawContextLawBody = "dimap id id" `congruency` "id"
        , lawContextTcName = "Profunctor", lawContextTcProp =
            let showX = show x;
            in lawWhere
                [ "dimap id id x" `congruency` "x, where"
                , "x = " ++ showX
                ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx2 lhs rhs ctx

lmapIdentity :: forall f. ProfunctorProp f
lmapIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = lmap id x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Lmap Identity", lawContextLawBody = "lmap id" `congruency` "id"
        , lawContextTcName = "Profunctor", lawContextTcProp =
            let showX = show x;
            in lawWhere
                [ "lmap id x" `congruency` "x, where"
                , "x = " ++ showX
                ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx2 lhs rhs ctx

rmapIdentity :: forall f. ProfunctorProp f
rmapIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = rmap id x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Rmap Identity", lawContextLawBody = "rmap id" `congruency` "id"
        , lawContextTcName = "Profunctor", lawContextTcProp =
            let showX = show x;
            in lawWhere
                [ "rmap id x" `congruency` "x, where"
                , "x = " ++ showX
                ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx2 lhs rhs ctx

profunctorComposition :: forall f. ProfunctorProp f
profunctorComposition fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let f = \i -> (i * 3) + 7
  let g = \i -> (i * 11) + 13
  let lhs = dimap f g x
  let rhs = lmap f . rmap g $ x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Profunctor Composition", lawContextLawBody = "dimap f g" `congruency` "lmap f . lmap g"
        , lawContextTcName = "Profunctor", lawContextTcProp =
            let showX = show x;
                showF = "\\i -> (i * 3) + 7";
                showG = "\\i -> (i * 11) + 13";
            in lawWhere
                [ "dimap f g x" `congruency` "x, where"
                , "f = " ++ showF
                , "g = " ++ showG
                , "x = " ++ showX
                ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx2 lhs rhs ctx

-- | Tests the following 'Profunctor' laws:
--
-- [__Dimap Identity__]: @'dimap' 'id' 'id'@ ≡ @'id'@
-- [__Lmap Identity__]: @'lmap' 'id'@ ≡ @'id'@
-- [__Rmap Identity__]: @'rmap' 'id'@ ≡ @'id'@
-- [__Composition__]: @'dimap' f g@ ≡ @'lmap' f '.' 'rmap' g@
profunctorLaws :: forall f.
  ( Profunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
profunctorLaws gen = Laws "Profunctor"
  [ ("Dimap Identity", dimapIdentity gen)
  , ("Lmap Identity", lmapIdentity gen)
  , ("Rmap Identity", rmapIdentity gen)
  , ("Composition", profunctorComposition gen)
  ]
#endif
