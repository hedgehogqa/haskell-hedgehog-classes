{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Contravariant (contravariantLaws) where

import Data.Functor.Contravariant (Contravariant(..))

import Hedgehog
import Hedgehog.Classes.Common

contravariantLaws ::
  ( Contravariant f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
contravariantLaws gen = Laws "Contravariant"
  [ ("Identity", contravariantIdentity gen)
  , ("Composition", contravariantComposition gen)
  ]

contravariantIdentity ::
  ( Contravariant f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
contravariantIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = contramap id a
  let rhs = id a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "contramap id == id"
        , lawContextTcName = "Contravariant", lawContextTcProp =
            let showA = show a
            in concat
              [ "contramap id x", congruent, "id x, where"
              , newline, tab, "x = ", showA
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

contravariantComposition ::
  ( Contravariant f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
contravariantComposition fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  f' <- forAll genQuadraticEquation
  g' <- forAll genQuadraticEquation
  let f = runQuadraticEquation f'
  let g = runQuadraticEquation g'
  let lhs = contramap f (contramap g a)
  let rhs = contramap (g . f) a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "contramap f . contramap g" ++ congruent ++ "contramap (g . f)"
        , lawContextTcName = "Contravariant", lawContextTcProp =
            let showF = show f'; showG = show g'; showA = show a;
            in concat
                 [ "contramap f . contramap g $ a", congruent, "contramap (g . f) a, where"
                 , newline, newline
                 , tab, "f = ", showF, newline
                 , tab, "g = ", showG, newline
                 , tab, "a = ", showA
                 ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

