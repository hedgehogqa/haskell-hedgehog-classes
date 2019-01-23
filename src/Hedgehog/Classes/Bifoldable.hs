{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Bifoldable (bifoldableLaws, bifoldableFunctorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Monoid (Endo(..), Sum(..), Product(..))

bifoldableLaws :: forall f.
  ( Bifoldable f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bifoldableLaws gen = Laws "Bifoldable"
  [ ("Identity", bifoldableIdentity gen)
  , ("FoldMap", bifoldableFoldMap gen)
  , ("Foldr", bifoldableFoldr gen)
  ]

bifoldableFunctorLaws :: forall f.
  ( Bifoldable f, Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bifoldableFunctorLaws gen = Laws "Bifoldable/Bifunctor"
  [ ("Composition", bifoldableFunctorComposition gen)
  , ("FoldMap", bifoldableFunctorFoldMap gen)
  ]

type BifoldableProp f =
  ( Bifoldable f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

bifoldableIdentity :: forall f. BifoldableProp f
bifoldableIdentity fgen = property $ do
  x <- forAll $ fgen genSmallSum genSmallSum
  let lhs = bifold x
  let rhs = bifoldMap id id x
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "forall x. bifold x == bifoldMap id id x"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx lhs rhs ctx
        
bifoldableFoldMap :: forall f. BifoldableProp f
bifoldableFoldMap fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = (bifoldMap Sum Sum x)
  let rhs = (bifoldr (mappend . Sum) (mappend . Sum) mempty x)
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "FoldMap", lawContextLawBody = "forall f g x. bifoldMap f g x == bifoldr (mappend . f) (mappend . g) mempty x"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx lhs rhs ctx

bifoldableFoldr :: forall f. BifoldableProp f
bifoldableFoldr fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  f' <- forAll genLinearEquationTwo
  g' <- forAll genLinearEquationTwo
  let f = runLinearEquationTwo f'
  let g = runLinearEquationTwo g'
  let z0 = 0
  let lhs = (bifoldr f g z0 x)
  let rhs = (appEndo (bifoldMap (Endo . f) (Endo . g) x) z0)
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Foldr", lawContextLawBody = "forall f g z t. bifoldr f g z t == appEndo (bifoldMap (Endo . f) (Endo . g) t) z"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx lhs rhs ctx 

type BifoldableFunctorProp f =
  ( Bifoldable f, Bifunctor f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

bifoldableFunctorComposition :: forall f. BifoldableFunctorProp f
bifoldableFunctorComposition fgen = property $ do
  x <- forAll $ fgen genSmallSum genSmallSum
  let lhs = bifoldMap Product Product x
  let rhs = bifold (bimap Product Product x)
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "forall f g. bifoldMap f g == bifold . bimap f g"
        , lawContextTcName = "Bifoldable/Bifunctor", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx lhs rhs ctx 

bifoldableFunctorFoldMap :: forall f. BifoldableFunctorProp f
bifoldableFunctorFoldMap fgen = property $ do
  x <- forAll $ fgen genSmallSum genSmallSum
  let h (Sum s) = s * s
  let i (Sum s) = s + s
  let lhs = bifoldMap Sum Sum (bimap h i x)
  let rhs = bifoldMap (Sum . h) (Sum . i) x
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "forall f g. bifoldMap f g . bimap hi == bifoldMap (f . h) (g . i)"
        , lawContextTcName = "Bifoldable/Bifunctor", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx lhs rhs ctx

