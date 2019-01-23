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
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "bifold" ++ congruent ++ "bifoldMap id id"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
             let showX = show x;
             in concat
                 [ "bimap id id x", congruent, "x, where"
                 , newline
                 , tab, "x = ", showX
                 ]
        , lawContextReduced = reduced lhs rhs 
        } 
  heqCtx lhs rhs ctx
        
bifoldableFoldMap :: forall f. BifoldableProp f
bifoldableFoldMap fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let f = Sum; g = Sum . (+1)
  let lhs = (bifoldMap f g x)
  let rhs = (bifoldr (mappend . f) (mappend . g) mempty x)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "FoldMap", lawContextLawBody = "bifoldMap f g == bifoldr (mappend . f) (mappend . g) mempty"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
             let showX = show x;
             in concat
                 [ "bifoldMap f g x", congruent, "bifoldr (mappend . f) (mappend . g) mempty x, where"
                 , newline
                 , tab, "f = \\x -> Sum x"
                 , tab, "g = \\x -> Sum (x + 1)"
                 , tab, "x = ", showX
                 ]
        , lawContextReduced = reduced lhs rhs 
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
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Foldr", lawContextLawBody = "bifoldr f g z t" ++ congruent ++ "appEndo (bifoldMap (Endo . f) (Endo . g) t) z"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
            let showX = show x; showF = show f'; showG = show g'; showZ = show z0;
            in concat
              [ "bifoldr f g z t", congruent, "appEndo (bifoldMap (Endo . f) (Endo . g) t z, where"
              , newline
              , tab, "f = ", showF, newline
              , tab, "g = ", showG, newline
              , tab, "t = ", showX, newline
              , tab, "z = ", showZ
              ]
        , lawContextReduced = reduced lhs rhs 
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
  let f = Product; g = Product . (+1)
  let lhs = bifoldMap f g x
  let rhs = bifold (bimap f g x)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "bifoldMap f g == bifold . bimap f g"
        , lawContextTcName = "Bifoldable/Bifunctor", lawContextTcProp =
            let showX = show x;
            in concat
              [ "bifoldMap f g x", congruent, "bifold . bimap f g $ x"
              , newline
              , tab, "f = \\x -> Product x", newline
              , tab, "g = \\x -> Product (x + 1)", newline
              , tab, "x = ", showX
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx 

bifoldableFunctorFoldMap :: forall f. BifoldableFunctorProp f
bifoldableFunctorFoldMap fgen = property $ do
  x <- forAll $ fgen genSmallSum genSmallSum
  let h (Sum s) = s * s + 3; showH = "\\(Sum s) -> s * s + 3"
  let i (Sum s) = s + s - 7; showI = "\\(Sum s) -> s + s - 7"
  let f = Sum; showF = "\\x -> Sum x"; g = Sum . (+1); showG = "\\x -> Sum (x + 1)"
  let lhs = bifoldMap f g (bimap h i x)
  let rhs = bifoldMap (f . h) (g . i) x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "bifoldMap f g . bimap h i" ++ congruent ++ "bifoldMap (f . h) (g . i)"
        , lawContextTcName = "Bifoldable/Bifunctor", lawContextTcProp =
            let showX = show x;
            in concat
              [ "bifoldMap f g . bimap h i $ x", congruent, "bifoldMap (f . h) (g . i) $ x, where"
              , newline
              , tab, "f = ", showF, newline
              , tab, "g = ", showG, newline
              , tab, "h = ", showH, newline
              , tab, "i = ", showI, newline
              , tab, "x = ", showX
              ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx lhs rhs ctx

