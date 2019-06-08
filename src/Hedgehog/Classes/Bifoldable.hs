{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Bifoldable (bifoldableLaws, bifoldableFunctorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Monoid (Endo(..), Sum(..), Product(..))

-- | Tests the following 'Bifoldable' laws:
--
-- [__Identity__]: @'bifold'@ ≡ @'bifoldMap' 'id' 'id'@
-- [__FoldMap__]: @'bifoldMap' f g@ ≡ @'bifoldr' ('mappend' '.' f) ('mappend' '.' g) 'mempty'@
-- [__Foldr__]: @'bifoldr' f g z t@ ≡ @'appEndo' ('bifoldMap' ('Endo' '.' f) ('Endo' '.' g) t) z@
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

-- | Tests the following 'Bifoldable' / 'Bifunctor' laws:
--
-- [__Composition__]: @'bifoldMap' f g@ ≡ @'bifold' '.' 'bimap' f g@
-- [__FoldMap__]: @'bifoldMap' f g '.' 'bimap' h i@ ≡ @'bifoldMap' (f '.' h) (g '.' i)@
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
        { lawContextLawName = "Identity", lawContextLawBody = "bifold" `congruency` "bifoldMap id id"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
             let showX = show x;
             in lawWhere
                 [ "bimap id id x" `congruency` "x, where"
                 , "x = " ++ showX
                 ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

bifoldableFoldMap :: forall f. BifoldableProp f
bifoldableFoldMap fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  f' <- forAll genQuadraticEquation
  g' <- forAll genQuadraticEquation
  let f = Sum . runQuadraticEquation f'
  let g = Sum . runQuadraticEquation g'
  let lhs = (bifoldMap f g x)
  let rhs = (bifoldr (mappend . f) (mappend . g) mempty x)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "FoldMap", lawContextLawBody = "bifoldMap f g" `congruency` "bifoldr (mappend . f) (mappend . g) mempty"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
             let showX = show x;
                 showF = show f';
                 showG = show g';
             in lawWhere
                 [ "bifoldMap f g x" `congruency` "bifoldr (mappend . f) (mappend . g) mempty x, where"
                 , "f = " ++ showF
                 , "g = " ++ showG
                 , "x = " ++ showX
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
        { lawContextLawName = "Foldr", lawContextLawBody = "bifoldr f g z t" `congruency` "appEndo (bifoldMap (Endo . f) (Endo . g) t) z"
        , lawContextTcName = "Bifoldable", lawContextTcProp =
            let showX = show x; showF = show f'; showG = show g'; showZ = show z0;
            in lawWhere
              [ "bifoldr f g z t" `congruency` "appEndo (bifoldMap (Endo . f) (Endo . g) t z, where"
              , "f = " ++ showF
              , "g = " ++ showG
              , "t = " ++ showX
              , "z = " ++ showZ
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
        { lawContextLawName = "Composition", lawContextLawBody = "bifoldMap f g" `congruency` "bifold . bimap f g"
        , lawContextTcName = "Bifoldable/Bifunctor", lawContextTcProp =
            let showX = show x;
            in lawWhere
              [ "bifoldMap f g x" `congruency` "bifold . bimap f g $ x"
              , "f = \\x -> Product x"
              , "g = \\x -> Product (x + 1)"
              , "x = " ++ showX
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
        { lawContextLawName = "FoldMap", lawContextLawBody = "bifoldMap f g . bimap h i" `congruency` "bifoldMap (f . h) (g . i)"
        , lawContextTcName = "Bifoldable/Bifunctor", lawContextTcProp =
            let showX = show x;
            in lawWhere
              [ "bifoldMap f g . bimap h i $ x" `congruency` "bifoldMap (f . h) (g . i) $ x, where"
              , "f = " ++ showF
              , "g = " ++ showG
              , "h = " ++ showH
              , "i = " ++ showI
              , "x = " ++ showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

