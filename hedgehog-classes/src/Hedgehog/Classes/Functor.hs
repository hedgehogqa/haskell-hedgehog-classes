{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Functor (functorLaws) where

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following 'Functor' laws:
--
-- [__Identity__]: @'fmap' 'id'@ ≡ @'id'@
-- [__Composition__]: @'fmap' f '.' 'fmap' g@ ≡ @'fmap' (f '.' g)@
-- [__Const__]: @'fmap' ('const' x)@ ≡ @x '<$'@
functorLaws ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
functorLaws gen = Laws "Functor"
  [ ("Identity", functorIdentity gen)
  , ("Composition", functorComposition gen)
  , ("Const", functorConst gen)
  ]

functorIdentity ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
functorIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = fmap id a
  let rhs = id a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Identity", lawContextTcName = "Functor"
        , lawContextLawBody = "fmap id" `congruency` "id"
        , lawContextTcProp =
            let showA = show a
            in lawWhere
              [ "fmap id a" `congruency` "id a, where"
              , "a = " ++ showA
              ]
        , lawContextReduced = reduced lhs rhs
        } 
  heqCtx lhs rhs ctx

functorComposition ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
functorComposition fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let f = func2; g = func1 
  let lhs = fmap f (fmap g a)
  let rhs = fmap (f . g) a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Composition", lawContextTcName = "Functor"
        , lawContextLawBody = "fmap f . fmap g" `congruency` "fmap (f . g)"
        , lawContextTcProp =
            let showA = show a
                showF = "\\(a,b) -> (odd a, if even a then Left (compare a b) else Right (b + 2)"
                showG = "\\i -> (div (i + 5) 3, i * i - 2 * i + 1)"
            in lawWhere
              [ "fmap f . fmap g $ a" `congruency` "fmap (f . g) a, where"
              , "f = " ++ showF
              , "g = " ++ showG
              , "a = " ++ showA
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx

functorConst ::
  ( Functor f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
functorConst fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let x = 'X'
  let lhs = fmap (const x) a
  let rhs = x <$ a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Const", lawContextTcName = "Functor"
        , lawContextLawBody = "fmap (const x)" `congruency` "x <$"
        , lawContextTcProp =
            let showA = show a
                showX = show x
            in lawWhere
              [ "fmap (const x) a" `congruency` "x <$ a, where"
              , "x = " ++ showX
              , "a = " ++ showA
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx lhs rhs ctx
