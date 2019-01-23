{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Applicative (applicativeLaws) where

import Control.Applicative (Applicative(..))

import Hedgehog
import Hedgehog.Classes.Common

applicativeLaws ::
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
applicativeLaws gen = Laws "Applicative"
  [ ("Identity", applicativeIdentity gen)
  , ("Composition", applicativeComposition gen)
  , ("Homomorphism", applicativeHomomorphism gen)
  , ("Interchange", applicativeInterchange gen)
  , ("LiftA2 Part 1", applicativeLiftA2_1 gen)
  , ("LiftA2 Part 2", applicativeLiftA2_2 gen) 
  ]

type ApplicativeProp f =
  ( Applicative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

applicativeIdentity :: forall f. ApplicativeProp f
applicativeIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = pure id <*> a
  let rhs = a
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "forall a. pure id <*> a = a"
        , lawContextTcName = "Applicative", lawContextTcProp =
             let showA = show a
             in "pure id <*> " ++ showA ++ " == " ++ showA
        }
  heqCtx1 lhs rhs ctx

applicativeComposition :: forall f. ApplicativeProp f
applicativeComposition fgen = property $ do
  u' <- forAll $ fgen genQuadraticEquation
  v' <- forAll $ fgen genQuadraticEquation
  w' <- forAll genSmallInteger
  let u = runQuadraticEquation <$> u'
      v = runQuadraticEquation <$> v'
      w = pure w'
  let lhs = pure (.) <*> u <*> v <*> w
  let rhs = u <*> (v <*> w)
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "forall a b c. pure (.) <*> a <*> b <*> c == a <*> (b <*> c)"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx1 lhs rhs ctx

applicativeHomomorphism :: forall f. ApplicativeProp f
applicativeHomomorphism _ = property $ do
  e <- forAll genQuadraticEquation
  a <- forAll genSmallInteger
  let f = runQuadraticEquation e
  let lhs = pure f <*> pure a
  let rhs = pure (f a) :: f Integer
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Homomorphism", lawContextLawBody = "forall f a. pure f <*> pure a == pure (f a)"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx1 lhs rhs ctx

applicativeInterchange :: forall f. ApplicativeProp f
applicativeInterchange fgen = property $ do
  u' <- forAll $ fgen genQuadraticEquation
  y <- forAll genSmallInteger
  let u = fmap runQuadraticEquation u'
  let lhs = (u <*> pure y)
  let rhs = pure ($ y) <*> u
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Interchange", lawContextLawBody = "forall a b. a <*> pure b == pure ($ b) <*> a"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx1 lhs rhs ctx

applicativeLiftA2_1 :: forall f. ApplicativeProp f
applicativeLiftA2_1 fgen = property $ do
  f' <- forAll $ fgen genQuadraticEquation
  x <- forAll $ fgen genSmallInteger
  let f = fmap runQuadraticEquation f'
  let lhs = liftA2 id f x
  let rhs = f <*> x
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "LiftA2 1", lawContextLawBody = "forall f x. liftA2 id f x == f <*> x"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx1 lhs rhs ctx

applicativeLiftA2_2 :: forall f. ApplicativeProp f
applicativeLiftA2_2 fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  y <- forAll $ fgen genSmallInteger
  let f a b = a * a - b
  let lhs = liftA2 f x y
  let rhs = f <$> x <*> y
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "LiftA2 2", lawContextLawBody = "forall f x y. liftA2 f x y == f <$> x <*> y"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let sLhs = show lhs; sRhs = show rhs;
            in sLhs ++ " == " ++ sRhs
        }
  heqCtx1 lhs rhs ctx

