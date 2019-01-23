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
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "pure id <*> v" ++ congruent ++ "v"
        , lawContextTcName = "Applicative", lawContextTcProp =
             let showA = show a
             in concat
                 [ "pure id <*> v", congruent, "v, where"
                 , newline
                 , tab, "v = ", showA
                 ]
        , lawContextReduced = reduced lhs rhs
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
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showU = show u'; showV = show v'; showW = show w';
            in concat
                 [ "pure (.) <*> u <*> v <*> w", congruent, "u <*> (v <*> w), where"
                 , newline, newline
                 , tab, "u = ", showU
                 , newline
                 , tab, "v = ", showV
                 , newline
                 , tab, "w = ", showW
                 ]
        , lawContextReduced = reduced lhs rhs
        }

  heqCtx1 lhs rhs ctx

applicativeHomomorphism :: forall f. ApplicativeProp f
applicativeHomomorphism _ = property $ do
  e <- forAll genQuadraticEquation
  a <- forAll genSmallInteger
  let f = runQuadraticEquation e
  let lhs = pure f <*> pure a
  let rhs = pure (f a) :: f Integer
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Homomorphism", lawContextLawBody = "pure f <*> pure x" ++ congruent ++ "pure (f x)"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showF = show e; showX = show a;
            in concat
              [ "pure f <*> pure x", congruent, "pure (f x), where"
              , newline, newline
              , tab, "f = ", showF
              , newline
              , tab, "x = ", showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

applicativeInterchange :: forall f. ApplicativeProp f
applicativeInterchange fgen = property $ do
  u' <- forAll $ fgen genQuadraticEquation
  y <- forAll genSmallInteger
  let u = fmap runQuadraticEquation u'
  let lhs = (u <*> pure y)
  let rhs = pure ($ y) <*> u
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Interchange", lawContextLawBody = "u <*> pure y" ++ congruent ++ "pure ($ y) <*> u"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showU = show u'; showY = show y;
            in concat
              [ "u <*> pure y", congruent, "pure ($ y) <*> u, where"
              , newline, newline
              , tab, "u = ", showU
              , newline
              , tab, "y = ", showY
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

applicativeLiftA2_1 :: forall f. ApplicativeProp f
applicativeLiftA2_1 fgen = property $ do
  f' <- forAll $ fgen genQuadraticEquation
  x <- forAll $ fgen genSmallInteger
  let f = fmap runQuadraticEquation f'
  let lhs = liftA2 id f x
  let rhs = f <*> x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "LiftA2 1", lawContextLawBody = "liftA2 id f x == f <*> x"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showF = show f'; showX = show x;
            in concat
              [ "liftA2 id f x", congruent, "f <*> x, where"
              , newline, newline
              , tab, "f = ", showF
              , newline
              , tab, "x = ", showX
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

applicativeLiftA2_2 :: forall f. ApplicativeProp f
applicativeLiftA2_2 fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  y <- forAll $ fgen genSmallInteger
  f' <- forAll $ genLinearEquationTwo
  let f = runLinearEquationTwo f'
  let lhs = liftA2 f x y
  let rhs = f <$> x <*> y
  let ctx = contextualise $ LawContext
        { lawContextLawName = "LiftA2 2", lawContextLawBody = "liftA2 f x y == f <$> x <*> y"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showF = show f'; showX = show x; showY = show y;
            in concat
              [ "liftA2 f x y", congruent, "f <$> x <*> y, where"
              , newline, newline
              , tab, "f = ", showF
              , newline
              , tab, "x = ", showX
              , newline
              , tab, "y = ", showY
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

