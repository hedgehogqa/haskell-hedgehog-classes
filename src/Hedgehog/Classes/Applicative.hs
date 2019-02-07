{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Applicative (applicativeLaws) where

import Control.Applicative (Applicative(..))

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following laws:
--
-- [__Identity__]: @'pure' 'id' '<*>' v ≡ v@
-- [__Composition__]: @'pure' ('.') '<*>' u '<*>' v '<*>' w ≡ u '<*>' (v '<*>' w)@
-- [__Homomorphism__]: @'pure' f '<*>' 'pure' x ≡ 'pure' (f x)@
-- [__Interchange__]: @u '<*>' 'pure' y ≡ 'pure' ('$' y) '<*>' u@
-- [__LiftA2 1__]: @'liftA2' 'id' f x ≡ f '<*>' x@
-- [__LiftA2 2__]: @'liftA2' f x y ≡ f '<$>' x '<*>' y@
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
        { lawContextLawName = "Identity", lawContextLawBody = "pure id <*> v" `congruency` "v"
        , lawContextTcName = "Applicative", lawContextTcProp =
             let showA = show a
             in lawWhere
                 [ "pure id <*> v" `congruency` "v, where"
                 , "v = " ++ showA
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
            in lawWhere
                 [ "pure (.) <*> u <*> v <*> w", congruent, "u <*> (v <*> w), where"
                 , "u = " ++ showU
                 , "v = " ++ showV
                 , "w = " ++ showW
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
        { lawContextLawName = "Homomorphism", lawContextLawBody = "pure f <*> pure x" `congruency` "pure (f x)"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showF = show e; showX = show a;
            in lawWhere
              [ "pure f <*> pure x", congruent, "pure (f x), where"
              , "f = " ++ showF
              , "x = " ++ showX
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
        { lawContextLawName = "Interchange", lawContextLawBody = "u <*> pure y" `congruency` "pure ($ y) <*> u"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showU = show u'; showY = show y;
            in lawWhere
              [ "u <*> pure y", congruent, "pure ($ y) <*> u, where"
              , "u = " ++ showU
              , "y = " ++ showY
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
        { lawContextLawName = "LiftA2 1", lawContextLawBody = "liftA2 id f x" `congruency` "f <*> x"
        , lawContextTcName = "Applicative", lawContextTcProp =
            let showF = show f'; showX = show x;
            in lawWhere
              [ "liftA2 id f x", congruent, "f <*> x, where"
              , "f = " ++ showF
              , "x = " ++ showX
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
            in lawWhere
              [ "liftA2 f x y" `congruency` "f <$> x <*> y, where"
              , "f = " ++ showF
              , "x = " ++ showX
              , "y = " ++ showY
              ]
        , lawContextReduced = reduced lhs rhs
        }
  heqCtx1 lhs rhs ctx

