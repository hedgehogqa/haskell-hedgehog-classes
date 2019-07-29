{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.Monad (monadLaws) where

import Control.Monad (ap)

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following 'Monad' laws:
--
-- [__Left Identity__]: @'return' a '>>=' k@ ≡ @k a@
-- [__Right Identity__]: @m '>>=' 'return'@ ≡ @m@
-- [__Associativity__]: @m '>>=' (\\x -> k x '>>=' h)@ ≡ @(m '>>=' k) '>>=' h@
-- [__Return__]: @'return'@ ≡ @'pure'@
-- [__Ap__]: @'ap' f x@ ≡ @f '<*>' x@
monadLaws ::
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadLaws gen = Laws "Monad"
  [ ("Left Identity", monadLeftIdentity gen)
  , ("Right Identity", monadRightIdentity gen)
  , ("Associativity", monadAssociativity gen)
  , ("Return", monadReturn gen)
  , ("Ap", monadAp gen)
  ]

type MonadProp f =
  ( Monad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

monadLeftIdentity :: forall f. MonadProp f
monadLeftIdentity _ = property $ do
  k' :: LinearEquationM f <- forAll genLinearEquationM
  a <- forAll $ genSmallInteger
  let k = runLinearEquationM k'

  let lhs = return a >>= k
  let rhs = k a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Left Identity", lawContextTcName = "Monad"
        , lawContextLawBody = "return a >>= k" `congruency` "k a"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showK = show k'
                showA = show a
            in lawWhere
              [ "return a >>= k" `congruency` "k a, where"
              , "k = " ++ showK
              , "a = " ++ showA
              ]
        }
  heqCtx1 lhs rhs ctx

monadRightIdentity :: forall f. MonadProp f
monadRightIdentity fgen = property $ do
  m <- forAll $ fgen genSmallInteger
  let lhs = m >>= return
  let rhs = m
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Right Identity", lawContextTcName = "Monad"
        , lawContextLawBody = "m >>= return" `congruency` "m"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showM = show m
            in lawWhere
              [ "m >>= return" `congruency` "m, where"
              , "m = " ++ showM
              ]
        }
  heqCtx1 lhs rhs ctx

monadAssociativity :: forall f. MonadProp f
monadAssociativity fgen = property $ do
  m <- forAll $ fgen genSmallInteger
  k' :: LinearEquationM f <- forAll genLinearEquationM
  h' :: LinearEquationM f <- forAll genLinearEquationM
  let k = runLinearEquationM k'
      h = runLinearEquationM h'
  let lhs = m >>= (\x -> k x >>= h)
  let rhs = (m >>= k) >>= h
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Associativity", lawContextTcName = "Monad"
        , lawContextLawBody = "m >>= (\\x -> k x >>= h)" `congruency` "(m >>= k) >>= h"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showM = show m
                showK = show k'
                showH = show h'
            in lawWhere
              [ "m >>= (\\x -> k x >>= h)" `congruency` "(m >>= k) >>= h, where"
              , "m = " ++ showM
              , "k = " ++ showK
              , "h = " ++ showH
              ]
        }
  heqCtx1 lhs rhs ctx

monadReturn :: forall f. MonadProp f
monadReturn _ = property $ do
  x <- forAll genSmallInteger
  let lhs = return x
  let rhs = pure x :: f Integer
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Return", lawContextTcName = "Monad"
        , lawContextLawBody = "return" `congruency` "pure"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x
            in lawWhere
              [ "return x" `congruency` "pure x, where"
              , "x = " ++ showX
              ]
        }
  heqCtx1 lhs rhs ctx

monadAp :: forall f. MonadProp f
monadAp _ = property $ do
  f' :: f QuadraticEquation <- forAll $ pure <$> genQuadraticEquation
  x :: f Integer <- forAll $ pure <$> genSmallInteger
  let f = fmap runQuadraticEquation f'

  let lhs = ap f x
  let rhs = f <*> x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Ap", lawContextTcName = "Monad"
        , lawContextLawBody = "ap f" `congruency` "f <*>"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x
                showF = show f'
            in lawWhere
              [ "ap f x" `congruency` "f <*> x, where"
              , "f = " ++ showF
              , "x = " ++ showX
              ]
        }
  heqCtx1 lhs rhs ctx

