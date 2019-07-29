{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.MonadPlus (monadPlusLaws) where

import Control.Monad (MonadPlus(..))

import Hedgehog
import Hedgehog.Classes.Common

-- | Tests the following 'MonadPlus' laws:
--
-- [__Left Identity__]: @'mplus' 'mzero'@ ≡ @'id'@
-- [__Right Identity__]: @'flip' 'mplus' 'mzero'@ ≡ @'id'@
-- [__Associativity__]: @'mplus' a ('mplus' b c)@ ≡ @'mplus' ('mplus' a b) c@
-- [__Left Zero__]: @'mzero' '>>=' f@ ≡ @'mzero'@
-- [__Right Zero__]: @v '>>' 'mzero'@ ≡ @'mzero'@
monadPlusLaws ::
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadPlusLaws gen = Laws "MonadPlus"
  [ ("Left Identity", monadPlusLeftIdentity gen)
  , ("Right Identity", monadPlusRightIdentity gen)
  , ("Associativity", monadPlusAssociativity gen)
  , ("Left Zero", monadPlusLeftZero gen)
  , ("Right Zero", monadPlusRightZero gen)
  ]

type MonadPlusProp f =
  ( MonadPlus f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

monadPlusLeftIdentity :: forall f. MonadPlusProp f
monadPlusLeftIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  let lhs = mplus mzero x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Left Identity", lawContextTcName = "MonadPlus"
        , lawContextLawBody = "mplus mzero" `congruency` "id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp  =
            let showX = show x; showMZero = show (mzero :: f Integer);
            in lawWhere
              [ "mplus mzero x" `congruency` "id x, where"
              , "x = " ++ showX
              , "mzero = " ++ showMZero
              ]
        }
  heqCtx1 lhs rhs ctx

monadPlusRightIdentity :: forall f. MonadPlusProp f
monadPlusRightIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  let lhs = mplus x mzero
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Right Identity", lawContextTcName = "MonadPlus"
        , lawContextLawBody = "flip mplus mzero" `congruency` "id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp  =
            let showX = show x; showMZero = show (mzero :: f Integer);
            in lawWhere
              [ "mplus x mzero" `congruency` "id x, where"
              , "x = " ++ showX
              , "mzero = " ++ showMZero
              ]
        }
  heqCtx1 lhs rhs ctx

monadPlusAssociativity :: forall f. MonadPlusProp f
monadPlusAssociativity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  b <- forAll $ fgen genSmallInteger
  c <- forAll $ fgen genSmallInteger
  let lhs = mplus a (mplus b c)
  let rhs = mplus (mplus a b) c
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Associativity", lawContextTcName = "MonadPlus"
        , lawContextLawBody = "mplus a (mplus b c)" `congruency` "mplus (mplus a b) c"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in lawWhere
              [ "mplus a (mplus b c)" `congruency` "mplus (mplus a b) c, where"
              , "a = " ++ showA
              , "b = " ++ showB
              , "c = " ++ showC
              ]
        }
  heqCtx1 lhs rhs ctx

monadPlusLeftZero :: forall f. MonadPlusProp f
monadPlusLeftZero _ = property $ do
  k' :: LinearEquationM f <- forAll genLinearEquationM
  let lhs = mzero >>= runLinearEquationM k'
  let rhs = mzero
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Left Zero", lawContextTcName = "MonadPlus"
        , lawContextLawBody = "mzero >>= f" `congruency` "mzero"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp  =
            let showF = show k'; showMZero = show (mzero :: f Integer);
            in lawWhere
              [ "mzero >>= f" `congruency` "mzero, where"
              , "f = " ++ showF
              , "mzero = " ++ showMZero
              ]
        }
  heqCtx1 lhs rhs ctx

monadPlusRightZero :: forall f. MonadPlusProp f
monadPlusRightZero fgen = property $ do
  v <- forAll $ fgen genSmallInteger
  let lhs = v >> (mzero :: f Integer)
  let rhs = mzero
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Right Zero", lawContextTcName = "MonadPlus"
        , lawContextLawBody = "v >> mzero" `congruency` "mzero"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp  =
            let showV = show v; showMZero = show (mzero :: f Integer);
            in lawWhere
              [ "v >> mzero" `congruency` "mzero, where"
              , "v = " ++ showV
              , "mzero = " ++ showMZero
              ]
        }
  heqCtx1 lhs rhs ctx


