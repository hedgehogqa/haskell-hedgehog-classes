{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.MonadZip (monadZipLaws) where

import Control.Arrow (Arrow(..))
import Control.Monad.Zip (MonadZip(mzip))

import Hedgehog
import Hedgehog.Classes.Common

monadZipLaws ::
  ( MonadZip f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadZipLaws gen = Laws "Monad"
  [ ("Naturality", monadZipNaturality gen)
  ]

type MonadZipProp f =
  ( MonadZip f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

monadZipNaturality :: forall f. MonadZipProp f
monadZipNaturality fgen = property $ do
  f' <- forAll genLinearEquation
  g' <- forAll genLinearEquation
  let f = runLinearEquation f'
      g = runLinearEquation g'
  ma <- forAll $ fgen genSmallInteger
  mb <- forAll $ fgen genSmallInteger
  let lhs = fmap (f *** g) (mzip ma mb)
  let rhs = mzip (fmap f ma) (fmap g mb)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Naturality", lawContextTcName = "MonadZip"
        , lawContextLawBody = "(fmap (f *** g) .) . mzip" `congruency` "(. fmap g) . mzip . fmap f"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showF = show f'; showG = show g'; showMA = show ma; showMB = show mb;
            in lawWhere
              [ "fmap (f *** g) (mzip ma mb)" `congruency` "mzip (fmap f ma) (fmap g mb), where"
              , "f = " ++ showF
              , "g = " ++ showG
              , "ma = " ++ showMA  
              , "mb = " ++ showMB
              ]
        }
  heqCtx1 lhs rhs ctx
