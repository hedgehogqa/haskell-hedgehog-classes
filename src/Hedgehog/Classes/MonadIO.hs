{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Hedgehog.Classes.MonadIO (monadIOLaws) where

import Control.Monad.IO.Class (MonadIO(..))

import Hedgehog
import Hedgehog.Classes.Common

monadIOLaws ::
  ( MonadIO f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
monadIOLaws gen = Laws "MonadIO"
  [ ("Return", monadIOReturn gen)
  , ("Lift", monadIOLift gen)
  ]

type MonadIOProp f =
  ( MonadIO f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

monadIOReturn :: forall f. MonadIOProp f
monadIOReturn _fgen = property $ do
  x <- forAll genSmallInteger
  let lhs = liftIO (return x)
  let rhs = return x :: f Integer
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Return", lawContextTcName = "MonadIO"
        , lawContextLawBody = "liftIO . return" `congruency` "return"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showX = show x
            in lawWhere
              [ "liftIO . return $ x" `congruency` "return x, where"
              , "x = " ++ showX
              ]
        }
  heqCtx1 lhs rhs ctx

monadIOLift :: forall f. MonadIOProp f
monadIOLift _fgen = property $ do
  m <- forAllWith showIO $ genIO genSmallInteger
  f' <- forAll genLinearEquation
  let f = pure . runLinearEquation f'
  let lhs = liftIO (m >>= f) :: f Integer
  let rhs = liftIO m >>= (liftIO . f) :: f Integer
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Lift", lawContextTcName = "MonadIO"
        , lawContextLawBody = "liftIO (m >>= f)" `congruency` "liftIO m >>= (liftIO . f)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showM = showIO m
                showF = show f'
            in lawWhere
              [ "liftIO (m >>= f)" `congruency` "liftIO m >>= (liftIO . f), where"
              , "f = " ++ showF
              , "m = " ++ showM
              ]
        }
  heqCtx1 lhs rhs ctx
