{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Generic (genericLaws, generic1Laws) where

import Hedgehog
import Hedgehog.Classes.Common

import GHC.Generics (Generic(..), Generic1(..))
import qualified Hedgehog.Gen as Gen

genericLaws ::
  ( Generic a, Eq a, Show a
  , Eq (Rep a x), Show (Rep a x)
  )
  => Gen a
  -> Gen (Rep a x) 
  -> Laws
genericLaws gena genr = Laws "Generic"
  [ ("From-To inverse", fromToInverse gena genr)
  , ("To-From inverse", toFromInverse gena genr) 
  ]

generic1Laws ::
  ( Generic1 f
  , forall x. Eq x => Eq (f x)
  , forall x. Show x => Show (f x)
  , forall x. Eq x => Eq (Rep1 f x)
  , forall x. Show x => Show (Rep1 f x)
  ) => (forall x. Gen x -> Gen (f x))
    -> (forall x. Gen x -> Gen (Rep1 f x))
    -> Laws
generic1Laws genf genr = Laws "Generic1"
  [ ("From1-To1 inverse", fromToInverse1 genf genr)
  , ("To1-From1 inverse", toFromInverse1 genf genr)
  ]

fromToInverse :: forall a x.
  ( Generic a
  , Eq (Rep a x)
  , Show (Rep a x)
  ) => Gen a -> Gen (Rep a x) -> Property
fromToInverse _gena genr = property $ do
  r <- forAll genr
  r === (from (to r :: a))

toFromInverse :: forall a x.
  ( Generic a
  , Eq a
  , Show a
  ) => Gen a -> Gen (Rep a x) -> Property
toFromInverse gena _genr = property $ do
  v <- forAll gena
  to (from v) === v

type Generic1Prop f =
  ( Generic1 f
  , forall x. Eq x => Eq (f x)
  , forall x. Show x => Show (f x)
  , forall x. Eq x => Eq (Rep1 f x)
  , forall x. Show x => Show (Rep1 f x)
  ) => (forall x. Gen x -> Gen (f x))
    -> (forall x. Gen x -> Gen (Rep1 f x))
    -> Property

fromToInverse1 :: forall f. Generic1Prop f
fromToInverse1 _genf genr = property $ do
  r <- forAll $ genr genSmallInteger
  r === (from1 (to1 r :: f Integer))

toFromInverse1 :: forall f. Generic1Prop f
toFromInverse1 genf _genr = property $ do
  v <- forAll $ genf genSmallInteger
  v === (to1 . from1 $ v)

