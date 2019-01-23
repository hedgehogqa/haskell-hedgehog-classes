{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Bitraversable (bitraversableLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Data.Bitraversable (Bitraversable(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))

import qualified Data.Set as S
import qualified Control.Monad.Trans.Writer.Lazy as WL

bitraversableLaws :: forall f.
  ( Bitraversable f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
bitraversableLaws gen = Laws "Bitraversable"
  [ ("Naturality", bitraversableNaturality gen)
  , ("Identity", bitraversableIdentity gen)
  , ("Composition", bitraversableComposition gen) 
  ]

type BitraversableProp f =
  ( Bitraversable f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

bitraversableNaturality :: forall f. BitraversableProp f
bitraversableNaturality fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let t = apTrans; f = func4; g = func4
  let lhs = bitraverse (t . f) (t . g) x
  let rhs = t (bitraverse f g x)
  lhs `heq1` rhs

bitraversableIdentity :: forall f. BitraversableProp f
bitraversableIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = bitraverse Identity Identity x
  let rhs = Identity x
  lhs `heq1` rhs

bitraversableComposition :: forall f. BitraversableProp f
bitraversableComposition fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let f1 = func6; f2 = func5; g1 = func4; g2 = func4
  let lhs :: Compose Triple (Compose Triple (WL.Writer (S.Set Integer))) (f Integer Integer)
      lhs = Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2 $ x
      
  let rhs :: Compose Triple (Compose Triple (WL.Writer (S.Set Integer))) (f Integer Integer)
      rhs = bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) x
  lhs `heq1` rhs
