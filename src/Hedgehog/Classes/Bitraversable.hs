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
  let t = apTrans
  let f = func4
  let g = func4
  let x' = bitraverse (t . f) (t . g) x
  let y' = t (bitraverse f g x)
  x' === y'

bitraversableIdentity :: forall f. BitraversableProp f
bitraversableIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let x' = bitraverse Identity Identity x
  let y' = Identity x
  x' === y'


bitraversableComposition :: forall f. BitraversableProp f
bitraversableComposition fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let f1 = func6
  let f2 = func5
  let g1 = func4
  let g2 = func4
  let x' :: Compose Triple (Compose Triple (WL.Writer (S.Set Integer))) (f Integer Integer)
      x' = Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2 $ x
      
  let y' :: Compose Triple (Compose Triple (WL.Writer (S.Set Integer))) (f Integer Integer)
      y' = bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) x
  x' === y' 
