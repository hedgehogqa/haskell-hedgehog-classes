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
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Naturality", lawContextLawBody = "bitraverse (t . f) (t . g)" `congruency` "t . bitraverse f g, for every applicative transformation t"
        , lawContextTcName = "Bitraversable", lawContextTcProp =
            let showX = show x;
            in lawWhere
                 [ "bitraverse (t . f) (t . g) $ x" `congruency` "t . bitraverse f g $ x, for every applicative transformation t, where"
                 , "x = " ++ showX
                 ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx1 lhs rhs ctx  

bitraversableIdentity :: forall f. BitraversableProp f
bitraversableIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let lhs = bitraverse Identity Identity x
  let rhs = Identity x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Identity", lawContextLawBody = "bitraverse Identity Identity == Identity"
        , lawContextTcName = "Bitraversable", lawContextTcProp =
            let showX = show x;
            in lawWhere
                 [ "bitraverse Identity Identity x" `congruency` "Identity x, where"
                 , "x = " ++ showX
                 ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx1 lhs rhs ctx  
  
bitraversableComposition :: forall f. BitraversableProp f
bitraversableComposition fgen = property $ do
  x <- forAll $ fgen genSmallInteger genSmallInteger
  let f1 = func6; f2 = func5; g1 = func4; g2 = func4
  let lhs :: Compose Triple (Compose Triple (WL.Writer (S.Set Integer))) (f Integer Integer)
      lhs = Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2 $ x
      
  let rhs :: Compose Triple (Compose Triple (WL.Writer (S.Set Integer))) (f Integer Integer)
      rhs = bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Composition", lawContextLawBody = "Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2 == bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2)"
        , lawContextTcName = "Bitraversable", lawContextTcProp =
            let showX = show x;  
            in lawWhere
                 [ "Compose . fmap (bitraverse g1 g2) . bitraverse f1 f2 $ x" `congruency` "bitraverse (Compose . fmap g1 . f1) (Compose . fmap g2 . f2) $ x, where"
                 , "x = " ++ showX
                 ]
        , lawContextReduced = reduced lhs rhs 
        }
  heqCtx1 lhs rhs ctx
