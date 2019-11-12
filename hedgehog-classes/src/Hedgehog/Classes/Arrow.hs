{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Arrow (arrowLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Control.Arrow(Arrow(..), (>>>))
import Control.Category(Category(..))
import Prelude hiding (id, (.))
import qualified Prelude

-- | Tests the following 'Arrow' laws:
--
-- [__Arr Identity__]: @'arr' 'id'@ ≡ @'id'@
-- [__Arr Composition__]: @'arr' (f '>>>' g)@ ≡ @'arr' f '>>>' 'arr' g@
-- [__Arr-First inverse__]: @'first' ('arr' f)@ ≡ @'arr' ('first' f)@
-- [__First Composition__]: @'first' (f '>>>' g)@ ≡ @'first' f '>>>' 'first' g@
-- [__Arrow Law 5__]: @'first' f '>>>' 'arr' 'fst'@ ≡ @'arr' 'fst' '>>>' f@
-- [__Arrow Law 6__]: @'first' f '>>>' 'arr' ('id' '***' g)@ ≡ @'arr' ('id' '***' g) '>>>' 'first' f@
-- [__Arrow Law 7__]: @'first' ('first' f) '>>>' 'arr' assoc@ ≡ @'arr' assoc '>>>' 'first' f, where assoc ((a,b),c) = (a,(b,c))@
arrowLaws :: forall f.
  ( Arrow f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Laws
arrowLaws gen = Laws "Arrow"
  [ ("Arr Identity", arrowLaw1 gen)
  , ("Arr Composition", arrowLaw2 gen)
  , ("Arr . First == First . Arr", arrowLaw3 gen)
  , ("First Composition", arrowLaw4 gen)
  , ("Arrow Law 5", arrowLaw5 gen)
  , ("Arrow Law 6", arrowLaw6 gen)
  , ("Arrow Law 7", arrowLaw7 gen)
  ]

type ArrowProp f =
  ( Arrow f
  , forall x y. (Eq x, Eq y) => Eq (f x y)
  , forall x y. (Show x, Show y) => Show (f x y)
  ) => (forall x y. Gen x -> Gen y -> Gen (f x y)) -> Property

arrowLaw1 :: forall f. ArrowProp f
arrowLaw1 _ = property $ do
  arr Prelude.id `heq2` (id :: f Integer Integer)

arrowLaw2 :: forall f. ArrowProp f
arrowLaw2 _ = property $ do
  f' <- forAll genQuadraticEquation
  g' <- forAll genQuadraticEquation
  let f = runQuadraticEquation f'
      g = runQuadraticEquation g'
  (arr (f >>> g) :: f Integer Integer) `heq2` (arr f >>> arr g) 

arrowLaw3 :: forall f. ArrowProp f
arrowLaw3 _ = property $ do
  f' <- forAll genQuadraticEquation
  let f = runQuadraticEquation f'
  let x = first (arr f) :: f (Integer, Integer) (Integer, Integer)
  let y = arr (first f) :: f (Integer, Integer) (Integer, Integer) 
  x `heq2` y

arrowLaw4 :: forall f. ArrowProp f
arrowLaw4 fgen = property $ do
  f <- forAll $ fgen genSmallInteger genSmallInteger
  g <- forAll $ fgen genSmallInteger genSmallInteger
  let x = first (f >>> g) :: f (Integer, Integer) (Integer, Integer)
  let y = first f >>> first g :: f (Integer, Integer) (Integer, Integer)
  x `heq2` y 

arrowLaw5 :: forall f. ArrowProp f
arrowLaw5 fgen = property $ do
  f <- forAll $ fgen genSmallInteger genSmallInteger
  let x = first f >>> arr fst :: f (Integer, Integer) Integer
  let y = arr fst >>> f :: f (Integer, Integer) Integer
  x `heq2` y 

arrowLaw6 :: forall f. ArrowProp f
arrowLaw6 fgen = property $ do
  f <- forAll $ fgen genSmallInteger genSmallInteger
  g' <- forAll genQuadraticEquation
  let g = runQuadraticEquation g'
  let x = ((first f) >>> (arr (Prelude.id *** g))) :: f (Integer, Integer) (Integer, Integer) 
  let y = arr (id *** g) >>> first f :: f (Integer, Integer) (Integer, Integer) 
  x `heq2` y

arrowLaw7 :: forall f. ArrowProp f
arrowLaw7 fgen = property $ do
  let assoc ((a,b),c) = (a,(b,c))
  f <- forAll $ fgen genSmallInteger genSmallInteger
  let x = first (first f) >>> arr assoc :: f ((Integer, Integer), Integer) (Integer, (Integer, Integer))
  let y = arr assoc >>> first f :: f ((Integer, Integer), Integer) (Integer, (Integer, Integer))
  x `heq2` y

