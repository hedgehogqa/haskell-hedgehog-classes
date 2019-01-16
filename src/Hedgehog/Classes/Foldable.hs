{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Hedgehog.Classes.Foldable (foldableLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (ErrorCall(..), try, evaluate)
import Data.Monoid (Sum(..), Endo(..), Dual(..))
import qualified Data.Foldable as Foldable

foldableLaws ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
foldableLaws gen = Laws "Foldable"
  [ ("fold", foldableFold gen)
  , ("foldMap", foldableFoldMap gen)
  , ("foldr", foldableFoldr gen)
  , ("foldr'", foldableFoldr' gen)
  , ("foldl", foldableFoldl gen)
  , ("foldl'", foldableFoldl' gen)
  , ("foldl1", foldableFoldl1 gen)
  , ("foldr1", foldableFoldr1 gen)
  , ("toList", foldableToList gen)
  , ("null", foldableNull gen)
  , ("length", foldableLength gen)
  ]

foldableFold ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFold fgen = property $ do
  a <- forAll $ fgen genSmallSum
  let lhs = Foldable.fold a
  let rhs = Foldable.foldMap id a
  heqCtx lhs rhs NoContext

foldableFoldMap ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldMap fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  e <- forAll genQuadraticEquation
  let f = Sum . runQuadraticEquation e
  let lhs = Foldable.foldMap f a
  let rhs = Foldable.foldr (mappend . f) mempty a
  heqCtx lhs rhs NoContext  

foldableFoldr ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldr fgen = property $ do
  e <- forAll genLinearEquationTwo
  z <- forAll genSmallInteger
  t <- forAll $ fgen genSmallInteger
  let f = runLinearEquationTwo e
  let lhs = Foldable.foldr f z t
  let rhs = appEndo (Foldable.foldMap (Endo . f) t) z
  heqCtx lhs rhs NoContext  

foldableFoldl ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldl fgen = property $ do
  e <- forAll genLinearEquationTwo
  z <- forAll genSmallInteger
  t <- forAll $ fgen genSmallInteger
  let f = runLinearEquationTwo e
  let lhs = Foldable.foldl f z t
  let rhs = appEndo (getDual (Foldable.foldMap (Dual . Endo . flip f) t)) z
  heqCtx lhs rhs NoContext

foldableFoldr' ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldr' fgen = property $ do
  xs <- forAll $ fgen (genBottom genSmallInteger)
  let f :: Bottom Integer -> Integer -> Integer
      f a b = case a of
        BottomUndefined -> error "foldableFoldr': your foldr' is not strict!"
        BottomValue v -> if even v then v else b
  let z0 = 0
  r1 <- liftIO $ do
    let f' k x z = k $! f x z
    e <- try (evaluate (Foldable.foldl f' id xs z0))
    case e of
      Left (_ :: ErrorCall) -> pure Nothing
      Right i -> pure (Just i)
  r2 <- liftIO $ do
    e <- try (evaluate (Foldable.foldr' f z0 xs))
    case e of
      Left (_ :: ErrorCall) -> pure Nothing
      Right i -> pure (Just i)
  r1 `heq` r2

foldableFoldl' ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldl' fgen = property $ do
  xs <- forAll $ fgen (genBottom genSmallInteger)
  let f :: Integer -> Bottom Integer -> Integer
      f a b = case b of
        BottomUndefined -> error "foldableFoldl': your foldl' is not strict!"
        BottomValue v -> if even v then a else v
  let z0 = 0
  r1 <- liftIO $ do
    let f' x k z = k $! f z x
    e <- try (evaluate (Foldable.foldr f' id xs z0))
    case e of
      Left (_ :: ErrorCall) -> pure Nothing
      Right i -> pure (Just i)
  r2 <- liftIO $ do
    e <- try (evaluate (Foldable.foldl' f z0 xs))
    case e of
      Left (_ :: ErrorCall) -> pure Nothing
      Right i -> pure (Just i) 
  heqCtx r1 r2 (Context "Your implementation of foldl' is not strict.")

foldableFoldl1 ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldl1 fgen = property $ do
  e <- forAll genLinearEquationTwo
  t <- forAll $ fgen genSmallInteger
  case compatToList t of
    [] -> success
    (x:xs) ->
      let f = runLinearEquationTwo e
      in Foldable.foldl1 f t `heq` Foldable.foldl f x xs

foldableFoldr1 ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableFoldr1 fgen = property $ do
  e <- forAll genLinearEquationTwo
  t <- forAll $ fgen genSmallInteger
  case unsnoc (compatToList t) of
    Nothing -> success
    Just (xs, x) ->
      let f = runLinearEquationTwo e
      in Foldable.foldr1 f t `heq` Foldable.foldr f x xs

foldableToList ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableToList fgen = property $ do
  t <- forAll $ fgen genSmallInteger
  Foldable.toList t `heq` Foldable.foldr (:) [] t

foldableNull ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableNull fgen = property $ do
  t <- forAll $ fgen genSmallInteger
  Foldable.null t `heq` Foldable.foldr (const (const False)) True t

foldableLength ::
  ( Foldable f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
foldableLength fgen = property $ do
  t <- forAll $ fgen genSmallInteger
  Foldable.length t `heq` getSum (Foldable.foldMap (const (Sum 1)) t)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = \case
  [] -> Nothing
  [x] -> Just ([], x)
  (x:y:xs) -> fmap (\(bs,b) -> (x:bs,b)) (unsnoc (y : xs))

compatToList :: Foldable f => f a -> [a]
compatToList = Foldable.foldMap (\x -> [x])
