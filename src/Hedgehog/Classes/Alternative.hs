{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}

module Hedgehog.Classes.Alternative (alternativeLaws) where

import Control.Applicative (Alternative(..))

import Hedgehog
import Hedgehog.Classes.Common

alternativeLaws ::
  ( Alternative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
alternativeLaws gen = Laws "Alternative"
  [ ("Left Identity", alternativeLeftIdentity gen)
  , ("Right Identity", alternativeRightIdentity gen)
  , ("Associativity", alternativeAssociativity gen)
  ]

leftIdentity :: (Show a, Show b) => a -> b -> LawContext
leftIdentity x y = LawContext
  { lawContextLawName = "Left Identity"
  , lawContextLawBody = "forall a. empty <|> a == a"
  , lawContextTcName = "Alternative"
  , lawContextTcProp =
      let showX = show x
          showY = show y
      in "empty <|> " ++ showX ++ " == " ++ showY
  }

alternativeLeftIdentity ::
  ( Alternative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
alternativeLeftIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = empty <|> a
  let rhs = a
  let ctx = showLawContext (leftIdentity lhs rhs)
  heqCtx1 lhs rhs ctx

rightIdentity :: (Show a, Show b) => a -> b -> LawContext
rightIdentity x y = LawContext
  { lawContextLawName = "Right Identity"
  , lawContextLawBody = "forall a. a == empty <|> a"
  , lawContextTcName = "Alternative"
  , lawContextTcProp =
      let showX = show x
          showY = show y
      in showY ++
"empty <|> " ++ showX ++ " == " ++ showY
  }


alternativeRightIdentity ::
  ( Alternative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
alternativeRightIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = a
  let rhs = (empty <|> a)
  let ctx = showLawContext (rightIdentity lhs rhs)
  heqCtx1 lhs rhs ctx  

alternativeAssociativity ::
  ( Alternative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property
alternativeAssociativity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  b <- forAll $ fgen genSmallInteger
  c <- forAll $ fgen genSmallInteger
  (a <|> (b <|> c)) `heq1` ((a <|> b) <|> c)
