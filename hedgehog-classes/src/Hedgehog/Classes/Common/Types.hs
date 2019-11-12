{-# language
    ConstraintKinds
  , KindSignatures
  , ImpredicativeTypes
  , QuantifiedConstraints
  , RankNTypes
  #-}

module Hedgehog.Classes.Common.Types
  ( Ctx
  , Ctx1
  , Ctx2

  , Gen1
  , Gen2
  ) where

import Hedgehog

import Data.Kind (Type, Constraint)

type Ctx (c :: Type -> Constraint) (a :: Type)
  = ( c a
    , Eq a
    , Show a
    )

type Ctx1 (c :: (Type -> Type) -> Constraint) (f :: Type -> Type)
  = (( c f
    , forall x. Eq x => Eq (f x)
    , forall x. Show x => Show (f x)
    ) :: Constraint)

type Ctx2 (c :: (Type -> Type -> Type) -> Constraint) f
  = (( c f
    , forall x y. (Eq x, Eq y) => Eq (f x y)
    , forall x y. (Show x, Show y) => Show (f x y)
    ) :: Constraint)

type Gen1 f = forall x. Gen x -> Gen (f x)

type Gen2 f = forall x y. Gen x -> Gen y -> Gen (f x y)
