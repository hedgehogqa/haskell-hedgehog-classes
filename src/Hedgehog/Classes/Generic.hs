{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Hedgehog.Classes.Generic (genericLaws) where

import Hedgehog
import Hedgehog.Classes.Common

import GHC.Generics (Generic(..))

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

fromToInverse :: forall a x.
  ( Generic a
  , Eq (Rep a x)
  , Show (Rep a x)
  ) => Gen a -> Gen (Rep a x) -> Property
fromToInverse _gena genr = property $ do
  r <- forAll genr
  let lhs = r
  let rhs = from (to r :: a)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "From-To inverse", lawContextTcName = "Generic"
        , lawContextLawBody = "from . to" `congruency` "id"
        , lawContextTcProp =
            let showR = show r
            in lawWhere
              [ "from . to $ r" `congruency` "id r, where"
              , "r = " ++ showR
              ]
        , lawContextReduced = reduced lhs rhs
        } 
  heqCtx lhs rhs ctx

toFromInverse :: forall a x.
  ( Generic a
  , Eq a
  , Show a
  ) => Gen a -> Gen (Rep a x) -> Property
toFromInverse gena _genr = property $ do
  v <- forAll gena
  let lhs = to (from v)
  let rhs = v
  let ctx = contextualise $ LawContext
        { lawContextLawName = "To-From inverse", lawContextTcName = "Generic"
        , lawContextLawBody = "to . from" `congruency` "id"
        , lawContextTcProp =
            let showV = show v
            in lawWhere
              [ "to . from $ v" `congruency` "id v, where"
              , "v = " ++ showV
              ]
        , lawContextReduced = reduced lhs rhs
        } 
  heqCtx lhs rhs ctx
       
{-
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
-}

{-
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
-}
