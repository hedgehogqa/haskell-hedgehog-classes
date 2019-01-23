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

type AlternativeProp f =
  ( Alternative f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

alternativeLeftIdentity :: forall f. AlternativeProp f
alternativeLeftIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = empty <|> a
  let rhs = a
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Left Identity", lawContextLawBody = "forall a. empty <|> a == a"
        , lawContextTcName = "Alternative", lawContextTcProp =
            let showX = show lhs; showY = show rhs;
            in "empty <|> " ++ showX ++ " == " ++ showY
        }
  heqCtx1 lhs rhs ctx

alternativeRightIdentity :: forall f. AlternativeProp f
alternativeRightIdentity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  let lhs = a <|> empty
  let rhs = a
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Right Identity", lawContextLawBody = "forall a. a <|> empty == a"
        , lawContextTcName = "Alternative", lawContextTcProp =
            let showX = show lhs; showY = show rhs;
            in showX ++ " <|> empty == " ++ showY
        }
  heqCtx1 lhs rhs ctx  

alternativeAssociativity :: forall f. AlternativeProp f
alternativeAssociativity fgen = property $ do
  a <- forAll $ fgen genSmallInteger
  b <- forAll $ fgen genSmallInteger
  c <- forAll $ fgen genSmallInteger
  let lhs = (a <|> (b <|> c))
  let rhs = ((a <|> b) <|> c)
  let ctx = showLawContext $ LawContext
        { lawContextLawName = "Associativity", lawContextLawBody = "forall a b c. a <|> (b <|> c) == (a <|> b) <|> c"
        , lawContextTcName = "Alternative", lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in showA ++ " <|> (" ++ showB ++ " <|> " ++ showC ++ ") == ("
               ++ showA ++ " <|> " ++ showB ++ ") <|> " ++ showC
        }
  heqCtx1 lhs rhs ctx
