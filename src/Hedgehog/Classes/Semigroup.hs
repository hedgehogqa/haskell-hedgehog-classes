{-# LANGUAGE ScopedTypeVariables #-}

module Hedgehog.Classes.Semigroup
  ( semigroupLaws
  , commutativeSemigroupLaws
  , exponentialSemigroupLaws
  , idempotentSemigroupLaws
  , rectangularBandSemigroupLaws
  ) where

import Data.Semigroup (Semigroup(..))
import Hedgehog
import Hedgehog.Classes.Common
import Data.List.NonEmpty
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Foldable as Foldable

-- | Tests the following 'Semigroup' laws:
--
-- [__Associativity__]: @@ ≡ @@
-- [__Concatenation__]: @@ ≡ @@
-- [__Times__]: @@ ≡ @@
semigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
semigroupLaws gen = Laws "Semigroup"
  [ ("Associativity", semigroupAssociative gen)
  , ("Concatenation", semigroupConcatenation gen)
  , ("Times", semigroupTimes gen)
  ]

-- | Tests the following 'Semigroup' laws:
--
-- [__Commutativity__]: @@ ≡ @@
commutativeSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
commutativeSemigroupLaws gen = Laws "Commutative Semigroup"
  [ ("Commutative", semigroupCommutative gen)
  ]

-- | Tests the following 'Semigroup' laws:
--
-- [__Exponential__]: @@ ≡ @@
exponentialSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
exponentialSemigroupLaws gen = Laws "Exponential Semigroup"
  [ ("Exponential", semigroupExponential gen)
  ]

-- | Tests the following 'Semigroup' laws:
--
-- [__Idempotent__]: @@ ≡ @@
idempotentSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
idempotentSemigroupLaws gen = Laws "Idempotent Semigroup"
  [ ("Idempotent", semigroupIdempotent gen)
  ]

-- | Tests the following 'Semigroup' laws:
--
-- [__Rectangular Band__]: @@ ≡ @@
rectangularBandSemigroupLaws :: (Eq a, Semigroup a, Show a) => Gen a -> Laws
rectangularBandSemigroupLaws gen = Laws "Rectangular Band Semigroup"
  [ ("Rectangular Band", semigroupRectangularBand gen)
  ]

semigroupAssociative :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupAssociative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  c <- forAll gen
  let lhs = a <> (b <> c)
  let rhs = (a <> b) <> c
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Associativity", lawContextTcName = "Semigroup"
        , lawContextLawBody = "a <> (b <> c)" `congruency` "(a <> b) <> c"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b; showC = show c;
            in lawWhere
              [ "a <> (b <> c)" `congruency` "(a <> b) <> c, where"
              , "a = " ++ showA
              , "b = " ++ showB
              , "c = " ++ showC
              ]
        }
  heqCtx lhs rhs ctx

semigroupCommutative :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupCommutative gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let lhs = a <> b
  let rhs = b <> a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Commutativity", lawContextTcName = "Semigroup"
        , lawContextLawBody = "a <> b" `congruency` "b <> a"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b;
            in lawWhere
              [ "a <> b" `congruency` "b <> a, where"
              , "a = " ++ showA
              , "b = " ++ showB
              ]
        }  
  heqCtx lhs rhs ctx

semigroupConcatenation :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupConcatenation gen = property $ do
  a <- forAll gen 
  as <- forAll $ genSmallList gen
  let ne = a :| as
  let lhs = sconcat ne
  let rhs = Foldable.foldr1 (<>) ne
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Concatenation", lawContextTcName = "Semigroup"
        , lawContextLawBody = "sconcat" `congruency` "foldr1 (<>)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showNE = show ne;
            in lawWhere
              [ "sconcat ne" `congruency` "foldr1 (<>) ne, where"
              , "ne = " ++ showNE
              ]
        }
  heqCtx lhs rhs ctx  

semigroupTimes :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupTimes gen = property $ do
  a <- forAll gen
  n <- forAll (Gen.int (Range.linear 2 5))
  let lhs = stimes n a
  let rhs = Foldable.foldr1 (<>) (replicate n a)
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Times", lawContextTcName = "Semigroup"
        , lawContextLawBody = "stimes" `congruency` "(foldr1 (<>) .) . replicate"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showN = show n; showA = show a;
            in lawWhere
              [ "stimes n a" `congruency` "foldr1 (<>) (replicate n a), where"
              , "a = " ++ showA
              , "n = " ++ showN
              ]
        }  
  heqCtx lhs rhs ctx

semigroupExponential :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupExponential gen = property $ do
  a <- forAll gen
  b <- forAll gen
  n <- forAll (Gen.int (Range.linear 2 5))
  let lhs = stimes n (a <> b)
  let rhs = stimes n a <> stimes n b
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Exponential", lawContextTcName = "Semigroup"
        , lawContextLawBody = "stimes n (a <> b)" `congruency` "stimes n a <> stimes n b"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showN = show n; showA = show a; showB = show b;
            in lawWhere
              [ "stimes n (a <> b)" `congruency` "stimes n a <> stimes n b, where"
              , "a = " ++ showA
              , "b = " ++ showB
              , "n = " ++ showN
              ]
        }  
  heqCtx lhs rhs ctx

semigroupIdempotent :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupIdempotent gen = property $ do
  a <- forAll gen
  let lhs = a <> a
  let rhs = a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Idempotency", lawContextTcName = "Semigroup"
        , lawContextLawBody = "a <> a" `congruency` "a"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a;
            in lawWhere
              [ "a <> a" `congruency` "a, where"
              , "a = " ++ showA
              ]
        }
  heqCtx lhs rhs ctx

semigroupRectangularBand :: forall a. (Eq a, Semigroup a, Show a) => Gen a -> Property
semigroupRectangularBand gen = property $ do
  a <- forAll gen
  b <- forAll gen
  let lhs = a <> b <> a
  let rhs = a
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Rectangular Band", lawContextTcName = "Semigroup"
        , lawContextLawBody = "a <> b <> a" `congruency` "a"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp =
            let showA = show a; showB = show b;
            in lawWhere
              [ "a <> b <> a" `congruency` "a, where"
              , "a = " ++ showA
              , "b = " ++ showB
              ]
        }  
  heqCtx lhs rhs ctx
