{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

#ifndef HAVE_COMONAD

module Hedgehog.Classes.Comonad () where

#else

module Hedgehog.Classes.Comonad (comonadLaws) where

import Control.Comonad

import Hedgehog
import Hedgehog.Classes.Common

comonadLaws ::
  ( Comonad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Laws
comonadLaws gen = Laws "Comonad"
  [ ("Extend/Extract Identity", extendExtractIdentity gen)
  , ("Extract/Extend", extractExtend gen)
  , ("Extend/Extend", extendExtend gen)
  , ("Extract Right Identity", extractRightIdentity gen)
  , ("Extract Left Identity", extractLeftIdentity gen)
  , ("Cokleisli Associativity", cokleisliAssociativity gen)
  , ("Extract/Duplicate Identity", extractDuplicateIdentity gen)
  , ("Fmap Extract/Duplicate Identity", fmapExtractDuplicateIdentity gen)
  , ("Double Duplication", doubleDup gen)
  , ("Extend/Fmap . Duplicate Identity", extendDuplicate gen)
  , ("Duplicate/Extend id Identity", duplicateExtendId gen)
  , ("Fmap/Extend Extract", fmapExtendExtract gen)
  , ("Fmap/LiftW Isomorphism", fmapLiftW gen)
  ]

type ComonadProp f =
  ( Comonad f
  , forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)
  ) => (forall x. Gen x -> Gen (f x)) -> Property

extendExtractIdentity :: forall f. ComonadProp f
extendExtractIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  let lhs = extend extract x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Extend/Extract Identity", lawContextTcName = "Comonad"
        , lawContextLawBody = "extend extract" `congruency` "id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "extend extract x" `congruency` "x, where"
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

extractExtend :: forall f. ComonadProp f
extractExtend fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  k <- forAll $ genLinearEquationW fgen
  let k' = runLinearEquationW k
  let lhs = extract . extend k' $ x
  let rhs = k' x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Extract/Extend", lawContextTcName = "Comonad"
        , lawContextLawBody = "extract . extend f" `congruency` "f"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "extract . extend f $ x" `congruency` "f x, where"
            , "f = " ++ show k
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

extendExtend :: forall f. ComonadProp f
extendExtend fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  f' <- forAll $ genLinearEquationW fgen
  g' <- forAll $ genLinearEquationW fgen
  let f = runLinearEquationW f'
  let g = runLinearEquationW g'
  let lhs = extend f . extend g $ x
  let rhs = extend (f . extend g) x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Extend/Extend", lawContextTcName = "Comonad"
        , lawContextLawBody = "extend f . extend g" `congruency` "extend (f . extend g)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "extend f . extend g $ x" `congruency` "extend (f . extend g) $ x, where"
            , "f = " ++ show f'
            , "g = " ++ show g'
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

extractRightIdentity :: forall f. ComonadProp f
extractRightIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  f' <- forAll $ genLinearEquationW fgen
  let f = runLinearEquationW f'
  let lhs = f =>= extract $ x
  let rhs = f x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Extract Cokleisli Right Identity", lawContextTcName = "Comonad"
        , lawContextLawBody = "f =>= extract" `congruency` "f"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "f =>= extract $ x" `congruency` "f x, where"
            , "f = " ++ show f'
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

extractLeftIdentity :: forall f. ComonadProp f
extractLeftIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  f' <- forAll $ genLinearEquationW fgen
  let f = runLinearEquationW f'
  let lhs = extract =>= f $ x
  let rhs = f x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Extract Cokleisli Left Identity", lawContextTcName = "Comonad"
        , lawContextLawBody = "extract =>= f" `congruency` "f"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "extract =>= f $ x" `congruency` "f x, where"
            , "f = " ++ show f'
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

cokleisliAssociativity :: forall f. ComonadProp f
cokleisliAssociativity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  f' <- forAll $ genLinearEquationW fgen
  g' <- forAll $ genLinearEquationW fgen
  h' <- forAll $ genLinearEquationW fgen
  let f = runLinearEquationW f'
  let g = runLinearEquationW g'
  let h = runLinearEquationW h'
  let lhs = (f =>= g) =>= h $ x
  let rhs = f =>= (g =>= h) $ x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Cokleisli Associativity", lawContextTcName = "Comonad"
        , lawContextLawBody = "(f =>= g) =>= h" `congruency` "f =>= (g =>= h)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "(f =>= g) =>= h $ x" `congruency` "f =>= (g =>= h) $ x, where"
            , "f = " ++ show f'
            , "g = " ++ show g'
            , "h = " ++ show h'
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx


extractDuplicateIdentity :: forall f. ComonadProp f
extractDuplicateIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  let lhs = extract . duplicate $ x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Extract/Duplicate Identity", lawContextTcName = "Comonad"
        , lawContextLawBody = "extract . duplicate" `congruency` "id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "extract . duplicate $ x" `congruency` "x, where"
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

fmapExtractDuplicateIdentity :: forall f. ComonadProp f
fmapExtractDuplicateIdentity fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  let lhs = fmap extract . duplicate $ x
  let rhs = x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Fmap Extract/Duplicate Identity", lawContextTcName = "Comonad"
        , lawContextLawBody = "fmap extract . duplicate" `congruency` "id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "fmap extract . duplicate $ x" `congruency` "x, where"
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

doubleDup :: forall f. ComonadProp f
doubleDup fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  let lhs = duplicate . duplicate $ x
  let rhs = fmap duplicate . duplicate $ x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Double Duplicate", lawContextTcName = "Comonad"
        , lawContextLawBody = "duplicate . duplicate" `congruency` "fmap duplicate . duplicate"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "duplicate . duplicate $ x" `congruency` "fmap duplicate . duplicate $ x, where"
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

extendDuplicate :: forall f. ComonadProp f
extendDuplicate fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  f' <- forAll $ genLinearEquationW fgen
  let f = runLinearEquationW f'
  let lhs = extend f $ x
  let rhs = fmap f . duplicate $ x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Extend/Fmap Duplicate", lawContextTcName = "Comonad"
        , lawContextLawBody = "extend f" `congruency` "fmap f . duplicate"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "extend f x" `congruency` "fmap f . duplicate $ x, where"
            , "f = " ++ show f'
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

duplicateExtendId :: forall f. ComonadProp f
duplicateExtendId fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  let lhs = duplicate x
  let rhs = extend id x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Duplicate/Extend Id", lawContextTcName = "Comonad"
        , lawContextLawBody = "duplicate" `congruency` "extend id"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "duplicate x" `congruency` "extend id x, where"
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

fmapExtendExtract :: forall f. ComonadProp f
fmapExtendExtract fgen = property $ do
  x :: f Integer <- forAll $ fgen genSmallInteger
  f' <- forAll genLinearEquation
  let f = runLinearEquation f'
  let lhs = fmap f x
  let rhs = extend (f . extract) x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Fmap/Extend Extract", lawContextTcName = "Comonad"
        , lawContextLawBody = "fmap f" `congruency` "extend (f . extract)"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "fmap f x" `congruency` "extend (f . extract) x, where"
            , "f = " ++ show f'
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

fmapLiftW :: forall f. ComonadProp f
fmapLiftW fgen = property $ do
  x <- forAll $ fgen genSmallInteger
  f' <- forAll genLinearEquation
  let f = runLinearEquation f'
  let lhs = fmap f x
  let rhs = liftW f x
  let ctx = contextualise $ LawContext
        { lawContextLawName = "Fmap/LiftW", lawContextTcName = "Comonad"
        , lawContextLawBody = "fmap" `congruency` "liftW"
        , lawContextReduced = reduced lhs rhs
        , lawContextTcProp = lawWhere
            [ "fmap f x" `congruency` "liftW f x, where"
            , "f = " ++ show f'
            , "x = " ++ show x
            ]
        }
  heqCtx lhs rhs ctx

#endif
