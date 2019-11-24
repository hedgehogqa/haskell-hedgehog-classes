{-# language CPP #-}

{-| This library provides sets of properties that should hold for common
    typeclasses.

    /Note:/ functions that test laws of a subclass never test the laws of
    a superclass. For example, 'commutativeSemigroupLaws' never tests
    the laws provided by 'semigroupLaws'.
-}
module Hedgehog.Classes
  ( -- * Running
    lawsCheck
  , lawsCheckOne
  , lawsCheckMany

    -- * Properties
    -- ** Ground types
  , binaryLaws
  , bitsLaws
  , eqLaws
  , integralLaws
  , monoidLaws
  , commutativeMonoidLaws
  , ordLaws
--  , ixLaws
  , enumLaws
  , boundedEnumLaws
  , semigroupLaws
  , commutativeSemigroupLaws
  , exponentialSemigroupLaws
  , idempotentSemigroupLaws
  , rectangularBandSemigroupLaws
#if HAVE_AESON
  , jsonLaws
#endif
  , genericLaws
#if HAVE_PRIMITIVE
  , primLaws
#endif
#if HAVE_SEMIRINGS
  , semiringLaws
  , ringLaws
  , starLaws
#endif
  , showLaws
  , showReadLaws
  , storableLaws

    -- ** Unary type constructors
  , alternativeLaws
  , applicativeLaws
#if HAVE_COMONAD
  , comonadLaws
#endif
  , contravariantLaws
  , foldableLaws
  , functorLaws
  , monadLaws
--  , monadFixLaws
  , monadIOLaws
  , monadPlusLaws
  , monadZipLaws
  , traversableLaws

    -- ** Binary type constructors
  , arrowLaws
  , bifoldableLaws
  , bifoldableFunctorLaws
  , bifunctorLaws
  , bitraversableLaws
  , categoryLaws
  , commutativeCategoryLaws
#if HAVE_PROFUNCTORS
  , profunctorLaws
#endif

    -- * Defining your own 'Laws'
  , Laws(..)
  , LawContext(..)
  , Context(..)
  , contextualise

    -- * Hedgehog equality tests sans source information
  , hLessThan, hGreaterThan
  , heq, heq1, heq2
  , heqCtx, heqCtx1, heqCtx2
  , hneq, hneq1, hneq2
  , hneqCtx, hneqCtx1, hneqCtx2
  ) where

import Hedgehog.Classes.Alternative (alternativeLaws)
import Hedgehog.Classes.Applicative (applicativeLaws)
import Hedgehog.Classes.Arrow (arrowLaws)
import Hedgehog.Classes.Bifoldable (bifoldableLaws, bifoldableFunctorLaws)
import Hedgehog.Classes.Bifunctor (bifunctorLaws)
import Hedgehog.Classes.Binary (binaryLaws)
import Hedgehog.Classes.Bitraversable (bitraversableLaws)
import Hedgehog.Classes.Bits (bitsLaws)
import Hedgehog.Classes.Category (categoryLaws, commutativeCategoryLaws)
import Hedgehog.Classes.Common
#ifdef HAVE_COMONAD
import Hedgehog.Classes.Comonad (comonadLaws)
#endif
import Hedgehog.Classes.Contravariant (contravariantLaws)
import Hedgehog.Classes.Enum (enumLaws, boundedEnumLaws)
import Hedgehog.Classes.Eq (eqLaws)
import Hedgehog.Classes.Foldable (foldableLaws)
import Hedgehog.Classes.Functor (functorLaws)
import Hedgehog.Classes.Generic (genericLaws)
import Hedgehog.Classes.Integral (integralLaws)
--import Hedgehog.Classes.Ix (ixLaws)
#if HAVE_AESON
import Hedgehog.Classes.Json (jsonLaws)
#endif
import Hedgehog.Classes.Monad (monadLaws)
import Hedgehog.Classes.MonadIO (monadIOLaws)
import Hedgehog.Classes.MonadPlus (monadPlusLaws)
import Hedgehog.Classes.MonadZip (monadZipLaws)
import Hedgehog.Classes.Monoid (monoidLaws, commutativeMonoidLaws)
#if HAVE_PROFUNCTORS
import Hedgehog.Classes.Profunctor (profunctorLaws)
#endif
import Hedgehog.Classes.Ord (ordLaws)
#if HAVE_PRIMITIVE
import Hedgehog.Classes.Prim (primLaws)
#endif
import Hedgehog.Classes.Semigroup (semigroupLaws, commutativeSemigroupLaws, exponentialSemigroupLaws, idempotentSemigroupLaws, rectangularBandSemigroupLaws)
#if HAVE_SEMIRINGS
import Hedgehog.Classes.Semiring (semiringLaws, ringLaws, starLaws)
#endif
import Hedgehog.Classes.Show (showLaws)
import Hedgehog.Classes.ShowRead (showReadLaws)
import Hedgehog.Classes.Storable (storableLaws)
import Hedgehog.Classes.Traversable (traversableLaws)

