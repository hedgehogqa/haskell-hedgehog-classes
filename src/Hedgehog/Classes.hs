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
  , showLaws
  , showReadLaws
  , storableLaws
  , genericLaws
  , jsonLaws

    -- ** Unary type constructors
  , alternativeLaws
  , applicativeLaws
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

    -- * Defining your own laws
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
import Hedgehog.Classes.Bitraversable (bitraversableLaws)
import Hedgehog.Classes.Bits (bitsLaws)
import Hedgehog.Classes.Category (categoryLaws, commutativeCategoryLaws)
import Hedgehog.Classes.Common
import Hedgehog.Classes.Contravariant (contravariantLaws)
import Hedgehog.Classes.Enum (enumLaws, boundedEnumLaws)
import Hedgehog.Classes.Eq (eqLaws)
import Hedgehog.Classes.Foldable (foldableLaws)
import Hedgehog.Classes.Functor (functorLaws)
import Hedgehog.Classes.Generic (genericLaws)
import Hedgehog.Classes.Integral (integralLaws)
--import Hedgehog.Classes.Ix (ixLaws)
import Hedgehog.Classes.Json (jsonLaws)
import Hedgehog.Classes.Monad (monadLaws)
import Hedgehog.Classes.MonadIO (monadIOLaws)
import Hedgehog.Classes.MonadPlus (monadPlusLaws)
import Hedgehog.Classes.MonadZip (monadZipLaws)
import Hedgehog.Classes.Monoid (monoidLaws, commutativeMonoidLaws)
import Hedgehog.Classes.Ord (ordLaws)
import Hedgehog.Classes.Semigroup (semigroupLaws, commutativeSemigroupLaws, exponentialSemigroupLaws, idempotentSemigroupLaws, rectangularBandSemigroupLaws)
import Hedgehog.Classes.Show (showLaws)
import Hedgehog.Classes.ShowRead (showReadLaws)
import Hedgehog.Classes.Storable (storableLaws)
import Hedgehog.Classes.Traversable (traversableLaws)
 
