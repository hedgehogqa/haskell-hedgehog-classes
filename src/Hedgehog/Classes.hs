{-| This library provides sets of properties that should hold for common
    typeclasses.

    /Note:/ on GHC < 8.5, this library uses the higher-kinded typeclasses
    ('Data.Functor.Classes.Show1', 'Data.Functor.Classes.Eq1', 'Data.Functor.Classes.Ord1', etc.),
    but on GHC >= 8.5, it uses `-XQuantifiedConstraints` to express these
    constraints more cleanly.
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
  , ixLaws
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

    -- * Types
  , Laws(..)
  ) where

import Hedgehog.Classes.Alternative (alternativeLaws)
import Hedgehog.Classes.Applicative (applicativeLaws)
import Hedgehog.Classes.Arrow (arrowLaws)
import Hedgehog.Classes.Bifoldable (bifoldableLaws, bifoldableFunctorLaws)
import Hedgehog.Classes.Bifunctor (bifunctorLaws)
import Hedgehog.Classes.Bitraversable (bitraversableLaws)
import Hedgehog.Classes.Bits (bitsLaws)
import Hedgehog.Classes.Category (categoryLaws, commutativeCategoryLaws)
import Hedgehog.Classes.Common (lawsCheck, lawsCheckOne, lawsCheckMany, Laws(..))
import Hedgehog.Classes.Contravariant (contravariantLaws)
import Hedgehog.Classes.Enum (enumLaws, boundedEnumLaws)
import Hedgehog.Classes.Eq (eqLaws)
import Hedgehog.Classes.Foldable (foldableLaws)
import Hedgehog.Classes.Functor (functorLaws)
import Hedgehog.Classes.Generic (genericLaws)
import Hedgehog.Classes.Integral (integralLaws)
import Hedgehog.Classes.Ix (ixLaws)
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
 
