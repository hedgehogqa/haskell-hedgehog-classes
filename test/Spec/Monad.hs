module Spec.Monad
  ( testMonad
  , testMonadIO
  , testMonadPlus
  , testMonadZip
  ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))

import Hedgehog
import Hedgehog.Classes

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import System.IO.Unsafe (unsafePerformIO)

import Prelude hiding (either)

testMonad :: [(String, [Laws])]
testMonad =
  [ ("[]", lawsList)
  , ("Either e", lawsEither)
  , ("Identity", lawsIdentity)
  --, ("IO", lawsIO)
  , ("Maybe", lawsMaybe)
  ]

{-
testMonadFix :: [(String, [Laws])]
testMonadFix =
  [ ("[]", fixLawsList)
  , ("Either e", fixLawsEither)
  , ("Identity", fixLawsIdentity)
  , ("IO", fixLawsIO)
  , ("Maybe", fixLawsMaybe)
  ]
-}

testMonadIO :: [(String, [Laws])]
testMonadIO =
  [ ("IO", ioLawsIO)
  ]

testMonadPlus :: [(String, [Laws])]
testMonadPlus =
  [ ("[]", plusLawsList)
  --, ("IO", plusLawsIO)
  , ("Maybe", plusLawsMaybe)
  ]

testMonadZip :: [(String, [Laws])]
testMonadZip =
  [ ("[]", zipLawsList)
  , ("Identity", zipLawsIdentity)
  , ("Maybe", zipLawsMaybe)
  ]

lawsEither :: [Laws]
lawsEither = [monadLaws eitherInteger]

eitherInteger :: MonadGen m => m a -> m (Either Integer a)
eitherInteger = either (Gen.integral (Range.linear 0 20))

either :: MonadGen m => m e -> m a -> m (Either e a)
either genE genA =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, Left <$> genE)
      , (1 + fromIntegral n, Right <$> genA)
      ]

lawsIdentity, zipLawsIdentity :: [Laws]
lawsIdentity = [monadLaws identity]
zipLawsIdentity = [monadZipLaws identity]

identity :: MonadGen m => m a -> m (Identity a)
identity = fmap Identity

lawsList, plusLawsList, zipLawsList :: [Laws]
lawsList = [monadLaws list]
plusLawsList = [monadPlusLaws list]
zipLawsList = [monadZipLaws list]

list :: MonadGen m => m a -> m [a]
list = Gen.list $ Range.linear 0 6

lawsMaybe, plusLawsMaybe, zipLawsMaybe :: [Laws]
lawsMaybe = [monadLaws Gen.maybe]
plusLawsMaybe = [monadPlusLaws Gen.maybe]
zipLawsMaybe = [monadZipLaws Gen.maybe]

lawsIO, ioLawsIO, plusLawsIO :: [Laws]
lawsIO = [monadLaws io]
ioLawsIO = [monadIOLaws io]
plusLawsIO = [monadPlusLaws io]

newtype TestIO a = TestIO (IO a)
-- | Unsafe!
instance Eq a => Eq (TestIO a) where
  TestIO a == TestIO b = unsafePerformIO $ do
    a' <- a
    b' <- b
    return $ a' == b'
-- | Unsafe!
instance Show a => Show (TestIO a) where
  showsPrec d (TestIO a) = unsafePerformIO $ fmap (showsPrec d) a
instance Functor TestIO where
  fmap f (TestIO a) = TestIO $ fmap f a
instance Applicative TestIO where
  pure = TestIO . pure
  TestIO a <*> TestIO b = TestIO $ a <*> b
instance Alternative TestIO where
  empty = TestIO empty
  TestIO a <|> TestIO b = TestIO $ a <|> b
-- | Unsafe!  And also doesn't seem to work...
instance Monad TestIO where
  TestIO a >>= f = f $ unsafePerformIO a
instance MonadIO TestIO where
  liftIO = TestIO
instance MonadPlus TestIO

io :: MonadGen m => m a -> m (TestIO a)
io = fmap return
