{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Hedgehog.Classes.Common.Laws
  ( Laws(..)
  , LawContext(..)

  , lawsCheck
  , lawsCheckOne
  , lawsCheckMany

  , contextualise
  , reduced
  , lawWhere
  , congruency

  , congruent
  , newline
  , tab
  , tab2
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (isSpace)
import Data.Monoid (All(..), Ap(..))
import Hedgehog (Gen)
import Hedgehog.Classes.Common.Property (Context(..))
import Hedgehog.Internal.Property (Property(..))
import Hedgehog.Internal.Region (Region)
import Hedgehog.Internal.Report (Report, Result(..), Progress(..), renderProgress, reportStatus)
import Hedgehog.Internal.Runner (checkReport)
import System.Exit (exitFailure)
import qualified Hedgehog.Classes.Common.PP as PP
import qualified Hedgehog.Internal.Region as Region
import qualified Hedgehog.Internal.Seed as Seed
import qualified System.IO.Silently as S

congruent :: String
congruent = " ≡ "

congruency :: String -> String -> String
congruency x y = x ++ congruent ++ y

newline, tab, tab2 :: String
newline = "\n"
tab = "    "
tab2 = "  "

-- | For footnotes
dagger :: String
dagger = "†"

lawWhere :: [String] -> String
lawWhere [] = []
lawWhere (l:ls) = l ++ newline ++ tab2 ++ lawWhere ls

-- | A 'Laws' is the name of the typeclass and the set of named properties associated with that typeclass.
data Laws = Laws
  { lawsTypeClass :: String
  , lawsProperties :: [(String, Property)]
  }

-- | The context surrounding the property test of a law. Use 'contextualise' to turn this into a 'Context'.
data LawContext = LawContext
  { lawContextLawName :: String -- ^ law name
  , lawContextLawBody :: String -- ^ law body
  , lawContextTcName  :: String -- ^ typeclass name
  , lawContextTcProp  :: String -- ^ how to show the specific property test
  , lawContextReduced :: String -- ^ reduced equation, eg "LHS = RHS" where neither LHS nor RHS are reducible
  }

reduced :: Show a => a -> a -> String
reduced lhs rhs = show lhs ++ congruent ++ show rhs

-- | Turn a 'LawContext' into a 'Context'.
contextualise :: LawContext -> Context
contextualise LawContext{..} = Context $ unlines
  [ "When testing the " ++ lawContextLawName ++ " law(" ++ dagger ++"), for the " ++ lawContextTcName ++ " typeclass, the following test failed: "
  , newline ++ lawContextTcProp
  , newline ++ "The reduced test is: "
  , tab2 ++ lawContextReduced
  , newline ++ "The law in question: "
  , tab2 ++ "(" ++ dagger ++ ") " ++ lawContextLawName ++ " Law: " ++ lawContextLawBody
  ]

-- | A convenience function for testing the properties of a typeclass.
--   For example, in GHCi:
--
-- >>> genOrdering :: Gen Ordering; genOrdering = frequency [(1,pure EQ),(1,pure LT),(1,pure GT)]
-- >>> lawsCheck (monoidLaws genOrdering)
-- Monoid: Left Identity    ✓ <interactive> passed 100 tests.
-- Monoid: Right Identity    ✓ <interactive> passed 100 tests.
-- Monoid: Associativity    ✓ <interactive> passed 100 tests.
-- Monoid: Concatenation    ✓ <interactive> passed 100 tests.
-- True
lawsCheck ::
     Laws -- ^ The 'Laws' you would like to check.
  -> IO Bool -- ^ 'True' if your tests pass, 'False' otherwise.
lawsCheck = fmap getAll . lawsCheckInternal

-- | A convenience function for testing many typeclass instances of
--   a single type.
--
-- >>> lawsCheckOne (word8 constantBounded) [jsonLaws, showReadLaws]
-- ToJSON/FromJSON: Partial Isomorphism    ✓ <interactive> passed 100 tests.
-- ToJSON/FromJSON: Encoding equals value    ✓ <interactive> passed 100 tests.
-- Show/Read: Partial Isomorphism: show/read    ✓ <interactive> passed 100 tests.
-- Show/Read: Partial Isomorphism: show/read with initial space    ✓ <interactive> passed 100 tests.
-- Show/Read: Partial Isomorphism: showsPrec/readsPrec    ✓ <interactive> passed 100 tests.
-- Show/Read: Partial Isomorphism: showList/readList    ✓ <interactive> passed 100 tests.
-- Show/Read: Partial Isomorphism: showListWith shows/readListDefault    ✓ <interactive> passed 100 tests.
-- True
lawsCheckOne ::
     Gen a -- ^ The generator for your type.
  -> [Gen a -> Laws] -- ^ Functions that take a generator and output 'Laws'.
  -> IO Bool -- ^ 'True' if your tests pass. 'False' otherwise.
lawsCheckOne g = fmap getAll . lawsCheckOneInternal g

-- | A convenience function for checking many typeclass instances of
--   multiple types.
--
-- @
-- import Control.Applicative (liftA2)
--
-- import Data.Map (Map)
-- import Data.Set (Set)
--
-- import qualified Data.List as List
-- import qualified Data.Set as Set
-- import qualified Data.Map as Map
--
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
--
-- import Hedgehog (Gen)
-- import Hedgehog.Classes
--
-- -- Generate a small @Set Int@
-- genSet :: Gen (Set Int)
-- genSet = Set.fromList \<$\> (Gen.list (Range.linear 2 10) (Gen.int Range.constantBounded))
--
-- -- Generate a small @Map String Int@
-- genMap :: Gen (Map String Int)
-- genMap = Map.fromList \<$\> (liftA2 List.zip genStrings genInts)
--   where
--     rng = Range.linear 2 6
--     genStrings = Gen.list rng (Gen.string rng Gen.lower)
--     genInts = Gen.list rng (Gen.int Range.constantBounded)
--
-- commonLaws :: (Eq a, Monoid a, Show a) => Gen a -> [Laws]
-- commonLaws p = [eqLaws p, monoidLaws p]
--
-- tests :: [(String, [Laws])]
-- tests =
--   [ ("Set Int", commonLaws genSet)
--   , ("Map String Int", commonLaws genMap)
--   ]
-- @
--
-- Now, in GHCi:
--
-- >>> lawsCheckMany tests
--
-- @
-- Testing properties for common typeclasses...
--
-- -------------
-- -- Set Int --
-- -------------
--
-- Eq: Transitive   ✓ <interactive> passed 100 tests.
-- Eq: Symmetric   ✓ <interactive> passed 100 tests.
-- Eq: Reflexive   ✓ <interactive> passed 100 tests.
-- Eq: Negation   ✓ <interactive> passed 100 tests.
-- Monoid: Left Identity   ✓ <interactive> passed 100 tests.
-- Monoid: Right Identity   ✓ <interactive> passed 100 tests.
-- Monoid: Associativity   ✓ <interactive> passed 100 tests.
-- Monoid: Concatenation   ✓ <interactive> passed 100 tests.
--
-- --------------------
-- -- Map String Int --
-- --------------------
--
-- Eq: Transitive   ✓ <interactive> passed 100 tests.
-- Eq: Symmetric   ✓ <interactive> passed 100 tests.
-- Eq: Reflexive   ✓ <interactive> passed 100 tests.
-- Eq: Negation   ✓ <interactive> passed 100 tests.
-- Monoid: Left Identity   ✓ <interactive> passed 100 tests.
-- Monoid: Right Identity   ✓ <interactive> passed 100 tests.
-- Monoid: Associativity   ✓ <interactive> passed 100 tests.
-- Monoid: Concatenation   ✓ <interactive> passed 100 tests.
--
-- All tests succeeded
-- True
-- @
lawsCheckMany ::
     [(String, [Laws])] -- ^ Pairs of type names and their associated laws to test.
  -> IO Bool -- ^ 'True' if your tests pass. 'False' otherwise.
lawsCheckMany = fmap getAll . lawsCheckManyInternal

lawsCheckInternal :: Laws -> IO All
lawsCheckInternal (Laws className properties) =
  flip foldMapA properties $ \(name,p) -> do
    putStr (className ++ ": " ++ name ++ " ")
    (out,b) <- S.capture $ check p
    if b
      then putStr "   ✓ <interactive> passed 100 tests.\n"
      else putStr $ (removeBadOutput out) <> "\n"
    pure (All b)

lawsCheckOneInternal :: Gen a -> [Gen a -> Laws] -> IO All
lawsCheckOneInternal p ls = foldMap (lawsCheckInternal . ($ p)) ls

lawsCheckManyInternal :: [(String, [Laws])] -> IO All
lawsCheckManyInternal xs = do
  putStrLn ""
  putStrLn "Testing properties for common typeclasses..."
  putStrLn ""
  r <- flip foldMapA xs $ \(typeName, laws) -> do
    putStrLn $ prettyHeader typeName
    r <- flip foldMapA laws $ \(Laws typeclassName properties) -> do
      flip foldMapA properties $ \(name,p) -> do
        putStr (typeclassName ++ ": " ++ name)
        (out,b) <- S.capture $ check p
        if b
          then putStr "   ✓ <interactive> passed 100 tests.\n"
          else putStr $ (removeBadOutput out) <> "\n"
        pure (boolToStatus b)
    putStrLn ""
    pure r
  putStrLn ""
  case r of
    Good -> putStrLn "All tests succeeded" *> pure mempty
    Bad  -> do
      putStrLn "One or more tests failed"
      exitFailure

foldMapA :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldMapA f = getAp . foldMap (Ap . f)

prettyHeader :: String -> String
prettyHeader s = unlines [topLine, middleLine, bottomLine]
  where
    line = replicate (length s + 6) '-'
    topLine = line
    bottomLine = line
    middleLine = "-- " ++ s ++ " --"

data Status = Bad | Good

instance Semigroup Status where
  Good <> x = x
  Bad <> _ = Bad

instance Monoid Status where
  mempty = Good

boolToStatus :: Bool -> Status
boolToStatus = \case { False -> Bad; True -> Good; }

checkRegion :: MonadIO m
  => Region
  -> Property
  -> m (Report Result)
checkRegion region prop = liftIO $ do
  seed <- liftIO Seed.random
  result <- checkReport (propertyConfig prop) 0 seed (propertyTest prop) $ \progress -> do
    ppprogress <- renderProgress Nothing Nothing progress
    case reportStatus progress of
      Running -> Region.setRegion region ppprogress
      Shrinking _ -> Region.openRegion region ppprogress
  ppresult <- PP.renderResult result
  case reportStatus result of
    Failed _ -> Region.openRegion region ppresult
    GaveUp -> Region.openRegion region ppresult
    OK -> Region.setRegion region ppresult
  pure result

check :: MonadIO m
  => Property
  -> m Bool
check prop = liftIO . Region.displayRegion $ \region ->
  (== OK) . reportStatus <$> checkRegion region prop

-- HACK!
-- BAD!
-- ALERT!

stripLeading :: String -> String
stripLeading = \case
  [] -> []
  s@(x:xs) -> if isSpace x
    then stripLeading xs
    else s

-- | Like 'Data.Functor.Contravariant.Predicate', but its
--   Semigroup/Monoid instances are disjunctive instead of
--   conjunctive.
newtype DPredicate a = DPredicate { getDPredicate :: a -> Bool }
instance Semigroup (DPredicate a) where
  DPredicate p <> DPredicate q = DPredicate $ \a -> p a || q a
instance Monoid (DPredicate a) where
  mempty = DPredicate $ const False

startsWithCorner :: DPredicate String
startsWithCorner = DPredicate $ \case
  [] -> False
  (x:_) -> x == '┏'

containsBar :: DPredicate String
containsBar = DPredicate $ \s -> any (== '┃') s

isBad :: String -> Bool
isBad = getDPredicate $ mconcat
  [ startsWithCorner
  , containsBar
  ]

removeBadOutput :: String -> String
removeBadOutput = unlines . go . lines where
  go [] = []
  go (x:xs) = if isBad (stripLeading x)
    then go xs
    else x : go xs
