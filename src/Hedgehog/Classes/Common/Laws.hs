{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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

data Laws = Laws
  { lawsTypeClass :: String
  , lawsProperties :: [(String, Property)]
  }

data LawContext = LawContext -- forall b. LawContext
  { lawContextLawName :: String -- ^ law name
  , lawContextLawBody :: String -- ^ law body
  , lawContextTcName  :: String -- ^ typeclass name
  , lawContextTcProp  :: String -- ^ how to show the specific property test
  , lawContextReduced :: String -- ^ reduced equation, eg "LHS = RHS" where neither LHS nor RHS are reducible
  }

reduced :: Show a => a -> a -> String
reduced lhs rhs = show lhs ++ congruent ++ show rhs

contextualise :: LawContext -> Context
contextualise LawContext{..} = Context $ unlines 
  [ "When testing the " ++ lawContextLawName ++ " law(" ++ dagger ++"), for the " ++ lawContextTcName ++ " typeclass, the following test failed: "
  , newline ++ lawContextTcProp
  , newline ++ "The reduced test is: "
  , tab2 ++ lawContextReduced
  , newline ++ "The law in question: "
  , tab2 ++ "(" ++ dagger ++ ") " ++ lawContextLawName ++ " Law: " ++ lawContextLawBody
  ]

lawsCheck :: Laws -> IO Bool
lawsCheck = fmap getAll . lawsCheckInternal

lawsCheckOne :: Gen a -> [Gen a -> Laws] -> IO Bool
lawsCheckOne g = fmap getAll . lawsCheckOneInternal g

lawsCheckMany :: [(String, [Laws])] -> IO Bool
lawsCheckMany = fmap getAll . lawsCheckManyInternal

lawsCheckInternal :: Laws -> IO All
lawsCheckInternal (Laws className properties) =
  flip foldMapA properties $ \(name,p) -> do
    putStr (className ++ ": " ++ name ++ " ")
    All <$> check p

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
        putStr (typeclassName ++ ": " ++ name ++ " ")
        check p >>= \case { True -> pure Good; _ -> pure Bad }
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

