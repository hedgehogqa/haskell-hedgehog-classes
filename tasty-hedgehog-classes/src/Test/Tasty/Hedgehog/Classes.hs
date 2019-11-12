{-# language
    TypeApplications
  #-}

module Test.Tasty.Hedgehog.Classes
  ( testLaws
  ) where

import Data.Maybe (fromMaybe)
import Data.Proxy
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Options
import qualified Test.Tasty.Providers as T

import Hedgehog
import Hedgehog.Classes

import Hedgehog.Internal.Property
import Hedgehog.Internal.Runner (checkReport)
import Hedgehog.Internal.Report
import Hedgehog.Internal.Seed as Seed

testLaws :: Laws -> T.TestTree
testLaws (Laws tcName laws_props) = testGroup tcName (go laws_props)
  where
    go [] = []
    go ((lawName,prop):lps) =
      T.singleTest lawName (P lawName prop) : go lps

data P = P { _testName :: String, _property :: Property }

instance T.IsTest P where
  testOptions = pure
    [ Option (Proxy @HedgehogReplay)
    , Option (Proxy @HedgehogShowReplay)
    , Option (Proxy @HedgehogTestLimit)
    , Option (Proxy @HedgehogDiscardLimit)
    , Option (Proxy @HedgehogShrinkLimit)
    , Option (Proxy @HedgehogShrinkRetries)
    ]
  run opts (P name (Property pConfig pTest)) yieldProgress = do
    let HedgehogReplay        replay     = lookupOption opts
    let HedgehogShowReplay    showReplay = lookupOption opts
    let HedgehogTestLimit     mTests     = lookupOption opts
    let HedgehogDiscardLimit  mDiscards  = lookupOption opts
    let HedgehogShrinkLimit   mShrinks   = lookupOption opts
    let HedgehogShrinkRetries mRetries   = lookupOption opts
    let config = PropertyConfig
          (fromMaybe (propertyTestLimit pConfig) mTests)
          (fromMaybe (propertyDiscardLimit pConfig) mDiscards)
          (fromMaybe (propertyShrinkLimit pConfig) mShrinks)
          (fromMaybe (propertyShrinkRetries pConfig) mRetries)

    randSeed <- Seed.random
    let size = maybe 0 fst replay
    let seed = maybe randSeed snd replay

    report <- checkReport config size seed pTest (yieldProgress . reportToProgress config)

    let resultFn = if reportStatus report == OK
          then T.testPassed
          else T.testFailed

    out <- reportOutput showReplay name report
    pure $ resultFn out

reportToProgress :: PropertyConfig -> Report Progress -> T.Progress
reportToProgress config (Report testsDone _ _ status) =
  let
    TestLimit testLimit = propertyTestLimit config
    ShrinkLimit shrinkLimit = propertyShrinkLimit config
    ratio x y = 1.0 * fromIntegral x / fromIntegral y
  in
    -- TODO add details for tests run / discarded / shrunk
    case status of
      Running ->
        T.Progress "Running" (ratio testsDone testLimit)
      Shrinking fr ->
        T.Progress "Shrinking" (ratio (failureShrinks fr) shrinkLimit)

reportOutput :: Bool -> String -> Report Result -> IO String
reportOutput showReplay name report = do
  s <- renderResult Nothing (Just (PropertyName name)) report
  case reportStatus report of
    Failed fr -> do
      let size = failureSize fr
      let seed = failureSeed fr
      let replayStr = if showReplay
            then "\nUse '--hedgehog-replay \""
              ++ show size
              ++ " "
              ++ show seed
              ++ "\"' to reproduce."
            else ""
      pure $ s ++ replayStr ++ "\n"
    GaveUp {} -> do
      let DiscardCount discards = reportDiscards report
      pure $ s ++ "   ⚐ <interactive> gave up after "
        ++ show discards
        ++ " discards.\n"
    OK -> do
      pure $ s ++ "   ✓ <interactive> passed 100 tests.\n"
