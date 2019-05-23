module Test.LocalTime.TZ
    ( getTestTZ
    ) where

import Control.Exception
import Data.Foldable
import Data.Maybe
import Data.Time
import Data.Time.Clock.LeapSeconds
import Data.Time.TZDatabase.Database
import Data.Time.TZDatabase.Series
import Data.Time.TZDatabase.Unix
import System.Directory
import System.Environment
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

testCountryCodes :: TestTree
testCountryCodes =
    testCase "tzdGetCountryCodes" $ do
        codes <- tzdGetCountryCodes unixTZDatabase
        assertBool "at least 200 countries" $ length codes >= 200

testZoneDescriptions :: [ZoneDescription] -> TestTree
testZoneDescriptions zones =
    testCase "tzdGetZoneDescriptions" $ do
        assertBool "at least 300 zones" $ length zones >= 300
        forM_ zones $ \MkZoneDescription {zoneLocation = (lat, long, acc), ..} -> do
            assertBool "zoneCountries length" $ length zoneCountries >= 1
            forM_ zoneCountries $ \zc -> assertEqual "zoneCountries member length" 2 $ length zc
            _ <- evaluate lat
            _ <- evaluate long
            _ <- evaluate acc
            _ <- evaluate zoneName
            _ <- evaluate zoneComment
            return ()

testGetTimeZoneSeries :: Maybe TimeZoneSpec -> TestTree
testGetTimeZoneSeries mname = testCase (fromMaybe "default" mname) $ unixGetTimeZoneSeriesForZone mname >> return ()

testLoadZones :: [TimeZoneSpec] -> TestTree
testLoadZones names =
    testGroup "unixGetTimeZoneSeriesForZone" $ fmap testGetTimeZoneSeries (Nothing : (fmap Just names))

testUnknownZoneName :: TestTree
testUnknownZoneName =
    testCase "unknown TimeZoneSpec" $ do
        setEnv "TZ" "unknown"
        tzs <- unixGetCurrentTimeZoneSeries
        assertEqual "default TimeZoneSpec" (TimeZoneSeries utc []) tzs

testGetLeapSecondList :: TestTree
testGetLeapSecondList =
    testGroup
        "unixGetLeapSecondList"
        [ testCase "values" $ do
              lsl <- unixGetLeapSecondList
              assertBool "expiration is later than version" $ lslExpiration lsl > lslVersion lsl
        , goldenVsFile "leap-seconds" "test/unix/golden/leap-seconds.ref" "test/unix/golden/leap-seconds.out" $ do
              lsl <- unixGetLeapSecondList
              -- just look at the first 26, since the list will grow.
              writeBinaryFile "test/unix/golden/leap-seconds.out" $
                  concat $ fmap (\(d, n) -> show d ++ " " ++ show n ++ "\n") $ take 26 $ lslTransitions lsl
        ]

getTestTZ :: IO TestTree
getTestTZ = do
    ls_exists <- doesFileExist "/usr/share/zoneinfo/leap-seconds.list" -- workaround for Travis, which doesn't have this file
    zones <- tzdGetZoneDescriptions unixTZDatabase
    let names = fmap zoneName zones
    return $
        testGroup "TZ" $
        [testCountryCodes, testZoneDescriptions zones, testLoadZones names, testUnknownZoneName] ++
        if ls_exists
            then [testGetLeapSecondList]
            else []
