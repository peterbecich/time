module Main where

import Test.Format.Format
import Test.LocalTime.TZ
import Test.LocalTime.TimeZone
import Test.Tasty

main :: IO ()
main = do
    testTZ <- getTestTZ
    defaultMain $ testGroup "Time" [testGroup "Format" [testFormat], testGroup "LocalTime" [testTimeZone], testTZ]
