-- | Parse an Olson timezone file, then render it.
-- If -i is specified, interpret it as a TimeZoneSeries before rendering.
module Main
    ( main
    ) where

import Data.Time.TZDatabase.Olson
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if head args == "-i"
        then getTimeZoneSeriesFromOlsonFile (args !! 1) >>= renderTimeZoneSeriesToOlsonFile (args !! 2)
        else getOlsonFromFile (args !! 0) >>= renderOlsonToFile (args !! 1)
