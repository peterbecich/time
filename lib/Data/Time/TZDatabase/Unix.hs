-- | UNIX-specific handling of time data.
module Data.Time.TZDatabase.Unix
    ( unixTZDatabase
    , unixGetCurrentTimeZoneSeries
    , unixGetTimeZoneSeriesForZone
    , unixGetLeapSecondList
    ) where

import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock.LeapSeconds
import Data.Time.TZDatabase.Database
import Data.Time.TZDatabase.Olson
import Data.Time.TZDatabase.Series
import System.Environment

-- http://www.pathname.com/fhs/pub/fhs-2.3.html#SPECIFICOPTIONS15
-- http://askubuntu.com/questions/118015/where-is-usr-share-zoneinfo-specified
unixTZFilePath :: String -> FilePath
unixTZFilePath fname = "/usr/share/zoneinfo/" ++ fname

unixTZDatabase :: TZDatabase
unixTZDatabase = MkTZDatabase $ \fname -> BS.readFile $ unixTZFilePath fname

unixGetTZSpecFile :: Maybe TimeZoneSpec -> IO (Maybe BS.ByteString)
unixGetTZSpecFile Nothing = catchDNEE $ BS.readFile "/etc/localtime"
unixGetTZSpecFile (Just (':':name)) = tzdGetMaybeFile unixTZDatabase name
unixGetTZSpecFile (Just name) = tzdGetMaybeFile unixTZDatabase name

-- | Get the 'TimeZoneSeries' for this 'TimeZoneSpec' (or for the system default), defaulting to UTC if the name is not found.
unixGetTimeZoneSeriesForZone :: Maybe TimeZoneSpec -> IO TimeZoneSeries
unixGetTimeZoneSeriesForZone mname = do
    mbs <- unixGetTZSpecFile mname
    return $
        olsonToTimeZoneSeries $
        case mbs of
            Just bs -> olsonFromByteString bs
            Nothing -> mempty {olsonPosixTZ = mname}

-- | Get the current 'TimeZoneSeries' (as specifed by @TZ@ env-var or else the system default).
unixGetCurrentTimeZoneSeries :: IO TimeZoneSeries
unixGetCurrentTimeZoneSeries = do
    mtzvar <- lookupEnv "TZ"
    unixGetTimeZoneSeriesForZone mtzvar

unixGetLeapSecondList :: IO LeapSecondList
unixGetLeapSecondList = tzdGetLeapSecondList unixTZDatabase
