-- | UNIX-specific handling of time data.
module Data.Time.TZDatabase.Database
    ( TZDatabase(..)
    , tzdGetMaybeFile
    , catchDNEE
    -- * Country Information
    , CountryCode
    , tzdGetCountryCodes
    -- * Zone Information
    , TimeZoneSpec
    , ZoneDescription(..)
    , tzdGetZoneDescriptions
    -- * OlsonData
    , tzdGetOlsonDataForZone
    , tzdGetTimeZoneSeriesForZone
    -- * Leap Seconds
    , tzdGetLeapSecondList
    ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Data.Maybe
import Data.Time.Clock.LeapSeconds
import Data.Time.TZDatabase.Olson
import Data.Time.TZDatabase.Series
import System.IO.Error

separate :: Char -> String -> [String]
separate sep s = let
    (val, rest) = break ((==) sep) s
    vals =
        case rest of
            [] -> []
            _:s' -> separate sep s'
    in val : vals

newtype TZDatabase = MkTZDatabase
    { tzdGetFile :: String -> IO BS.ByteString
    }

catchDNEE :: IO a -> IO (Maybe a)
catchDNEE ioa =
    catchIOError
        (fmap Just ioa)
        (\e ->
             if isDoesNotExistError e
                 then return Nothing
                 else ioError e)

tzdGetMaybeFile :: TZDatabase -> String -> IO (Maybe BS.ByteString)
tzdGetMaybeFile tzd fname = catchDNEE $ tzdGetFile tzd fname

readZoneInfoFile :: TZDatabase -> ([String] -> Maybe a) -> [String] -> IO [a]
readZoneInfoFile tzd toValues filenames = do
    let
        findFirst :: [String] -> IO (String, BS.ByteString)
        findFirst [] = fail $ "could not find " ++ intercalate " " filenames
        findFirst (f:ff) = do
            mbs <- tzdGetMaybeFile tzd f
            case mbs of
                Just bs -> return (f, bs)
                Nothing -> findFirst ff
    (fname, bs) <- findFirst filenames
    let
        readLine ('#':_) = return Nothing
        readLine "" = return Nothing
        readLine s =
            case toValues $ separate '\t' s of
                Just a -> return $ Just a
                Nothing -> fail $ "unexpected line in " ++ fname ++ ": " ++ s
    mstrs <- traverse readLine $ lines $ C.unpack bs
    return $ catMaybes mstrs

-- | ISO 3166 alpha-2 country code (i.e., two capital letters).
type CountryCode = String

-- | Get the country codes and names found in @iso3166.tab@.
-- Note that earlier versions of zoneinfo don't include this file, in which case this will throw exception matching 'isDoesNotExistError'.
tzdGetCountryCodes :: TZDatabase -> IO [(CountryCode, String)]
tzdGetCountryCodes tzd = let
    toCountryCode [code, country] = Just (code, country)
    toCountryCode _ = Nothing
    in readZoneInfoFile tzd toCountryCode ["iso3166.tab"]

-- | Time-zone specification as used in the TZ environment variable, e.g. \"America/Los_Angeles\".
type TimeZoneSpec = String

-- | Information about a zone.
data ZoneDescription = MkZoneDescription
    { zoneCountries :: [CountryCode]
        -- ^ The countries that overlap the zone.
    , zoneLocation :: (Rational, Rational, Bool)
        -- ^ (Latitude, longitude, accuracy) of principal location in degrees, + is north, east.
        -- Accuracy indicates that the location is specified to arcseconds rather than arcminutes.
    , zoneName :: TimeZoneSpec
        -- ^ Zone name.
    , zoneComment :: String
        -- ^ Comments; present if and only if a country has multiple zones.
    }

getSign :: Char -> Maybe Rational
getSign '+' = Just 1
getSign '-' = Just (-1)
getSign _ = Nothing

getDigit :: Char -> Maybe Rational
getDigit c
    | c < '0' = Nothing
getDigit c
    | c > '9' = Nothing
getDigit c = Just $ toRational $ (fromEnum c) - (fromEnum '0')

getDigit2 :: Char -> Char -> Maybe Rational
getDigit2 c1 c2 = do
    r1 <- getDigit c1
    r2 <- getDigit c2
    return $ r1 * 10 + r2

getDigit3 :: Char -> Char -> Char -> Maybe Rational
getDigit3 c1 c2 c3 = do
    r1 <- getDigit2 c1 c2
    r2 <- getDigit c3
    return $ r1 * 10 + r2

parseISO6709 :: String -> Maybe (Rational, Rational, Bool)
parseISO6709 [xsn, xd1, xd2, xm1, xm2, ysn, yd1, yd2, yd3, ym1, ym2] = do
    xsgn <- getSign xsn
    xdeg <- getDigit2 xd1 xd2
    xmin <- getDigit2 xm1 xm2
    ysgn <- getSign ysn
    ydeg <- getDigit3 yd1 yd2 yd3
    ymin <- getDigit2 ym1 ym2
    return (xsgn * (xdeg + xmin / 60), ysgn * (ydeg + ymin / 60), False)
parseISO6709 [xsn, xd1, xd2, xm1, xm2, xs1, xs2, ysn, yd1, yd2, yd3, ym1, ym2, ys1, ys2] = do
    xsgn <- getSign xsn
    xdeg <- getDigit2 xd1 xd2
    xmin <- getDigit2 xm1 xm2
    xsec <- getDigit2 xs1 xs2
    ysgn <- getSign ysn
    ydeg <- getDigit3 yd1 yd2 yd3
    ymin <- getDigit2 ym1 ym2
    ysec <- getDigit2 ys1 ys2
    return (xsgn * (xdeg + xmin / 60 + xsec / 3600), ysgn * (ydeg + ymin / 60 + ysec / 3600), True)
parseISO6709 _ = Nothing

getISO6709 :: String -> (Rational, Rational, Bool)
getISO6709 location =
    case parseISO6709 location of
        Just loc -> loc
        Nothing -> error $ "bad IS06709: " ++ location

-- | Get the zone descriptions found in @zone1970.tab@ (or @zone.tab@).
tzdGetZoneDescriptions :: TZDatabase -> IO [ZoneDescription]
tzdGetZoneDescriptions tzd = let
    toZoneDescription [countries, location, name] =
        Just $
        MkZoneDescription
            { zoneCountries = separate ',' countries
            , zoneLocation = getISO6709 location
            , zoneName = name
            , zoneComment = ""
            }
    toZoneDescription [countries, location, name, comment] =
        Just $
        MkZoneDescription
            { zoneCountries = separate ',' countries
            , zoneLocation = getISO6709 location
            , zoneName = name
            , zoneComment = comment
            }
    toZoneDescription _ = Nothing
    in readZoneInfoFile tzd toZoneDescription ["zone1970.tab", "zone.tab"]

-- | Obtain an 'OlsonData' file for this 'TimeZoneSpec' (or for the system default).
-- If the file is not in the database, return an empty 'OlsonData' with the name in 'olsonPosixTZ'.
tzdGetOlsonDataForZone :: TZDatabase -> TimeZoneSpec -> IO OlsonData
tzdGetOlsonDataForZone tzd name = do
    mbs <- tzdGetMaybeFile tzd name
    return $
        case mbs of
            Just bs -> olsonFromByteString bs
            Nothing -> mempty {olsonPosixTZ = Just name}

-- | Get the 'TimeZoneSeries' for this 'TimeZoneSpec' (or for the system default), defaulting to UTC if the name is not found.
tzdGetTimeZoneSeriesForZone :: TZDatabase -> TimeZoneSpec -> IO TimeZoneSeries
tzdGetTimeZoneSeriesForZone tzd name = do
    odata <- tzdGetOlsonDataForZone tzd name
    return $ olsonToTimeZoneSeries odata

-- | Get the leap-second list found in @leap-seconds.list@.
-- Note that earlier versions of zoneinfo don't include this file, in which case this will throw exception matching 'isDoesNotExistError'.
tzdGetLeapSecondList :: TZDatabase -> IO LeapSecondList
tzdGetLeapSecondList tzd = do
    bs <- tzdGetFile tzd "leap-seconds.list"
    case parseNISTLeapSecondList $ C.unpack bs of
        Just lsl -> return lsl
        Nothing -> fail $ "leap-seconds.list: parse failure"
