-----------------------------------------------------------------------------

-- |
-- Module      :  Data.Time.TZDatabase.Olson
-- Copyright   :  Yitzchak Gale 2018
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- A parser and renderer for binary Olson timezone files whose format
-- are specified by the tzfile(5) man page on Unix-like systems. For
-- more information about this format, see
-- <http://www.twinsun.com/tz/tz-link.htm>. Functions are provided for
-- converting the parsed data into @TimeZoneSeries@ and @TimeZone@
-- objects.
{- Copyright (c) 2018 Yitzchak Gale. All rights reserved.
For licensing information, see the BSD3-style license in the file
LICENSE that was originally distributed by the author together with
this file. -}
module Data.Time.TZDatabase.Olson
    ( module Data.Time.TZDatabase.Olson.Parse
    , module Data.Time.TZDatabase.Olson.Render
    , module Data.Time.TZDatabase.Olson.Types
    ) where

import Data.Time.TZDatabase.Olson.Parse
import Data.Time.TZDatabase.Olson.Render
import Data.Time.TZDatabase.Olson.Types
