{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2024 Edward F. Behn, Jr.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

#{include "plhaskell.h"}

module PGdatetime (Date, Time, TimeTZ, Timestamp, TimestampTZ, Interval) where

import Data.Functor ((<&>))
import Data.Int (Int32, Int64)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CBool (CBool))
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr (Ptr, WordPtr (WordPtr))
import Foreign.Storable (Storable (alignment, peek, poke, sizeOf), peekByteOff, pokeByteOff)
import PGcommon (Datum (Datum), pWithCString, palloc, pallocArray, palloca)
import PGsupport (BaseType (read, write))
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadPrec (ReadPrec, readS_to_Prec)
import Text.Read (parens, readPrec)
import Prelude (IO, Read, Show (show), const, id, maxBound, minBound, otherwise, return, ($), (+), (==), (>>=))

foreign import capi safe "plhaskell.h DatumGetPointer"
  datumGetPointer :: Datum -> IO (Ptr a)

foreign import capi safe "plhaskell.h PointerGetDatum"
  pointerGetDatum :: Ptr a -> IO Datum

mkReadPrec :: (Storable a) => (Ptr a -> CString -> IO CBool) -> (a -> b) -> ReadPrec b
mkReadPrec readFunc convert = parens $ readS_to_Prec $ const parse
  where
    parse "" = []
    parse ('(' : _) = []
    parse ('[' : _) = []
    parse s = unsafePerformIO $ do
      palloca $
        \pObj -> do
          pWithCString (beforeClose s) $ \buf -> do
            resp <- readFunc pObj buf
            if (toBool resp)
              then do
                obj <- peek pObj
                return [(convert obj, afterClose s)]
              else return []
    beforeClose "" = ""
    beforeClose (')' : _) = ""
    beforeClose (']' : _) = ""
    beforeClose (x : xs) = x : beforeClose xs
    afterClose "" = ""
    afterClose x@(')' : _) = x
    afterClose x@(']' : _) = x
    afterClose (_ : xs) = afterClose xs

data Date
  = DateNInfinity
  | Date Int32
  | DatePInfinity

numToDate :: Int32 -> Date
numToDate date
  | date == minBound = DateNInfinity
  | date == maxBound = DatePInfinity
  | otherwise = Date date

foreign import capi safe "plhaskell.h DatumGetDateADT"
  datumGetDateADT :: Datum -> IO Int32

foreign import capi safe "plhaskell.h DateADTGetDatum"
  dateADTGetDatum :: Int32 -> IO Datum

instance BaseType Date where
  read datum = datumGetDateADT datum <&> numToDate

  write DateNInfinity = dateADTGetDatum minBound
  write (Date date) = dateADTGetDatum date
  write DatePInfinity = dateADTGetDatum maxBound

foreign import capi safe "plhaskell.h date_read"
  dateRead :: Ptr Int32 -> CString -> IO CBool

instance Read Date where
  readPrec = mkReadPrec dateRead numToDate

foreign import capi safe "plhaskell.h date_show"
  dateShow :: Int32 -> CString -> IO ()

instance Show Date where
  show DateNInfinity = "-infinity"
  show DatePInfinity = "infinity"
  show (Date date) = unsafePerformIO $
    pallocArray (#{const MAXDATELEN} + 1) $
      \buf -> do
        dateShow date buf
        peekCString buf

data Time = Time Int64

foreign import capi safe "plhaskell.h DatumGetTimeADT"
  datumGetTimeADT :: Datum -> IO Int64

foreign import capi safe "plhaskell.h TimeADTGetDatum"
  timeADTGetDatum :: Int64 -> IO Datum

instance BaseType Time where
  read datum = datumGetTimeADT datum <&> Time
  write (Time time) = timeADTGetDatum time

foreign import capi safe "plhaskell.h time_read"
  timeRead :: Ptr Int64 -> CString -> IO CBool

instance Read Time where
  readPrec = mkReadPrec timeRead Time

foreign import capi safe "plhaskell.h time_show"
  timeShow :: Int64 -> CString -> IO ()

instance Show Time where
  show (Time time) = unsafePerformIO $
    pallocArray (#{const MAXDATELEN} + 1) $
      \buf -> do
        timeShow time buf
        peekCString buf

data TimeTZ = TimeTZ Int64 Int32

instance BaseType TimeTZ where
  read datum = datumGetPointer datum >>= peek
  write timeTZ = do
    pTimeTZ <- palloc #{size TimeTzADT}
    poke pTimeTZ timeTZ
    pointerGetDatum pTimeTZ

instance Storable TimeTZ where
  sizeOf _ = #{size TimeTzADT}
  alignment _ = #{alignment TimeTzADT}

  peek pTimeTZ = do
    time <- #{peek TimeTzADT, time} pTimeTZ
    zone <- #{peek TimeTzADT, zone} pTimeTZ
    return $ TimeTZ time zone

  poke pTimeTZ (TimeTZ time zone) = do
    #{poke TimeTzADT, time} pTimeTZ time
    #{poke TimeTzADT, zone} pTimeTZ zone

foreign import capi safe "plhaskell.h timetz_read"
  timeTZRead :: Ptr TimeTZ -> CString -> IO CBool

instance Read TimeTZ where
  readPrec = mkReadPrec timeTZRead id

foreign import capi safe "plhaskell.h timetz_show"
  timeTZShow :: Ptr TimeTZ -> CString -> IO ()

instance Show TimeTZ where
  show timeTZ = unsafePerformIO $
    palloca $
      \pTimeTZ -> pallocArray (#{const MAXDATELEN} + 1) $
        \buf -> do
          poke pTimeTZ timeTZ
          timeTZShow pTimeTZ buf
          peekCString buf

data Timestamp
  = TimestampNInfinity
  | Timestamp Int64
  | TimestampPInfinity

numToTimestamp :: Int64 -> Timestamp
numToTimestamp timestamp
  | timestamp == minBound = TimestampNInfinity
  | timestamp == maxBound = TimestampPInfinity
  | otherwise = Timestamp timestamp

foreign import capi safe "plhaskell.h DatumGetTimestamp"
  datumGetTimestamp :: Datum -> IO Int64

foreign import capi safe "plhaskell.h TimestampGetDatum"
  timestampGetDatum :: Int64 -> IO Datum

instance BaseType Timestamp where
  read datum = datumGetTimestamp datum <&> numToTimestamp

  write TimestampNInfinity = timestampGetDatum minBound
  write (Timestamp timestamp) = timestampGetDatum timestamp
  write TimestampPInfinity = timestampGetDatum maxBound

foreign import capi safe "plhaskell.h timestamp_read"
  timestampRead :: Ptr Int64 -> CString -> IO CBool

instance Read Timestamp where
  readPrec = mkReadPrec timestampRead numToTimestamp

foreign import capi safe "plhaskell.h timestamp_show"
  timestampShow :: Int64 -> CString -> IO ()

instance Show Timestamp where
  show TimestampNInfinity = "-infinity"
  show TimestampPInfinity = "infinity"
  show (Timestamp timestamp) = unsafePerformIO $
    pallocArray (#{const MAXDATELEN} + 1) $
      \buf -> do
        timestampShow timestamp buf
        peekCString buf

data TimestampTZ
  = TimestampTZNInfinity
  | TimestampTZ Int64
  | TimestampTZPInfinity

numToTimestampTZ :: Int64 -> TimestampTZ
numToTimestampTZ timestamptz
  | timestamptz == minBound = TimestampTZNInfinity
  | timestamptz == maxBound = TimestampTZPInfinity
  | otherwise = TimestampTZ timestamptz

foreign import capi safe "plhaskell.h DatumGetTimestampTz"
  datumGetTimestampTZ :: Datum -> IO Int64

foreign import capi safe "plhaskell.h TimestampTzGetDatum"
  timestampTZGetDatum :: Int64 -> IO Datum

instance BaseType TimestampTZ where
  read datum = datumGetTimestampTZ datum <&> numToTimestampTZ

  write TimestampTZNInfinity = timestampTZGetDatum minBound
  write (TimestampTZ timestamptz) = timestampTZGetDatum timestamptz
  write TimestampTZPInfinity = timestampTZGetDatum maxBound

foreign import capi safe "plhaskell.h timestamptz_read"
  timestampTZRead :: Ptr Int64 -> CString -> IO CBool

instance Read TimestampTZ where
  readPrec = mkReadPrec timestampTZRead numToTimestampTZ

foreign import capi safe "plhaskell.h timestamptz_show"
  timestampTZShow :: Int64 -> CString -> IO ()

instance Show TimestampTZ where
  show TimestampTZNInfinity = "-infinity"
  show TimestampTZPInfinity = "infinity"
  show (TimestampTZ timestamptz) = unsafePerformIO $
    pallocArray (#{const MAXDATELEN} + 1) $
      \buf -> do
        timestampTZShow timestamptz buf
        peekCString buf

data Interval = Interval Int64 Int32 Int32

instance BaseType Interval where
  read datum = datumGetPointer datum >>= peek
  write interval = do
    pInterval <- palloc #{size Interval}
    poke pInterval interval
    pointerGetDatum pInterval

instance Storable Interval where
  sizeOf _ = #{size Interval}
  alignment _ = #{alignment Interval}

  peek pInterval = do
    time <- #{peek Interval, time} pInterval
    day <- #{peek Interval, day} pInterval
    month <- #{peek Interval, month} pInterval
    return $ Interval time day month

  poke pInterval (Interval time day month) = do
    #{poke Interval, time} pInterval time
    #{poke Interval, day} pInterval day
    #{poke Interval, month} pInterval month

foreign import capi safe "plhaskell.h interval_read"
  intervalRead :: Ptr Interval -> CString -> IO CBool

instance Read Interval where
  readPrec = mkReadPrec intervalRead id

foreign import capi safe "plhaskell.h interval_show"
  intervalShow :: Ptr Interval -> CString -> IO ()

instance Show Interval where
  show interval = unsafePerformIO $
    palloca $
      \pInterval -> pallocArray (#{const MAXDATELEN} + 1) $
        \buf -> do
          poke pInterval interval
          intervalShow pInterval buf
          peekCString buf
