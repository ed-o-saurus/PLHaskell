{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2026 Edward F. Behn, Jr.
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

#{include "postgres.h"}
#{include "utils/date.h"}
#{include "utils/datetime.h"}

module PGdatetime
  ( Date
      ( DateNInfinity,
        DatePInfinity
      ),
    Time,
    Timestamp
      ( TimestampNInfinity,
        TimestampPInfinity
      ),
    Interval,
    Weekday (..),
    Month (..),
    HasDate
      ( year,
        month,
        day,
        dayOfWeek,
        dayOfYear,
        iso
      ),
    HasTime
      ( hour,
        minute,
        second,
        microsecond
      ),
    IntervalDiff
      ( addInterval,
        subInterval,
        diff
      ),
    mkDate,
    mkTime,
    mkTimestamp,
    mkInterval,
    combineTimestamp,
    separateTimestamp,
    dateMinusDate,
    dateMinusInt,
    datePlusInt,
    doubleTimesInterval,
    dateMinusInterval,
    datePlusInterval,
    years,
    months,
    days,
    hours,
    minutes,
    seconds,
    microseconds,
    transactionTimestampUTC',
    statementTimestampUTC',
    clockTimestampUTC',
  )
where

import Control.Exception
  ( Exception,
    throw,
  )
import Data.Functor
  ( (<$>),
  )
import Data.Int
  ( Int32,
    Int64,
  )
import Data.Text
  ( Text,
  )
import Foreign.C.String
  ( CString,
    peekCString,
  )
import Foreign.C.Types
  ( CBool
      ( CBool
      ),
  )
import Foreign.Marshal.Utils
  ( toBool,
  )
import Foreign.Ptr
  ( Ptr,
  )
import Foreign.Storable
  ( Storable
      ( alignment,
        peek,
        poke,
        sizeOf
      ),
    peekByteOff,
    pokeByteOff,
  )
import PGcommon
  ( Datum
      ( Datum
      ),
    Oid,
    pWithCString,
    palloc,
    pallocArray,
    palloca,
  )
import PGsupport
  ( BaseType
      ( read,
        write
      ),
    callFunc1,
    callFunc2,
    callFunc3,
    callFunc6,
    callFunc7,
  )
import System.IO.Unsafe
  ( unsafePerformIO,
  )
import Text.ParserCombinators.ReadPrec
  ( ReadPrec,
    readS_to_Prec,
  )
import Text.Read
  ( parens,
    readPrec,
  )
import Prelude
  ( Bool
      ( False,
        True
      ),
    Double,
    Enum
      ( fromEnum,
        pred,
        succ,
        toEnum
      ),
    Eq,
    IO,
    Integer,
    Maybe
      ( Just
      ),
    Ord,
    Read,
    Show
      ( show
      ),
    const,
    fromIntegral,
    id,
    maxBound,
    maybe,
    minBound,
    mod,
    otherwise,
    quot,
    rem,
    return,
    round,
    undefined,
    ($),
    (*),
    (+),
    (/),
    (<=),
    (==),
    (>>=),
  )

foreign import capi safe "postgres.h DatumGetPointer"
  datumGetPointer :: Datum -> IO (Ptr a)

foreign import capi safe "postgres.h PointerGetDatum"
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

foreign import capi safe "utils/datetime.h DatumGetDateADT"
  datumGetDateADT :: Datum -> IO Int32

foreign import capi safe "utils/datetime.h DateADTGetDatum"
  dateADTGetDatum :: Int32 -> IO Datum

instance BaseType Date where
  read datum = numToDate <$> datumGetDateADT datum

  write DateNInfinity = dateADTGetDatum minBound
  write (Date date) = dateADTGetDatum date
  write DatePInfinity = dateADTGetDatum maxBound

foreign import capi safe "datetime_plh.h date_read"
  dateRead :: Ptr Int32 -> CString -> IO CBool

instance Read Date where
  readPrec = mkReadPrec dateRead numToDate

foreign import capi safe "datetime_plh.h date_show"
  dateShow :: Int32 -> CString -> IO ()

instance Show Date where
  show DateNInfinity = "-infinity"
  show DatePInfinity = "infinity"
  show (Date date) = unsafePerformIO $
    pallocArray (#{const MAXDATELEN} + 1) $
      \buf -> do
        dateShow date buf
        peekCString buf

instance Eq Date where
  DateNInfinity == DateNInfinity = True
  DatePInfinity == DatePInfinity = True
  (Date date1) == (Date date2) = (date1 == date2)
  _date1 == _date2 = False

instance Ord Date where
  DateNInfinity <= DatePInfinity = True
  (Date _date) <= DatePInfinity = True
  DateNInfinity <= (Date _date) = True
  (Date date1) <= (Date date2) = (date1 <= date2)
  _date1 <= _date2 = False

data DateInfiniteEnumException = DateInfiniteEnumException
  deriving (Show)

instance Exception DateInfiniteEnumException

instance Enum Date where
  fromEnum DateNInfinity = throw DateInfiniteEnumException
  fromEnum DatePInfinity = throw DateInfiniteEnumException
  fromEnum (Date date) = fromIntegral date

  toEnum date = Date $ fromIntegral date

  succ DateNInfinity = DateNInfinity
  succ DatePInfinity = DatePInfinity
  succ (Date date) = Date (succ date)

  pred DateNInfinity = DateNInfinity
  pred DatePInfinity = DatePInfinity
  pred (Date date) = Date (pred date)

data Time = Time Int64

foreign import capi safe "utils/date.h DatumGetTimeADT"
  datumGetTimeADT :: Datum -> IO Int64

foreign import capi safe "utils/date.h TimeADTGetDatum"
  timeADTGetDatum :: Int64 -> IO Datum

instance BaseType Time where
  read datum = Time <$> datumGetTimeADT datum
  write (Time time) = timeADTGetDatum time

foreign import capi safe "datetime_plh.h time_read"
  timeRead :: Ptr Int64 -> CString -> IO CBool

instance Read Time where
  readPrec = mkReadPrec timeRead Time

foreign import capi safe "datetime_plh.h time_show"
  timeShow :: Int64 -> CString -> IO ()

instance Show Time where
  show (Time time) = unsafePerformIO $
    pallocArray (#{const MAXDATELEN} + 1) $
      \buf -> do
        timeShow time buf
        peekCString buf

instance Eq Time where
  (Time time1) == (Time time2) = time1 == time2

instance Ord Time where
  (Time time1) <= (Time time2) = time1 <= time2

data Timestamp
  = TimestampNInfinity
  | Timestamp Int64
  | TimestampPInfinity

numToTimestamp :: Int64 -> Timestamp
numToTimestamp timestamp
  | timestamp == minBound = TimestampNInfinity
  | timestamp == maxBound = TimestampPInfinity
  | otherwise = Timestamp timestamp

foreign import capi safe "utils/timestamp.h DatumGetTimestamp"
  datumGetTimestamp :: Datum -> IO Int64

foreign import capi safe "utils/timestamp.h TimestampGetDatum"
  timestampGetDatum :: Int64 -> IO Datum

instance BaseType Timestamp where
  read datum = numToTimestamp <$> datumGetTimestamp datum

  write TimestampNInfinity = timestampGetDatum minBound
  write (Timestamp timestamp) = timestampGetDatum timestamp
  write TimestampPInfinity = timestampGetDatum maxBound

foreign import capi safe "datetime_plh.h timestamp_read"
  timestampRead :: Ptr Int64 -> CString -> IO CBool

instance Read Timestamp where
  readPrec = mkReadPrec timestampRead numToTimestamp

foreign import capi safe "datetime_plh.h timestamp_show"
  timestampShow :: Int64 -> CString -> IO ()

instance Show Timestamp where
  show TimestampNInfinity = "-infinity"
  show TimestampPInfinity = "infinity"
  show (Timestamp timestamp) = unsafePerformIO $
    pallocArray (#{const MAXDATELEN} + 1) $
      \buf -> do
        timestampShow timestamp buf
        peekCString buf

instance Eq Timestamp where
  TimestampNInfinity == TimestampNInfinity = True
  TimestampPInfinity == TimestampPInfinity = True
  (Timestamp timestamp1) == (Timestamp timestamp2) = (timestamp1 == timestamp2)
  _timestamp1 == _timestamp2 = False

instance Ord Timestamp where
  TimestampNInfinity <= TimestampPInfinity = True
  (Timestamp _timestamp) <= TimestampPInfinity = True
  TimestampNInfinity <= (Timestamp _timestamp) = True
  (Timestamp timestamp1) <= (Timestamp timestamp2) = (timestamp1 <= timestamp2)
  _timestamp1 <= _timestamp2 = False

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
    day' <- #{peek Interval, day} pInterval
    month' <- #{peek Interval, month} pInterval
    return $ Interval time day' month'

  poke pInterval (Interval time day' month') = do
    #{poke Interval, time} pInterval time
    #{poke Interval, day} pInterval day'
    #{poke Interval, month} pInterval month'

foreign import capi safe "datetime_plh.h interval_read"
  intervalRead :: Ptr Interval -> CString -> IO CBool

instance Read Interval where
  readPrec = mkReadPrec intervalRead id

foreign import capi safe "datetime_plh.h interval_show"
  intervalShow :: Ptr Interval -> CString -> IO ()

instance Show Interval where
  show interval = unsafePerformIO $
    palloca $
      \pInterval -> pallocArray (#{const MAXDATELEN} + 1) $
        \buf -> do
          poke pInterval interval
          intervalShow pInterval buf
          peekCString buf

duration :: Interval -> Integer
duration (Interval time day' month') = (fromIntegral time) + 86_400_000_000 * ((fromIntegral day') + 30 * (fromIntegral month'))

instance Eq Interval where
  interval1 == interval2 = (duration interval1) == (duration interval2)

instance Ord Interval where
  interval1 <= interval2 = (duration interval1) <= (duration interval2)

foreign import ccall safe "access/xact.h GetCurrentTransactionStartTimestamp"
  cTransactionTimestampUTC :: IO Int64

transactionTimestampUTC' :: IO Timestamp
transactionTimestampUTC' = Timestamp <$> cTransactionTimestampUTC

foreign import ccall safe "access/xact.h GetCurrentStatementStartTimestamp"
  cStatementTimestampUTC :: IO Int64

statementTimestampUTC' :: IO Timestamp
statementTimestampUTC' = Timestamp <$> cStatementTimestampUTC

foreign import ccall safe "utils/timestamp.h GetCurrentTimestamp"
  cGetCurrentTimestampUTC :: IO Int64

clockTimestampUTC' :: IO Timestamp
clockTimestampUTC' = Timestamp <$> cGetCurrentTimestampUTC

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq)

instance Enum Month where
  fromEnum Jan = 1
  fromEnum Feb = 2
  fromEnum Mar = 3
  fromEnum Apr = 4
  fromEnum May = 5
  fromEnum Jun = 6
  fromEnum Jul = 7
  fromEnum Aug = 8
  fromEnum Sep = 9
  fromEnum Oct = 10
  fromEnum Nov = 11
  fromEnum Dec = 12

  toEnum x = toEnum' $ mod x 12
    where
      toEnum' 1 = Jan
      toEnum' 2 = Feb
      toEnum' 3 = Mar
      toEnum' 4 = Apr
      toEnum' 5 = May
      toEnum' 6 = Jun
      toEnum' 7 = Jul
      toEnum' 8 = Aug
      toEnum' 9 = Sep
      toEnum' 10 = Oct
      toEnum' 11 = Nov
      toEnum' 0 = Dec
      toEnum' _ = undefined

  succ Jan = Feb
  succ Feb = Mar
  succ Mar = Apr
  succ Apr = May
  succ May = Jun
  succ Jun = Jul
  succ Jul = Aug
  succ Aug = Sep
  succ Sep = Oct
  succ Oct = Nov
  succ Nov = Dec
  succ Dec = Jan

  pred Jan = Dec
  pred Feb = Jan
  pred Mar = Feb
  pred Apr = Mar
  pred May = Apr
  pred Jun = May
  pred Jul = Jun
  pred Aug = Jul
  pred Sep = Aug
  pred Oct = Sep
  pred Nov = Oct
  pred Dec = Nov

data Weekday = Sun | Mon | Tue | Wen | Thu | Fri | Sat deriving (Eq)

instance Enum Weekday where
  fromEnum Sun = 0
  fromEnum Mon = 1
  fromEnum Tue = 2
  fromEnum Wen = 3
  fromEnum Thu = 4
  fromEnum Fri = 5
  fromEnum Sat = 6

  toEnum x = toEnum' $ mod x 7
    where
      toEnum' 0 = Sun
      toEnum' 1 = Mon
      toEnum' 2 = Tue
      toEnum' 3 = Wen
      toEnum' 4 = Thu
      toEnum' 5 = Fri
      toEnum' 6 = Sat
      toEnum' _ = undefined

  succ Sun = Mon
  succ Mon = Tue
  succ Tue = Wen
  succ Wen = Thu
  succ Thu = Fri
  succ Fri = Sat
  succ Sat = Sun

  pred Sun = Sat
  pred Mon = Sun
  pred Tue = Mon
  pred Wen = Tue
  pred Thu = Wen
  pred Fri = Thu
  pred Sat = Fri

mkSecs :: Int32 -> Int32 -> Double
mkSecs sec usec = (fromIntegral sec) + (fromIntegral usec) / 1_000_000

foreign import ccall safe "&make_date_oid"
  pMakeDateOid :: Ptr Oid

mkDate :: Int32 -> Month -> Int32 -> Date
mkDate year' month' day' = unsafePerformIO $ callFunc3 pMakeDateOid (Just year') (Just ((fromIntegral $ fromEnum month') :: Int32)) (Just day') >>= maybe undefined return

foreign import ccall safe "&make_time_oid"
  pMakeTimeOid :: Ptr Oid

mkTime :: Int32 -> Int32 -> Int32 -> Int32 -> Time
mkTime hour' minute' second' microsecond' = unsafePerformIO $ callFunc3 pMakeTimeOid (Just hour') (Just minute') (Just $ mkSecs second' microsecond') >>= maybe undefined return

foreign import ccall safe "&make_timestamp_oid"
  pMakeTimestampOid :: Ptr Oid

mkTimestamp :: Int32 -> Month -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Timestamp
mkTimestamp year' month' day' hour' minute' second' microsecond' = unsafePerformIO $ callFunc6 pMakeTimestampOid (Just year') (Just ((fromIntegral $ fromEnum month') :: Int32)) (Just day') (Just hour') (Just minute') (Just $ mkSecs second' microsecond') >>= maybe undefined return

foreign import ccall safe "&make_interval_oid"
  pMakeIntervalOid :: Ptr Oid

mkInterval :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Interval
mkInterval years' months' weeks' days' hours' minutes' seconds' microseconds' = unsafePerformIO $ callFunc7 pMakeIntervalOid (Just years') (Just months') (Just weeks') (Just days') (Just hours') (Just minutes') (Just $ mkSecs seconds' microseconds') >>= maybe undefined return

class HasDate a where
  datePartForDate :: Text -> a -> Double

  year :: a -> Int32
  year val = round $ datePartForDate "year" val

  month :: a -> Month
  month val = toEnum $ round $ datePartForDate "month" val

  day :: a -> Int32
  day val = round $ datePartForDate "day" val

  dayOfWeek :: a -> Weekday
  dayOfWeek val = toEnum $ round $ datePartForDate "dow" val

  dayOfYear :: a -> Int32
  dayOfYear val = round $ datePartForDate "doy" val

  iso :: a -> (Int32, Int32)
  iso val = (round $ datePartForDate "isoyear" val, round $ datePartForDate "week" val)

class HasTime a where
  datePartForTime :: Text -> a -> Double

  hour :: a -> Int32
  hour time = round $ datePartForTime "hour" time

  minute :: a -> Int32
  minute time = round $ datePartForTime "minute" time

  second :: a -> Int32
  second time = quot (round $ datePartForTime "microsecond" time) 1_000_000

  microsecond :: a -> Int32
  microsecond time = rem (round $ datePartForTime "microsecond" time) 1_000_000

foreign import ccall safe "&date_part_date_oid"
  pDatePartDateOid :: Ptr Oid

datePartDate :: Text -> Date -> Double
datePartDate part date = unsafePerformIO $ callFunc2 pDatePartDateOid (Just part) (Just date) >>= maybe undefined return

instance HasDate Date where
  datePartForDate = datePartDate

foreign import ccall safe "&date_part_time_oid"
  pDatePartTimeOid :: Ptr Oid

datePartTime :: Text -> Time -> Double
datePartTime part time = unsafePerformIO $ callFunc2 pDatePartTimeOid (Just part) (Just time) >>= maybe undefined return

instance HasTime Time where
  datePartForTime = datePartTime

foreign import ccall safe "&date_part_timestamp_oid"
  pDatePartTimestampOid :: Ptr Oid

datePartTimestamp :: Text -> Timestamp -> Double
datePartTimestamp part timestamp = unsafePerformIO $ callFunc2 pDatePartTimestampOid (Just part) (Just timestamp) >>= maybe undefined return

instance HasDate Timestamp where
  datePartForDate = datePartTimestamp

instance HasTime Timestamp where
  datePartForTime = datePartTimestamp

foreign import ccall safe "&date_part_interval_oid"
  pDatePartIntervalOid :: Ptr Oid

datePartInterval :: Text -> Interval -> Double
datePartInterval part interval = unsafePerformIO $ callFunc2 pDatePartIntervalOid (Just part) (Just interval) >>= maybe undefined return

years :: Interval -> Int32
years interval = round $ datePartInterval "year" interval

months :: Interval -> Int32
months interval = round $ datePartInterval "month" interval

days :: Interval -> Int32
days interval = round $ datePartInterval "day" interval

hours :: Interval -> Int32
hours interval = round $ datePartInterval "hour" interval

minutes :: Interval -> Int32
minutes interval = round $ datePartInterval "minute" interval

seconds :: Interval -> Int32
seconds interval = quot (round $ datePartInterval "microsecond" interval) 1_000_000

microseconds :: Interval -> Int32
microseconds interval = rem (round $ datePartInterval "microsecond" interval) 1_000_000

foreign import ccall safe "&combine_oid"
  pCombineOid :: Ptr Oid

combineTimestamp :: Date -> Time -> Timestamp
combineTimestamp date time = unsafePerformIO $ callFunc2 pCombineOid (Just date) (Just time) >>= maybe undefined return

foreign import ccall safe "&get_date_oid"
  pGetDateOid :: Ptr Oid

foreign import ccall safe "&get_time_oid"
  pGetTimeOid :: Ptr Oid

separateTimestamp :: Timestamp -> (Date, Time)
separateTimestamp timestamp = (unsafePerformIO $ callFunc1 pGetDateOid (Just timestamp) >>= maybe undefined return, unsafePerformIO $ callFunc1 pGetTimeOid (Just timestamp) >>= maybe undefined return)

foreign import ccall safe "&date_mi_oid"
  pDateMiOid :: Ptr Oid

dateMinusDate :: Date -> Date -> Int32
dateMinusDate date1 date2 = unsafePerformIO $ callFunc2 pDateMiOid (Just date1) (Just date2) >>= maybe undefined return

foreign import ccall safe "&date_mii_oid"
  pDateMiIOid :: Ptr Oid

dateMinusInt :: Date -> Int32 -> Date
dateMinusInt date int = unsafePerformIO $ callFunc2 pDateMiIOid (Just date) (Just int) >>= maybe undefined return

foreign import ccall safe "&date_pli_oid"
  pDatePlIOid :: Ptr Oid

datePlusInt :: Date -> Int32 -> Date
datePlusInt date int = unsafePerformIO $ callFunc2 pDatePlIOid (Just date) (Just int) >>= maybe undefined return

foreign import ccall safe "&mul_d_interval_oid"
  pMulDIntervalOid :: Ptr Oid

doubleTimesInterval :: Double -> Interval -> Interval
doubleTimesInterval coeff interval = unsafePerformIO $ callFunc2 pMulDIntervalOid (Just coeff) (Just interval) >>= maybe undefined return

foreign import ccall safe "&date_mi_interval_oid"
  pDateMiIntervalOid :: Ptr Oid

dateMinusInterval :: Date -> Interval -> Timestamp
dateMinusInterval date interval = unsafePerformIO $ callFunc2 pDateMiIntervalOid (Just date) (Just interval) >>= maybe undefined return

foreign import ccall safe "&date_pl_interval_oid"
  pDatePlIntervalOid :: Ptr Oid

datePlusInterval :: Date -> Interval -> Timestamp
datePlusInterval date interval = unsafePerformIO $ callFunc2 pDatePlIntervalOid (Just date) (Just interval) >>= maybe undefined return

class IntervalDiff a where
  addInterval :: a -> Interval -> a
  subInterval :: a -> Interval -> a
  diff :: a -> a -> Interval

foreign import ccall safe "&time_pl_interval_oid"
  pTimePlIntervalOid :: Ptr Oid

foreign import ccall safe "&time_mi_interval_oid"
  pTimeMiIntervalOid :: Ptr Oid

foreign import ccall safe "&time_mi_time_oid"
  pTimeMiTimeOid :: Ptr Oid

instance IntervalDiff Time where
  addInterval time interval = unsafePerformIO $ callFunc2 pTimePlIntervalOid (Just time) (Just interval) >>= maybe undefined return
  subInterval time interval = unsafePerformIO $ callFunc2 pTimeMiIntervalOid (Just time) (Just interval) >>= maybe undefined return
  diff time1 time2 = unsafePerformIO $ callFunc2 pTimeMiTimeOid (Just time1) (Just time2) >>= maybe undefined return

foreign import ccall safe "&timestamp_pl_interval_oid"
  pTimestampPlIntervalOid :: Ptr Oid

foreign import ccall safe "&timestamp_mi_interval_oid"
  pTimestampMiIntervalOid :: Ptr Oid

foreign import ccall safe "&timestamp_mi_oid"
  pTimestampMiOid :: Ptr Oid

instance IntervalDiff Timestamp where
  addInterval timestamp interval = unsafePerformIO $ callFunc2 pTimestampPlIntervalOid (Just timestamp) (Just interval) >>= maybe undefined return
  subInterval timestamp interval = unsafePerformIO $ callFunc2 pTimestampMiIntervalOid (Just timestamp) (Just interval) >>= maybe undefined return
  diff timestamp1 timestamp2 = unsafePerformIO $ callFunc2 pTimestampMiOid (Just timestamp1) (Just timestamp2) >>= maybe undefined return

foreign import ccall safe "&interval_pl_oid"
  pIntervalPlOid :: Ptr Oid

foreign import ccall safe "&interval_mi_oid"
  pIntervalMiOid :: Ptr Oid

instance IntervalDiff Interval where
  addInterval interval1 interval2 = unsafePerformIO $ callFunc2 pIntervalPlOid (Just interval1) (Just interval2) >>= maybe undefined return
  subInterval interval1 interval2 = unsafePerformIO $ callFunc2 pIntervalMiOid (Just interval1) (Just interval2) >>= maybe undefined return
  diff = subInterval
