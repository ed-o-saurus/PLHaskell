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

import Data.Int (Int32, Int64)
import Foreign.Ptr (Ptr, WordPtr (WordPtr))
import Foreign.Storable (peekByteOff, pokeByteOff)
import PGcommon (Datum (Datum), palloc)
import PGsupport (BaseType (read, write))

foreign import capi safe "plhaskell.h DatumGetPointer"
  datumGetPointer :: Datum -> IO (Ptr a)

foreign import capi safe "plhaskell.h PointerGetDatum"
  pointerGetDatum :: Ptr a -> IO Datum

newtype Date = Date Int32

foreign import capi safe "plhaskell.h DatumGetDateADT"
  datumGetDateADT :: Datum -> IO Date

foreign import capi safe "plhaskell.h DateADTGetDatum"
  dateADTGetDatum :: Date -> IO Datum

instance BaseType Date where
  read = datumGetDateADT
  write = dateADTGetDatum

newtype Time = Time Int64

foreign import capi safe "plhaskell.h DatumGetTimeADT"
  datumGetTimeADT :: Datum -> IO Time

foreign import capi safe "plhaskell.h TimeADTGetDatum"
  timeADTGetDatum :: Time -> IO Datum

instance BaseType Time where
  read = datumGetTimeADT
  write = timeADTGetDatum

data TimeTZ = TimeTZ Int64 Int32

instance BaseType TimeTZ where
  read datum = do
    ptr <- datumGetPointer datum
    time <- #{peek TimeTzADT, time} ptr
    zone <- #{peek TimeTzADT, zone} ptr
    return $ TimeTZ time zone

  write (TimeTZ time zone) = do
    ptr <- palloc #{size TimeTzADT}
    #{poke TimeTzADT, time} ptr time
    #{poke TimeTzADT, zone} ptr zone
    pointerGetDatum ptr

newtype Timestamp = Timestamp Int64

foreign import capi safe "plhaskell.h DatumGetTimestamp"
  datumGetTimestamp :: Datum -> IO Timestamp

foreign import capi safe "plhaskell.h TimestampGetDatum"
  timestampGetDatum :: Timestamp -> IO Datum

instance BaseType Timestamp where
  read = datumGetTimestamp
  write = timestampGetDatum

newtype TimestampTZ = TimestampTZ Int64

foreign import capi safe "plhaskell.h DatumGetTimestampTz"
  datumGetTimestampTz :: Datum -> IO TimestampTZ

foreign import capi safe "plhaskell.h TimestampTzGetDatum"
  timestampTzGetDatum :: TimestampTZ -> IO Datum

instance BaseType TimestampTZ where
  read = datumGetTimestampTz
  write = timestampTzGetDatum

data Interval = Interval Int64 Int32 Int32

instance BaseType Interval where
  read datum = do
    ptr <- datumGetPointer datum
    time <- #{peek Interval, time} ptr
    day <- #{peek Interval, day} ptr
    month <- #{peek Interval, month} ptr
    return $ Interval time day month

  write (Interval time day month) = do
    ptr <- palloc #{size TimeTzADT}
    #{poke Interval, time} ptr time
    #{poke Interval, day} ptr day
    #{poke Interval, month} ptr month
    pointerGetDatum ptr
