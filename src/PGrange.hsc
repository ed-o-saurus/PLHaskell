{-# LANGUAGE CApiFFI #-}
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

#{include "plhaskell.h"}
#{include "utils/rangetypes.h"}
#{include "utils/multirangetypes.h"}

module PGrange
  ( Bound
      ( InfiniteBound,
        ClosedBound,
        OpenBound
      ),
    Range
      ( EmptyRange,
        BoundRange
      ),
    MultiRange,
    rangeMapM,
    readRange,
    writeRange,
    multiRangeMapM,
    readMultiRange,
    writeMultiRange,
  )
where

import Data.Functor
  ( (<$>),
  )
import Data.Int
  ( Int32,
  )
import Data.Word
  ( Word32,
  )
import Foreign.C.Types
  ( CBool
      ( CBool
      ),
    CInt
      ( CInt
      ),
    CUInt
      ( CUInt
      ),
  )
import Foreign.Marshal.Utils
  ( fromBool,
    toBool,
  )
import Foreign.Ptr
  ( Ptr,
    nullPtr,
  )
import Foreign.Storable
  ( peek,
    peekByteOff,
    pokeByteOff,
  )
import PGcommon
  ( Oid
      ( Oid
      ),
    TypeInfo,
    getTypeOid,
    numRange,
    pWithArrayLen,
    palloc,
    pfree,
    voidDatum,
  )
import PGsupport
  ( Datum
      ( Datum
      ),
  )
import Prelude
  ( Bool
      ( False,
        True
      ),
    IO,
    Int,
    Maybe
      ( Just
      ),
    Monad,
    fromIntegral,
    mapM,
    maybe,
    return,
    undefined,
    ($),
    (.),
    (>>=),
  )

-- Dummy type to make pointer
data TypeCacheEntry

foreign import capi safe "utils/multirangetypes.h DatumGetRangeTypeP"
  datumGetRangeType :: Datum -> IO (Ptr (Range Datum))

foreign import capi safe "utils/multirangetypes.h RangeTypePGetDatum"
  rangeTypePGetDatum :: Ptr (Range Datum) -> IO Datum

-- Extract typcache from TypeInfo
getTypeCacheEntry :: Ptr TypeInfo -> IO (Ptr TypeCacheEntry)
getTypeCacheEntry = #{peek TypeInfo, typcache}

data Bound a
  = InfiniteBound
  | OpenBound a
  | ClosedBound a

data Range a
  = EmptyRange
  | BoundRange (Bound a) (Bound a)

rangeMapM :: forall a b m. (Monad m) => (Maybe a -> m (Maybe b)) -> Range a -> m (Range b)
rangeMapM _func EmptyRange = return EmptyRange
rangeMapM func (BoundRange low up) = do
  low' <- boundMapM low
  up' <- boundMapM up
  return $ BoundRange low' up'
  where
    boundMapM :: Bound a -> m (Bound b)
    boundMapM InfiniteBound = return InfiniteBound
    boundMapM (OpenBound val) = OpenBound <$> func' val
    boundMapM (ClosedBound val) = ClosedBound <$> func' val
    func' :: a -> m b
    func' val = do
      mVal <- func $ Just val
      maybe undefined return mVal

foreign import capi safe "utils/rangetypes.h range_deserialize"
  rangeDeserialize :: Ptr TypeCacheEntry -> Ptr (Range Datum) -> Ptr (Bound Datum) -> Ptr (Bound Datum) -> Ptr CBool -> IO ()

readRange' :: Ptr TypeInfo -> Ptr (Range Datum) -> IO (Range Datum)
readRange' pTypeInfo pRange = do
  pTypeCacheEntry <- getTypeCacheEntry pTypeInfo
  pLowerBound <- palloc #{size RangeBound}
  pUpperBound <- palloc #{size RangeBound}
  pIsEmpty <- palloc #{size bool}
  rangeDeserialize pTypeCacheEntry pRange pLowerBound pUpperBound pIsEmpty
  isEmpty <- peek pIsEmpty
  retVal <-
    if (toBool isEmpty)
      then return EmptyRange
      else do
        lowerBound <- readBound pLowerBound
        upperBound <- readBound pUpperBound
        return $ BoundRange lowerBound upperBound
  pfree pLowerBound
  pfree pUpperBound
  pfree pIsEmpty
  return retVal
  where
    readBound :: Ptr (Bound Datum) -> IO (Bound Datum)
    readBound pBound = do
      infinite <- #{peek RangeBound, infinite} pBound :: IO CBool
      if (toBool infinite)
        then return InfiniteBound
        else do
          val <- #{peek RangeBound, val} pBound
          inclusive <- #{peek RangeBound, inclusive} pBound :: IO CBool
          if (toBool inclusive)
            then return $ ClosedBound val
            else return $ OpenBound val

readRange :: Ptr TypeInfo -> Datum -> IO (Range Datum)
readRange pTypeInfo datum = datumGetRangeType datum >>= readRange' pTypeInfo

foreign import capi safe "utils/rangetypes.h make_empty_range"
  makeEmptyRange :: Ptr TypeCacheEntry -> IO (Ptr (Range Datum))

foreign import capi safe "utils/rangetypes.h make_range"
  makeRange :: Ptr TypeCacheEntry -> Ptr (Bound Datum) -> Ptr (Bound Datum) -> CBool -> Ptr Datum -> IO (Ptr (Range Datum))

writeRange' :: Ptr TypeInfo -> Range Datum -> IO (Ptr (Range Datum))
writeRange' pTypeInfo EmptyRange = do
  pTypeCacheEntry <- getTypeCacheEntry pTypeInfo
  makeEmptyRange pTypeCacheEntry
writeRange' pTypeInfo (BoundRange lowerBound upperBound) = do
  pTypeCacheEntry <- getTypeCacheEntry pTypeInfo
  pLowerBound <- palloc #{size RangeBound}
  pUpperBound <- palloc #{size RangeBound}
  populateBound pLowerBound lowerBound True
  populateBound pUpperBound upperBound False
  pRangeType <- makeRange pTypeCacheEntry pLowerBound pUpperBound (fromBool False) nullPtr
  pfree pLowerBound
  pfree pUpperBound
  return pRangeType
  where
    populateBound :: Ptr (Bound Datum) -> Bound Datum -> Bool -> IO ()
    populateBound pBound InfiniteBound l = do
      #{poke RangeBound, infinite} pBound ((fromBool True) :: CBool)
      #{poke RangeBound, inclusive} pBound ((fromBool False) :: CBool)
      #{poke RangeBound, lower} pBound ((fromBool l) :: CBool)
      #{poke RangeBound, val} pBound voidDatum
    populateBound pBound (OpenBound val) l = do
      #{poke RangeBound, infinite} pBound ((fromBool False) :: CBool)
      #{poke RangeBound, inclusive} pBound ((fromBool False) :: CBool)
      #{poke RangeBound, lower} pBound ((fromBool l) :: CBool)
      #{poke RangeBound, val} pBound val
    populateBound pBound (ClosedBound val) l = do
      #{poke RangeBound, infinite} pBound ((fromBool False) :: CBool)
      #{poke RangeBound, inclusive} pBound ((fromBool True) :: CBool)
      #{poke RangeBound, lower} pBound ((fromBool l) :: CBool)
      #{poke RangeBound, val} pBound val

writeRange :: Ptr TypeInfo -> Range Datum -> IO Datum
writeRange pTypeInfo range = writeRange' pTypeInfo range >>= rangeTypePGetDatum

type MultiRange a = [Range a]

multiRangeMapM :: forall a b m. (Monad m) => (Maybe a -> m (Maybe b)) -> MultiRange a -> m (MultiRange b)
multiRangeMapM func = mapM $ rangeMapM func

foreign import capi safe "utils/multirangetypes.h multirange_get_range"
  multiRangeGetRange :: Ptr TypeCacheEntry -> Ptr (MultiRange Datum) -> CInt -> IO (Ptr (Range Datum))

foreign import capi safe "utils/multirangetypes.h DatumGetMultirangeTypeP"
  datumGetMultirangeTypeP :: Datum -> IO (Ptr (MultiRange Datum))

readMultiRange :: Ptr TypeInfo -> Datum -> IO (MultiRange Datum)
readMultiRange pTypeInfo datum = do
  pTypeCacheEntry <- getTypeCacheEntry pTypeInfo
  pMultiRange <- datumGetMultirangeTypeP datum
  rangeCount <- #{peek MultirangeType, rangeCount} pMultiRange :: IO (Word32)
  pRanges <- mapM (multiRangeGetRange pTypeCacheEntry pMultiRange . fromIntegral) (numRange rangeCount)
  mapM (readRange' pTypeInfo) pRanges

foreign import ccall safe "utils/multirangetypes.h make_multirange"
  cMakeMultiRange :: Oid -> Ptr TypeCacheEntry -> Int32 -> Ptr (Ptr (Range Datum)) -> IO (Ptr (MultiRange Datum))

makeMultiRange :: Oid -> Ptr TypeCacheEntry -> Int -> Ptr (Ptr (Range Datum)) -> IO (Ptr (MultiRange Datum))
makeMultiRange multiRangeOid pTypeCacheEntry len = cMakeMultiRange multiRangeOid pTypeCacheEntry (fromIntegral len)

foreign import capi safe "range_plh.h MultirangeTypePGetDatum"
  multiRangePToDatum :: Ptr (MultiRange Datum) -> IO Datum

writeMultiRange :: Ptr TypeInfo -> MultiRange Datum -> IO Datum
writeMultiRange pTypeInfo multiRange = do
  pTypeCacheEntry <- getTypeCacheEntry pTypeInfo
  typeOid <- getTypeOid pTypeInfo
  pRanges <- mapM (writeRange' pTypeInfo) multiRange
  pMultiRange <- pWithArrayLen pRanges $ makeMultiRange typeOid pTypeCacheEntry
  multiRangePToDatum pMultiRange
