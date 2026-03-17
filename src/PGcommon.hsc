{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- This module implements functions to allocate memory using postgres' memory allocation.
-- This prevents memory leaks in case of an ERROR event.

#{include "plhaskell.h"}

module PGcommon
  ( Datum
      ( Datum
      ),
    NullableDatum
      ( NullableDatum
      ),
    Oid
      ( Oid
      ),
    TypeInfo,
    assert,
    handler,
    getCount,
    getElement,
    getTypeOid,
    getValueType,
    getFields,
    pfree,
    palloc,
    palloca,
    pallocArray,
    pUseAsCString,
    pWith,
    pWithArray,
    pWithArrayLen,
    pWithCString,
    pWithCString2,
    numRange,
    unNullableDatum,
    voidDatum,
  )
where

import Control.Exception
  ( SomeException,
  )
import Data.ByteString
  ( ByteString,
    useAsCStringLen,
  )
import Data.Int
  ( Int16,
  )
import Data.Word
  ( Word16,
    Word64,
  )
import Foreign.C.String
  ( CString,
    CStringLen,
    withCStringLen,
  )
import Foreign.C.Types
  ( CBool
      ( CBool
      ),
    CSize
      ( CSize
      ),
    CUInt
      ( CUInt
      ),
  )
import Foreign.Marshal.Array
  ( pokeArray,
  )
import Foreign.Marshal.Utils
  ( copyBytes,
    fromBool,
    toBool,
  )
import Foreign.Ptr
  ( Ptr,
  )
import Foreign.Storable
  ( Storable,
    alignment,
    peek,
    peekByteOff,
    peekElemOff,
    poke,
    pokeByteOff,
    sizeOf,
  )
import Prelude
  ( Bool
      ( False,
        True
      ),
    Eq,
    IO,
    Int,
    Integral,
    Maybe
      ( Just,
        Nothing
      ),
    Num,
    String,
    const,
    flip,
    fromIntegral,
    length,
    mapM,
    return,
    show,
    undefined,
    ($),
    (*),
    (+),
    (-),
    (.),
    (>>=),
  )

-- Dummy type to make pointers
data TypeInfo

newtype Datum = Datum Word64 deriving newtype (Storable)

newtype Oid = Oid CUInt deriving newtype (Eq, Num, Storable)

assert :: Bool -> IO () -> IO ()
assert True _action = return ()
assert False action = action

voidDatum :: Datum
voidDatum = Datum 0

-- Extract the value type from TypeInfo
getValueType :: Ptr TypeInfo -> IO Word16
getValueType = #{peek TypeInfo, value_type}

-- Extract type_oid from TypeInfo
getTypeOid :: Ptr TypeInfo -> IO Oid
getTypeOid = #{peek TypeInfo, type_oid}

-- Extract count of sub-fields from TypeInfo
getCount :: Ptr TypeInfo -> IO Int16
getCount = #{peek TypeInfo, count}

newtype NullableDatum = NullableDatum {unNullableDatum :: Maybe Datum}

instance Storable NullableDatum where
  sizeOf _ = #{size NullableDatum}
  alignment _ = #{alignment NullableDatum}

  peek pNullableDatum = do
    CBool isNull <- #{peek NullableDatum, isnull} pNullableDatum
    if (toBool isNull)
      then return $ NullableDatum Nothing
      else do
        datum <- #{peek NullableDatum, value} pNullableDatum
        return $ NullableDatum $ Just datum

  poke pNullableDatum nullableDatum =
    case unNullableDatum nullableDatum of
      Nothing -> #{poke NullableDatum, isnull} pNullableDatum ((fromBool True) :: CBool)
      Just datum -> do
        #{poke NullableDatum, isnull} pNullableDatum ((fromBool False) :: CBool)
        #{poke NullableDatum, value} pNullableDatum datum

-- Get fields of TypeInfo
getFields :: Ptr TypeInfo -> IO [Ptr TypeInfo]
getFields pTypeInfo = do
  count <- getCount pTypeInfo
  mapM (\j -> #{peek TypeInfo, fields} pTypeInfo >>= ((flip peekElemOff) . fromIntegral) j) (numRange count)

getElement :: Ptr TypeInfo -> IO (Ptr TypeInfo)
getElement = #{peek TypeInfo, element}

-- Allocate memory using postgres' mechanism
foreign import capi safe "postgres.h palloc"
  palloc :: CSize -> IO (Ptr a)

-- Allocate zeroed memory using postgres' mechanism
foreign import capi safe "postgres.h palloc0"
  palloc0 :: CSize -> IO (Ptr a)

-- Free memory using postgres' mechanism
foreign import capi safe "postgres.h pfree"
  pfree :: Ptr a -> IO ()

palloca :: (Storable a) => (Ptr a -> IO b) -> IO b
palloca = pallocArray 1

pallocArray :: forall a b. (Storable a) => Int -> (Ptr a -> IO b) -> IO b
pallocArray size action = do
  ptr <- palloc $ fromIntegral $ size * sizeOf (undefined :: a)
  retVal <- action ptr
  pfree ptr
  return retVal

pallocCopy :: CStringLen -> IO CString
pallocCopy (ptr1, len) = do
  ptr2 <- palloc0 ((fromIntegral len) + 1)
  copyBytes ptr2 ptr1 len
  return ptr2

pWithCString :: String -> (CString -> IO b) -> IO b
pWithCString s action = do
  ptr <- withCStringLen s pallocCopy
  retVal <- action ptr
  pfree ptr
  return retVal

pWithCString2 :: String -> String -> (CString -> CString -> IO b) -> IO b
pWithCString2 s1 s2 action = do
  ptr1 <- withCStringLen s1 pallocCopy
  ptr2 <- withCStringLen s2 pallocCopy
  retVal <- action ptr1 ptr2
  pfree ptr1
  pfree ptr2
  return retVal

pUseAsCString :: ByteString -> (CString -> IO b) -> IO b
pUseAsCString bs action = do
  ptr <- useAsCStringLen bs pallocCopy
  retVal <- action ptr
  pfree ptr
  return retVal

pWith :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
pWith val = pWithArray [val]

pWithArray :: (Storable a) => [a] -> (Ptr a -> IO b) -> IO b
pWithArray vals = pWithArrayLen vals . const

pWithArrayLen :: (Storable a) => [a] -> (Int -> Ptr a -> IO b) -> IO b
pWithArrayLen vals action = do
  let len = length vals
  pallocArray len $ \ptr -> do
    pokeArray ptr vals
    action len ptr

foreign import capi safe "error_plh.h handler"
  cHandler :: CString -> IO (Datum)

handler :: SomeException -> IO Datum
handler exp = pWithCString (show exp) cHandler

numRange :: (Integral a) => a -> [a]
numRange 0 = []
numRange n = [0 .. n - 1]
