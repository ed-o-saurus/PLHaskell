{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2023 Edward F. Behn, Jr.
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

#include "plhaskell.h"

module PGsupport (ValueInfo, Datum, ReadWrite (readType, writeType, write), getField, iterate, voidDatum, mkResultList, readIsNull, wrapVoidFunc, writeNotNull, writeNull, writeVoid) where

import Data.ByteString       (packCStringLen, useAsCStringLen, ByteString)
import Data.Functor          ((<&>))
import Data.Int              (Int16, Int32, Int64)
import Data.Text             (head, singleton, Text)
import Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import Foreign.C.Types       (CBool (CBool), CSize (CSize))
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr           (FunPtr, Ptr, WordPtr (WordPtr), nullPtr, ptrToWordPtr)
import Foreign.StablePtr     (StablePtr, castPtrToStablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable      (Storable, peek, peekByteOff, peekElemOff, poke, pokeByteOff)
import Prelude               (Bool (False, True), Char, Double, Float, IO, Maybe (Just, Nothing), flip, fromIntegral, map, return, ($), (+), (.), (>>), (>>=))

import MemoryUtils           (palloc)

data ValueInfo
newtype Datum = Datum WordPtr deriving newtype (Storable)

voidDatum :: Datum
voidDatum = Datum $ ptrToWordPtr nullPtr

-- Get field of ValueInfo struct
getField :: Ptr ValueInfo -> Int16 -> IO (Ptr ValueInfo)
getField pValueInfo i = do
    fields <- (#peek struct ValueInfo, fields) pValueInfo
    peekElemOff fields $ fromIntegral i

-- Determine the value of the isNull field of a ValueInfo struct
readIsNull :: Ptr ValueInfo -> IO Bool
readIsNull pValueInfo = do
    CBool isNull <- (#peek struct ValueInfo, is_null) pValueInfo
    return $ toBool isNull

-- Set isNull to true
writeNull :: Ptr ValueInfo -> IO ()
writeNull pValueInfo = (#poke struct ValueInfo, is_null) pValueInfo (CBool $ fromBool True)

-- Set isNull to false
writeNotNull :: Ptr ValueInfo -> IO ()
writeNotNull pValueInfo = (#poke struct ValueInfo, is_null) pValueInfo (CBool $ fromBool False)

-- Do nothing when returning void
writeVoid :: () -> Ptr ValueInfo -> IO ()
writeVoid () _pValueInfo = return ()

class ReadWrite a where
    read :: Datum -> IO a
    write :: a -> IO Datum

    readType :: Ptr ValueInfo -> IO (Maybe a)
    readType pValueInfo = do
        isNull <- readIsNull pValueInfo
        if isNull
        then return Nothing
        else do
            value <- (#peek struct ValueInfo, value) pValueInfo
            read value <&> Just

    writeType :: Maybe a -> Ptr ValueInfo -> IO ()
    writeType Nothing pValueInfo = do
        (#poke struct ValueInfo, value) pValueInfo voidDatum
        writeNull pValueInfo

    writeType (Just result) pValueInfo = do
        write result >>= (#poke struct ValueInfo, value) pValueInfo
        writeNotNull pValueInfo

-- Get the size of a variable length array
foreign import capi unsafe "postgres.h VARSIZE_ANY_EXHDR"
    getVarSize :: Datum -> IO CSize

-- Set the size of a variable length array
foreign import capi unsafe "postgres.h SET_VARSIZE"
    c_setVarSize :: Datum -> CSize -> IO ()

setVarSize :: Datum -> CSize -> IO ()
setVarSize datum len = c_setVarSize datum ((#const VARHDRSZ) + len)

-- Get the start of a variable length array
foreign import capi unsafe "postgres.h VARDATA_ANY"
    getVarData :: Datum -> IO (Ptr b)

instance ReadWrite ByteString where
    read datum = do
        len <- getVarSize datum
        pData <- getVarData datum
        packCStringLen (pData, fromIntegral len)

    write result = useAsCStringLen result (\(src, len) -> do
        p <- palloc $ fromIntegral (len + (#const VARHDRSZ))
        let value = Datum $ ptrToWordPtr p
        setVarSize value (fromIntegral len)
        pData <- getVarData value
        copyBytes pData src len
        return value)

instance ReadWrite Text where
    read value = read value <&> decodeUtf8
    write = write . encodeUtf8

instance ReadWrite Char where
    read value = read value <&> head
    write = write . singleton

foreign import capi unsafe "postgres.h DatumGetBool"
    datumGetBool :: Datum -> IO CBool

foreign import capi unsafe "postgres.h BoolGetDatum"
    boolGetDatum :: CBool -> IO Datum

instance ReadWrite Bool where
    read value = datumGetBool value <&> toBool
    write = boolGetDatum . CBool . fromBool

foreign import capi unsafe "postgres.h DatumGetInt16"
    datumGetInt16 :: Datum -> IO Int16

foreign import capi unsafe "postgres.h Int16GetDatum"
    int16GetDatum :: Int16 -> IO Datum

instance ReadWrite Int16 where
    read = datumGetInt16
    write = int16GetDatum

foreign import capi unsafe "postgres.h DatumGetInt32"
    datumGetInt32 :: Datum -> IO Int32

foreign import capi unsafe "postgres.h Int32GetDatum"
    int32GetDatum :: Int32 -> IO Datum

instance ReadWrite Int32 where
    read = datumGetInt32
    write = int32GetDatum

foreign import capi unsafe "postgres.h DatumGetInt64"
    datumGetInt64 :: Datum -> IO Int64

foreign import capi unsafe "postgres.h Int64GetDatum"
    int64GetDatum :: Int64 -> IO Datum

instance ReadWrite Int64 where
    read = datumGetInt64
    write = int64GetDatum

foreign import capi unsafe "postgres.h DatumGetFloat4"
    datumGetFloat4 :: Datum -> IO Float

foreign import capi unsafe "postgres.h Float4GetDatum"
    float4GetDatum :: Float -> IO Datum

instance ReadWrite Float where
    read = datumGetFloat4
    write = float4GetDatum

foreign import capi unsafe "postgres.h DatumGetFloat8"
    datumGetFloat8 :: Datum -> IO Double

foreign import capi unsafe "postgres.h Float8GetDatum"
    float8GetDatum :: Double -> IO Datum

instance ReadWrite Double where
    read = datumGetFloat8
    write = float8GetDatum

-- Convert a list of values to a list of actions with that execute writeResult on the elements of the list
mkResultList :: (Maybe a -> Ptr ValueInfo -> IO ())-> [Maybe a] -> Ptr ValueInfo -> [IO ()]
mkResultList writeResult results pResultValueInfo = map ((flip writeResult) pResultValueInfo) results

iterate :: Ptr (StablePtr [IO ()]) -> IO ()
iterate pList = do
    spList <- peek pList
    writeResultList <- deRefStablePtr spList
    freeStablePtr spList
    case writeResultList of
        [] -> poke pList (castPtrToStablePtr nullPtr)
        (writeResult:tail) -> (newStablePtr tail) >>= (poke pList) >> writeResult

foreign import ccall "wrapper"
    wrapVoidFunc :: IO () -> IO (FunPtr (IO ()))
