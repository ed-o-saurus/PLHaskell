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

module PGsupport (ValueInfo, Datum, ReadWrite (readType, write), voidDatum, getField, readIsNull, readBytea, readText, readChar, readBool, readInt2, readInt4, readInt8, readFloat4, readFloat8, wrapVoidFunc, writeNull, writeNotNull, writeVoid, writeBytea, writeText, writeChar, writeBool, writeInt2, writeInt4, writeInt8, writeFloat4, writeFloat8, iterate) where

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
import Prelude               (Bool (False, True), Char, Double, Float, IO, Maybe (Just, Nothing), Show, fromIntegral, return, ($), (+), (.), (>>=))

data ValueInfo
newtype Datum = Datum WordPtr deriving newtype (Storable, Show)

voidDatum :: Datum
voidDatum = Datum (ptrToWordPtr nullPtr)

-- Allocate memory using postgres' mechanism
foreign import capi safe "plhaskell.h palloc"
    palloc :: CSize -> IO (Ptr a)

-- Get field of ValueInfo struct
getField :: Ptr ValueInfo -> Int16 -> IO (Ptr ValueInfo)
getField pValueInfo i = do
    fields <- (#peek struct ValueInfo, fields) pValueInfo
    peekElemOff fields (fromIntegral i)

-- Determine the value of the isNull field of a ValueInfo struct
readIsNull :: Ptr ValueInfo -> IO Bool
readIsNull pValueInfo = do
    CBool isNull <- (#peek struct ValueInfo, is_null) pValueInfo
    return (toBool isNull)

class ReadWrite a where
    read :: Datum -> IO a
    write :: a -> IO Datum

    readType :: Ptr ValueInfo -> IO (Maybe a)
    readType pValueInfo = do
        value <- (#peek struct ValueInfo, value) pValueInfo
        CBool isNull <- (#peek struct ValueInfo, is_null) pValueInfo
        if (toBool isNull)
        then return Nothing
        else read value <&> Just

    writeType :: Maybe a -> Ptr ValueInfo -> IO ()
    writeType Nothing pValueInfo = do
        (#poke struct ValueInfo, value) pValueInfo voidDatum
        (#poke struct ValueInfo, is_null) pValueInfo (CBool $ fromBool True)

    writeType (Just result) pValueInfo = do
        value <- write result
        (#poke struct ValueInfo, value) pValueInfo value
        (#poke struct ValueInfo, is_null) pValueInfo (CBool $ fromBool False)

-- Get the size of a variable length array
foreign import capi safe "postgres.h VARSIZE_ANY_EXHDR"
    getVarSize :: Datum -> IO CSize

-- Set the size of a variable length array
foreign import capi safe "postgres.h SET_VARSIZE"
    c_setVarSize :: Datum -> CSize -> IO ()

setVarSize :: Datum -> CSize -> IO ()
setVarSize datum len = c_setVarSize datum ((#const VARHDRSZ) + len)

-- Get the start of a variable length array
foreign import capi safe "postgres.h VARDATA_ANY"
    getVarData :: Datum -> IO (Ptr b)

instance ReadWrite ByteString where
    read datum = do
        len <- getVarSize datum
        pData <- getVarData datum
        packCStringLen (pData, fromIntegral len)

    write result = useAsCStringLen result (\(src, len) -> do
        p <- palloc (fromIntegral (len + (#const VARHDRSZ)))
        let value = Datum (ptrToWordPtr p)
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

foreign import capi safe "postgres.h DatumGetBool"
    datumGetBool :: Datum -> IO CBool

foreign import capi safe "postgres.h BoolGetDatum"
    boolGetDatum :: CBool -> IO Datum

instance ReadWrite Bool where
    read value = datumGetBool value <&> toBool
    write = boolGetDatum . CBool . fromBool

foreign import capi safe "postgres.h DatumGetInt16"
    datumGetInt16 :: Datum -> IO Int16

foreign import capi safe "postgres.h Int16GetDatum"
    int16GetDatum :: Int16 -> IO Datum

instance ReadWrite Int16 where
    read = datumGetInt16
    write = int16GetDatum

foreign import capi safe "postgres.h DatumGetInt32"
    datumGetInt32 :: Datum -> IO Int32

foreign import capi safe "postgres.h Int32GetDatum"
    int32GetDatum :: Int32 -> IO Datum

instance ReadWrite Int32 where
    read = datumGetInt32
    write = int32GetDatum

foreign import capi safe "postgres.h DatumGetInt64"
    datumGetInt64 :: Datum -> IO Int64

foreign import capi safe "postgres.h Int64GetDatum"
    int64GetDatum :: Int64 -> IO Datum

instance ReadWrite Int64 where
    read = datumGetInt64
    write = int64GetDatum

foreign import capi safe "postgres.h DatumGetFloat4"
    datumGetFloat4 :: Datum -> IO Float

foreign import capi safe "postgres.h Float4GetDatum"
    float4GetDatum :: Float -> IO Datum

instance ReadWrite Float where
    read = datumGetFloat4
    write = float4GetDatum

foreign import capi safe "postgres.h DatumGetFloat8"
    datumGetFloat8 :: Datum -> IO Double

foreign import capi safe "postgres.h Float8GetDatum"
    float8GetDatum :: Double -> IO Datum

instance ReadWrite Double where
    read = datumGetFloat8
    write = float8GetDatum

readBytea :: Ptr ValueInfo -> IO (Maybe ByteString)
readBytea = readType

readText :: Ptr ValueInfo -> IO (Maybe Text)
readText = readType

readChar :: Ptr ValueInfo -> IO (Maybe Char)
readChar = readType

readBool :: Ptr ValueInfo -> IO (Maybe Bool)
readBool = readType

readInt2 :: Ptr ValueInfo -> IO (Maybe Int16)
readInt2 = readType

readInt4 :: Ptr ValueInfo -> IO (Maybe Int32)
readInt4 = readType

readInt8 :: Ptr ValueInfo -> IO (Maybe Int64)
readInt8 = readType

readFloat4 :: Ptr ValueInfo -> IO (Maybe Float)
readFloat4 = readType

readFloat8 :: Ptr ValueInfo -> IO (Maybe Double)
readFloat8 = readType

-- Set isNull to true
writeNull :: Ptr ValueInfo -> IO ()
writeNull pValueInfo = (#poke struct ValueInfo, is_null) pValueInfo (CBool (fromBool True))

-- Set isNull to false
writeNotNull :: Ptr ValueInfo -> IO ()
writeNotNull pValueInfo = (#poke struct ValueInfo, is_null) pValueInfo (CBool (fromBool False))

-- Do nothing when returning void
writeVoid :: () -> Ptr ValueInfo -> IO ()
writeVoid () _ = return ()

writeBytea :: Maybe ByteString -> Ptr ValueInfo -> IO ()
writeBytea = writeType

writeText :: Maybe Text -> Ptr ValueInfo -> IO ()
writeText = writeType

writeChar :: Maybe Char -> Ptr ValueInfo -> IO ()
writeChar = writeType

writeBool :: Maybe Bool -> Ptr ValueInfo -> IO ()
writeBool = writeType

writeInt2 :: Maybe Int16 -> Ptr ValueInfo -> IO ()
writeInt2 = writeType

writeInt4 :: Maybe Int32 -> Ptr ValueInfo -> IO ()
writeInt4 = writeType

writeInt8 :: Maybe Int64 -> Ptr ValueInfo -> IO ()
writeInt8 = writeType

writeFloat4 :: Maybe Float -> Ptr ValueInfo -> IO ()
writeFloat4 = writeType

writeFloat8 :: Maybe Double -> Ptr ValueInfo -> IO ()
writeFloat8 = writeType

iterate :: Ptr (StablePtr [IO ()]) -> Ptr CBool -> IO ()
iterate pList pMoreResults = do
    spList <- peek pList
    writeResultList <- deRefStablePtr spList
    freeStablePtr spList
    case writeResultList of
        [] -> do
            poke pMoreResults (CBool (fromBool False))
            poke pList (castPtrToStablePtr nullPtr)
        (writeResult:tail) -> do
            poke pMoreResults (CBool (fromBool True))
            (newStablePtr tail) >>= (poke pList)
            writeResult

foreign import ccall "wrapper"
    wrapVoidFunc :: (IO ()) -> IO (FunPtr (IO ()))
