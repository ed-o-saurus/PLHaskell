{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Safe #-}
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

module PGsupport (getField, readIsNull, readBytea, readText, readChar, readBool, readInt2, readInt4, readInt8, readFloat4, readFloat8, wrapVoidFunc, writeNull, writeNotNull, writeVoid, writeBytea, writeText, writeChar, writeBool, writeInt2, writeInt4, writeInt8, writeFloat4, writeFloat8, iterate) where

import Data.ByteString       (packCStringLen, useAsCStringLen, ByteString)
import Data.Functor          ((<&>))
import Data.Int              (Int16, Int32, Int64)
import Data.Text             (head, singleton, Text)
import Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import Foreign.C.Types       (CBool (CBool), CShort (CShort), CInt (CInt), CLong (CLong), CFloat (CFloat), CDouble (CDouble), CSize (CSize))
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr           (FunPtr, Ptr, WordPtr (WordPtr), castPtr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.StablePtr     (StablePtr, castPtrToStablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable      (peek, peekByteOff, peekElemOff, poke, pokeByteOff)
import Prelude               (Bool (False, True), Char, Double, Float, Int, IO, Maybe (Just, Nothing), fromIntegral, return, ($), (+), (.), (>>=))

type ValueInfo = ()
type Datum = WordPtr

-- Allocate memory using postgres' mechanism
foreign import capi safe "plhaskell.h palloc"
    c_palloc :: CSize -> IO (Ptr a)

palloc :: Int -> IO (Ptr a)
palloc size = c_palloc (CSize (fromIntegral size))

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
        (#poke struct ValueInfo, value) pValueInfo (ptrToWordPtr nullPtr)
        (#poke struct ValueInfo, is_null) pValueInfo (CBool $ fromBool True)

    writeType (Just result) pValueInfo = do
        value <- write result
        (#poke struct ValueInfo, value) pValueInfo value
        (#poke struct ValueInfo, is_null) pValueInfo (CBool $ fromBool False)

-- Get the size of a variable length array
foreign import capi safe "postgres.h VARSIZE_ANY_EXHDR"
    c_GetVarSize :: Ptr () -> IO CInt

getVarSize :: Datum -> IO Int32
getVarSize value = do
    CInt size <- c_GetVarSize (wordPtrToPtr value)
    return size

-- Set the size of a variable length array
foreign import capi safe "postgres.h SET_VARSIZE"
    c_SetVarSize :: Ptr () -> CInt -> IO ()

setVarSize :: Datum -> Int32 -> IO ()
setVarSize value len = c_SetVarSize (wordPtrToPtr value) (CInt ((#const VARHDRSZ) + len))

-- Get the start of a variable length array
foreign import capi safe "postgres.h VARDATA_ANY"
    c_GetVarData :: Ptr () -> IO (Ptr ())

getVarData :: Datum -> IO (Ptr ())
getVarData = c_GetVarData . wordPtrToPtr

instance ReadWrite ByteString where
    read value = do
        len <- getVarSize value
        pData <- getVarData value
        packCStringLen (castPtr pData, fromIntegral len)

    write result = useAsCStringLen result (\(src, len) -> do
        ptr <- palloc (len + (#const VARHDRSZ))
        let value = ptrToWordPtr ptr
        setVarSize value (fromIntegral len)
        pData <- getVarData value
        copyBytes (castPtr pData) src len
        return value)

instance ReadWrite Text where
    read value = read value <&> decodeUtf8
    write = write . encodeUtf8

instance ReadWrite Char where
    read value = read value <&> head
    write = write . singleton

foreign import capi safe "postgres.h DatumGetBool"
    c_DatumGetBool :: Datum -> IO CBool

foreign import capi safe "postgres.h BoolGetDatum"
    c_BoolGetDatum :: CBool -> IO Datum

instance ReadWrite Bool where
    read value = c_DatumGetBool value <&> toBool
    write = c_BoolGetDatum . CBool . fromBool

foreign import capi safe "postgres.h DatumGetInt16"
    c_DatumGetInt16 :: Datum -> IO CShort

foreign import capi safe "postgres.h Int16GetDatum"
    c_Int16GetDatum :: CShort -> IO Datum

instance ReadWrite Int16 where
    read value = do
        CShort arg <- c_DatumGetInt16 value
        return arg

    write = c_Int16GetDatum . CShort

foreign import capi safe "postgres.h DatumGetInt32"
    c_DatumGetInt32 :: Datum -> IO CInt

foreign import capi safe "postgres.h Int32GetDatum"
    c_Int32GetDatum :: CInt -> IO Datum

instance ReadWrite Int32 where
    read value = do
        CInt arg <- c_DatumGetInt32 value
        return arg

    write = c_Int32GetDatum . CInt

foreign import capi safe "postgres.h DatumGetInt64"
    c_DatumGetInt64 :: Datum -> IO CLong

foreign import capi safe "postgres.h Int64GetDatum"
    c_Int64GetDatum :: CLong -> IO Datum

instance ReadWrite Int64 where
    read value = do
        CLong arg <- c_DatumGetInt64 value
        return arg

    write = c_Int64GetDatum . CLong

foreign import capi safe "postgres.h DatumGetFloat4"
    c_DatumGetFloat4 :: Datum -> IO CFloat

foreign import capi safe "postgres.h Float4GetDatum"
    c_Float4GetDatum :: CFloat -> IO Datum

instance ReadWrite Float where
    read value = do
        CFloat arg <- c_DatumGetFloat4 value
        return arg

    write = c_Float4GetDatum . CFloat

foreign import capi safe "postgres.h DatumGetFloat8"
    c_DatumGetFloat8 :: Datum -> IO CDouble

foreign import capi safe "postgres.h Float8GetDatum"
    c_Float8GetDatum :: CDouble -> IO Datum

instance ReadWrite Double where
    read value = do
        CDouble arg <- c_DatumGetFloat8 value
        return arg

    write  = c_Float8GetDatum . CDouble

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
