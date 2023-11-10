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

module PGsupport (TypeInfo, Datum, ReadWrite (readType, writeType, write), getField, voidDatum, mkResultList, readIsNull, wrapVoidFunc, writeIsNull, writeVoid) where

import Data.ByteString       (packCStringLen, useAsCStringLen, ByteString)
import Data.Functor          ((<$>))
import Data.Int              (Int16, Int32, Int64)
import Data.Text             (head, singleton, Text)
import Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import Foreign.C.Types       (CBool (CBool), CSize (CSize))
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr           (FunPtr, Ptr, WordPtr (WordPtr), nullPtr, ptrToWordPtr)
import Foreign.Storable      (Storable, peekByteOff, peekElemOff, pokeByteOff)
import Prelude               (Bool (False, True), Char, Double, Float, IO, Maybe (Just, Nothing), flip, fromIntegral, map, return, ($), (+), (.), (>>=))

import MemoryUtils           (palloc)

data TypeInfo
newtype Datum = Datum WordPtr deriving newtype (Storable)

voidDatum :: Datum
voidDatum = Datum $ ptrToWordPtr nullPtr

-- Get field of TypeInfo struct
getField :: Ptr TypeInfo -> Int16 -> IO (Ptr TypeInfo)
getField pTypeInfo i = do
    fields <- (#peek struct TypeInfo, fields) pTypeInfo
    peekElemOff fields $ fromIntegral i

-- Determine the value of the isNull field of a TypeInfo struct
readIsNull :: Ptr TypeInfo -> IO Bool
readIsNull pTypeInfo = do
    CBool isNull <- (#peek struct TypeInfo, is_null) pTypeInfo
    return $ toBool isNull

-- Set isNull
writeIsNull :: Bool-> Ptr TypeInfo -> IO ()
writeIsNull isNull pTypeInfo = (#poke struct TypeInfo, is_null) pTypeInfo (CBool $ fromBool isNull)

-- Do nothing when returning void
writeVoid :: () -> Ptr TypeInfo -> IO ()
writeVoid () _pTypeInfo = return ()

class ReadWrite a where
    read :: Datum -> IO a
    write :: a -> IO Datum

    readType :: Ptr TypeInfo -> IO (Maybe a)
    readType pTypeInfo = do
        isNull <- readIsNull pTypeInfo
        if isNull
        then return Nothing
        else do
            value <- (#peek struct TypeInfo, value) pTypeInfo
            Just <$> read value

    writeType :: Maybe a -> Ptr TypeInfo -> IO ()
    writeType Nothing pTypeInfo = do
        (#poke struct TypeInfo, value) pTypeInfo voidDatum
        writeIsNull True pTypeInfo

    writeType (Just result) pTypeInfo = do
        write result >>= (#poke struct TypeInfo, value) pTypeInfo
        writeIsNull False pTypeInfo

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
        ptr <- palloc $ fromIntegral (len + (#const VARHDRSZ))
        let value = Datum $ ptrToWordPtr ptr
        setVarSize value (fromIntegral len)
        pData <- getVarData value
        copyBytes pData src len
        return value)

instance ReadWrite Text where
    read value = decodeUtf8 <$> read value
    write = write . encodeUtf8

instance ReadWrite Char where
    read value = head <$> read value
    write = write . singleton

foreign import capi unsafe "postgres.h DatumGetBool"
    datumGetBool :: Datum -> IO CBool

foreign import capi unsafe "postgres.h BoolGetDatum"
    boolGetDatum :: CBool -> IO Datum

instance ReadWrite Bool where
    read value = toBool <$> datumGetBool value
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
mkResultList :: (Maybe a -> Ptr TypeInfo -> IO ())-> [Maybe a] -> Ptr TypeInfo -> [IO ()]
mkResultList writeResult results pResultTypeInfo = map ((flip writeResult) pResultTypeInfo) results

foreign import ccall "wrapper"
    wrapVoidFunc :: IO () -> IO (FunPtr (IO ()))
