{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Safe #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2022 Edward F. Behn, Jr.
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

-- This module is designed to be imported by each PostgreSQL function

#include "plhaskell.h"

module PGsupport (getField, readIsNull, readBytea, readText, readChar, readBool, readInt2, readInt4, readInt8, readFloat4, readFloat8, writeNull, writeNotNull, writeVoid, writeBytea, writeText, writeChar, writeBool, writeInt2, writeInt4, writeInt8, writeFloat4, writeFloat8, iterate) where

import Data.Functor          ((<&>))
import Data.Int              (Int16, Int32, Int64)
import Data.Word             (Word8)
import Foreign.C.String      (peekCStringLen, withCStringLen)
import Foreign.C.Types       (CBool (CBool), CInt (CInt), CSize (CSize))
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr           (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.StablePtr     (StablePtr, castPtrToStablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable      (Storable, sizeOf, peek, peekByteOff, poke, pokeByteOff)
import Prelude               (Bool (False, True), Char, Double, Float, Int, IO, Maybe (Just, Nothing), String, flip, fromIntegral, head, length, return, (*), (+), (.), (>>=))

type ValueInfo = ()

-- Allocate memory using postgres' mechanism
foreign import capi safe "plhaskell.h palloc"
    c_palloc :: CSize -> IO (Ptr a)

palloc :: Int -> IO (Ptr a)
palloc size = c_palloc (CSize (fromIntegral size))

-- Get field of ValueInfo struct
getField :: Ptr ValueInfo -> Int16 -> IO (Ptr ValueInfo)
getField pValueInfo i = do
    fields <- (#peek struct ValueInfo, Fields) pValueInfo
    peek (plusPtr fields (fromIntegral i * (#size struct ValueInfo*)))

-- Determine the value of the isNull field of a ValueInfo struct
readIsNull :: Ptr ValueInfo -> IO Bool
readIsNull pValueInfo = do
    CBool isNull <- (#peek struct ValueInfo, isNull) pValueInfo
    return (toBool isNull)

-- Read a Haskell type from a ValueInfo struct
read :: (Ptr ValueInfo -> IO a) -> Ptr ValueInfo -> IO (Maybe a)
read read' pValueInfo = do
    isNull <- readIsNull pValueInfo
    if isNull
    then return Nothing
    else read' pValueInfo <&> Just

-- Get Location where data is stored
getDataLocation :: Ptr ValueInfo -> IO (Ptr a)
getDataLocation pValueInfo = do
    CBool byVal <- (#peek struct ValueInfo, ByVal) pValueInfo
    if toBool byVal
    then return ((#ptr struct ValueInfo, Value) pValueInfo)
    else (#peek struct ValueInfo, Value) pValueInfo

-- Get the size of a variable length array
foreign import capi safe "postgres.h VARSIZE_ANY_EXHDR"
    c_GetVarSize :: Ptr () -> IO CInt

getVarSize :: Ptr () -> IO Int32
getVarSize pArg = do
    CInt size <- c_GetVarSize pArg
    return size

-- Set the size of a variable length array
foreign import capi safe "postgres.h SET_VARSIZE"
    c_SetVarSize :: Ptr () -> CInt -> IO ()

setVarSize :: Ptr () -> Int -> IO ()
setVarSize pResult len = c_SetVarSize pResult (CInt ((#const VARHDRSZ) + fromIntegral len))

-- Get the start of a variable length array
foreign import capi safe "postgres.h VARDATA_ANY"
    getVarData :: Ptr () -> IO (Ptr ())

-- Functions to read Haskell types from ValueInfo structs
readBytea' :: Ptr ValueInfo -> IO [Word8]
readBytea' pValueInfo = do
    pArg <- getDataLocation pValueInfo
    len <- getVarSize pArg
    pData <- getVarData pArg
    peekArray (fromIntegral len) (castPtr pData)

readBytea :: Ptr ValueInfo -> IO (Maybe [Word8])
readBytea = read readBytea'

readText' :: Ptr ValueInfo -> IO String
readText' pValueInfo = do
    pArg <- getDataLocation pValueInfo
    len <- getVarSize pArg
    pData <- getVarData pArg
    peekCStringLen (castPtr pData, fromIntegral len)

readText :: Ptr ValueInfo -> IO (Maybe String)
readText = read readText'

readChar' :: Ptr ValueInfo -> IO Char
readChar' pValueInfo = readText' pValueInfo <&> head

readChar :: Ptr ValueInfo -> IO (Maybe Char)
readChar = read readChar'

readNumber :: Storable a => Ptr ValueInfo -> IO a
readNumber pValueInfo = getDataLocation pValueInfo >>= peek

readBool' :: Ptr ValueInfo -> IO Bool
readBool' pValueInfo = do
    CBool arg <- readNumber pValueInfo
    return (toBool arg)

readBool :: Ptr ValueInfo -> IO (Maybe Bool)
readBool = read readBool'

readInt2 :: Ptr ValueInfo -> IO (Maybe Int16)
readInt2 = read readNumber

readInt4 :: Ptr ValueInfo -> IO (Maybe Int32)
readInt4 = read readNumber

readInt8 :: Ptr ValueInfo -> IO (Maybe Int64)
readInt8 = read readNumber

readFloat4 :: Ptr ValueInfo -> IO (Maybe Float)
readFloat4 = read readNumber

readFloat8 :: Ptr ValueInfo -> IO (Maybe Double)
readFloat8 = read readNumber

-- Set isNull to true
writeNull :: Ptr ValueInfo -> IO ()
writeNull pValueInfo = (#poke struct ValueInfo, isNull) pValueInfo (CBool (fromBool True))

-- Set isNull to false
writeNotNull :: Ptr ValueInfo -> IO ()
writeNotNull pValueInfo = (#poke struct ValueInfo, isNull) pValueInfo (CBool (fromBool False))

-- Do nothing when returning void
writeVoid :: () -> Ptr ValueInfo -> IO ()
writeVoid () _ = return ()

-- Allocate memory only if the type is not ByVal and return pointer to location of data
allocDataLocation :: Ptr ValueInfo -> Int -> IO (Ptr a)
allocDataLocation pValueInfo size = do
    CBool byVal <- (#peek struct ValueInfo, ByVal) pValueInfo
    if toBool byVal
    then return ((#ptr struct ValueInfo, Value) pValueInfo)
    else do
        address <- palloc size
        (#poke struct ValueInfo, Value) pValueInfo address
        return address

-- Write a Haskell type to a ValueInfo struct
write :: (a -> Ptr ValueInfo -> IO ()) -> Maybe a -> Ptr ValueInfo -> IO ()
write _ Nothing pValueInfo = writeNull pValueInfo -- Set isNull if passed Nothing
write write' (Just result) pValueInfo = do -- Use write' if passed Just ...
    writeNotNull pValueInfo
    write' result pValueInfo

-- Functions to write Haskell types to ValueInfo structs
writeBytea' :: [Word8] -> Ptr ValueInfo -> IO ()
writeBytea' result pValueInfo = do
    let len = length result
    pResult <- allocDataLocation pValueInfo (len + (#const VARHDRSZ))
    setVarSize pResult len
    pData <- getVarData pResult
    pokeArray (castPtr pData) result

writeBytea :: Maybe [Word8] -> Ptr ValueInfo -> IO ()
writeBytea = write writeBytea'

writeText' :: String -> Ptr ValueInfo -> IO ()
writeText' result pValueInfo = withCStringLen result (\(src, len) -> do
    pResult <- allocDataLocation pValueInfo (len + (#const VARHDRSZ))
    setVarSize pResult len
    pData <- getVarData pResult
    copyBytes (castPtr pData) src len)

writeText :: Maybe String -> Ptr ValueInfo -> IO ()
writeText = write writeText'

writeChar' :: Char -> Ptr ValueInfo -> IO ()
writeChar' result = writeText' [result]

writeChar :: Maybe Char -> Ptr ValueInfo -> IO ()
writeChar = write writeChar'

writeBool' :: Bool -> Ptr ValueInfo -> IO ()
writeBool' result pValueInfo = allocDataLocation pValueInfo 1 >>= flip poke ((CBool . fromBool) result)

writeBool :: Maybe Bool -> Ptr ValueInfo -> IO ()
writeBool = write writeBool'

writeNumber :: Storable a => a -> Ptr ValueInfo -> IO ()
writeNumber result pValueInfo = allocDataLocation pValueInfo (sizeOf result) >>= flip poke result

writeInt2 :: Maybe Int16 -> Ptr ValueInfo -> IO ()
writeInt2 = write writeNumber

writeInt4 :: Maybe Int32 -> Ptr ValueInfo -> IO ()
writeInt4 = write writeNumber

writeInt8 :: Maybe Int64 -> Ptr ValueInfo -> IO ()
writeInt8 = write writeNumber

writeFloat4 :: Maybe Float -> Ptr ValueInfo -> IO ()
writeFloat4 = write writeNumber

writeFloat8 :: Maybe Double -> Ptr ValueInfo -> IO ()
writeFloat8 = write writeNumber


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
            spTail <- newStablePtr tail
            poke pList spTail
            writeResult



