{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
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

-- This module implements functions to allocate memory useing postgres' memory allocation.
-- This prevents memory leaks in case of an ERROR event.

#include "plhaskell.h"

module PGcommon (ArrayType, CallInfo, Datum (Datum), NullableDatum, Oid (Oid), TypeInfo, assert, getCount, getElement, getFields, palloc, pallocArray, pUseAsCString, pWithArray, pWithArrayLen, pWithCString, pWithCString2, range, unNullableDatum, voidDatum) where

import Data.ByteString       (ByteString, useAsCStringLen)
import Data.Int              (Int16)
import Foreign.C.String      (CString, CStringLen, withCStringLen)
import Foreign.C.Types       (CBool (CBool), CSize (CSize), CUInt (CUInt))
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Utils (copyBytes, toBool)
import Foreign.Ptr           (Ptr, WordPtr (WordPtr), nullPtr, ptrToWordPtr)
import Foreign.Storable      (alignment, peek, peekByteOff, peekElemOff, poke, sizeOf, Storable)
import Prelude               (Bool(False, True), Eq, Int, Integral, IO, Maybe (Nothing, Just), Num, String, const, flip, fromIntegral, length, mapM, return, undefined, ($), (.), (*), (-), (+), (>>=))

-- Dummy types to make pointers
data CallInfo
data TypeInfo
data ArrayType

newtype Datum = Datum WordPtr deriving newtype (Storable)
newtype Oid = Oid CUInt deriving newtype (Eq, Num, Storable)

assert :: Bool -> IO () -> IO ()
assert True _action = return ()
assert False action = action -- pUseAsCString (encodeUtf8 msg) (plhaskellReport (#const ERROR))

voidDatum :: Datum
voidDatum = Datum $ ptrToWordPtr nullPtr

-- Extract count of sub-fields from TypeInfo sruct
getCount :: Ptr TypeInfo -> IO Int16
getCount = (#peek struct TypeInfo, count)

newtype NullableDatum = NullableDatum {unNullableDatum :: Maybe Datum}
instance Storable NullableDatum where
    sizeOf _ = (#size NullableDatum)
    alignment _ = (#alignment NullableDatum)

    peek pNullableDatum = do
        CBool isNull <- (#peek NullableDatum, isnull) pNullableDatum
        if (toBool isNull)
        then return $ NullableDatum Nothing
        else do
            datum <- (#peek NullableDatum, value) pNullableDatum
            return $ NullableDatum $ Just datum

    poke = undefined -- Never used

-- Get fields of TypeInfo struct
getFields :: Ptr TypeInfo -> IO [Ptr TypeInfo]
getFields pTypeInfo = do
    count <- getCount pTypeInfo
    mapM (\j -> (#peek struct TypeInfo, fields) pTypeInfo >>= ((flip peekElemOff) . fromIntegral) j) (range count)

getElement :: Ptr TypeInfo -> IO (Ptr TypeInfo)
getElement = (#peek struct TypeInfo, element)

-- Allocate memory using postgres' mechanism
foreign import capi safe "plhaskell.h palloc"
    palloc :: CSize -> IO (Ptr a)

-- Allocate zeroed memory using postgres' mechanism
foreign import capi safe "plhaskell.h palloc0"
    palloc0 :: CSize -> IO (Ptr a)

-- Free memory using postgres' mechanism
foreign import capi safe "plhaskell.h pfree"
    pfree :: Ptr a -> IO ()

pallocArray :: forall a b . Storable a => Int -> (Ptr a -> IO b) -> IO b
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

pWithArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
pWithArray vals = pWithArrayLen vals . const

pWithArrayLen :: Storable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
pWithArrayLen vals action = do
    let len = length vals
    pallocArray len $ \ptr -> do
        pokeArray ptr vals
        action len ptr

range :: (Integral a) => a -> [a]
range 0 = []
range n = [0 .. n-1]
