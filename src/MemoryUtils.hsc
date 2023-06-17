{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- This module implements functions to allocate memory useing postgres' memory allocation.
-- This prevents memory leaks in case of an ERROR event.

module MemoryUtils (palloc, pUseAsCString, pWithArray, pWithArrayLen, pWithCString) where

import Data.ByteString       (ByteString, useAsCStringLen)
import Foreign.C.String      (CString, CStringLen, withCStringLen)
import Foreign.C.Types       (CSize (CSize))
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (sizeOf, Storable)
import Prelude               (Int, IO, String, const, fromIntegral, length, return, undefined, ($), (.), (*), (+))

-- Allocate memory using postgres' mechanism
foreign import capi unsafe "postgres.h palloc"
    palloc :: CSize -> IO (Ptr a)

-- Allocate zeroed memory using postgres' mechanism
foreign import capi unsafe "postgres.h palloc0"
    palloc0 :: CSize -> IO (Ptr a)

-- Allocate memory using postgres' mechanism
foreign import capi unsafe "postgres.h pfree"
    pfree :: Ptr a -> IO ()

pallocArray :: forall a b . Storable a => Int -> (Ptr a -> IO b) -> IO b
pallocArray size action = do
    ptr <- palloc (fromIntegral (size * sizeOf (undefined :: a)))
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
    pallocArray len $ \ptr -> do
        pokeArray ptr vals
        action len ptr
    where
        len = length vals
