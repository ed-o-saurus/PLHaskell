{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
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

module PGsupport (Datum (Datum), ReadWrite (decode, encode), TypeInfo, decodeComposite, encodeComposite, encodeVoid, maybeWrap, mkResultList, unNullableDatum, wrapFunction, writeResult) where

import Data.ByteString       (packCStringLen, useAsCStringLen, ByteString)
import Data.Functor          ((<$>))
import Data.Int              (Int16, Int32, Int64)
import Data.Maybe            (fromMaybe, isNothing)
import Data.Text             (head, singleton, Text)
import Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import Foreign.C.Types       (CBool (CBool), CSize (CSize))
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr           (FunPtr, Ptr, WordPtr (WordPtr), ptrToWordPtr)
import Foreign.Storable      (poke)
import Prelude               (Bool (False, True), Char, Double, Float, IO, Maybe (Just, Nothing), fromIntegral, map, return, zipWith, ($), (+), (.), (>>=))

import PGcommon              (Datum (Datum), NullableDatum, TypeInfo, getCount, palloc, pallocArray, pWithArray, unNullableDatum, voidDatum)

encodeVoid :: () -> IO (Maybe Datum)
encodeVoid () =  return Nothing

maybeWrap :: (a -> IO b) -> Maybe a -> IO (Maybe b)
maybeWrap _ Nothing = return Nothing
maybeWrap func (Just value) = Just <$> func value

class ReadWrite a where
    read :: Datum -> IO a
    write :: a -> IO Datum

    decode :: Maybe Datum -> IO (Maybe a)
    decode = maybeWrap read

    encode :: Maybe a -> IO (Maybe Datum)
    encode = maybeWrap write

writeResult :: Ptr CBool -> Maybe Datum -> IO Datum
writeResult pIsNull Nothing = do
    poke pIsNull (fromBool True)
    return voidDatum

writeResult pIsNull (Just result) = do
    poke pIsNull (fromBool False)
    return result

foreign import capi unsafe "plhaskell.h decode_composite"
    c_decodeComposite :: Ptr TypeInfo -> Datum -> Ptr Datum -> Ptr CBool -> IO ()

decodeComposite :: Ptr TypeInfo -> Datum -> IO [Maybe Datum]
decodeComposite pTypeInfo datum = do
    count <- getCount pTypeInfo
    let count' = fromIntegral count
    pallocArray count' $ \pDatums -> pallocArray count' $ \pIsNulls -> do
        c_decodeComposite pTypeInfo datum pDatums pIsNulls
        datums  <- peekArray count' pDatums
        isNulls <- peekArray count' pIsNulls
        return $ zipWith (\fieldDatum isNull -> (if (toBool isNull) then Nothing else Just fieldDatum)) datums isNulls

foreign import capi unsafe "plhaskell.h encode_composite"
    c_encodeComposite :: Ptr TypeInfo -> Ptr Datum -> Ptr CBool -> IO Datum

encodeComposite :: Ptr TypeInfo -> [Maybe Datum] -> IO Datum
encodeComposite pTypeInfo fields = do
    let datums  = map (fromMaybe voidDatum)          fields
    let isNulls = map (CBool . fromBool . isNothing) fields
    pWithArray datums $ pWithArray isNulls . c_encodeComposite pTypeInfo

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

mkResultList :: (a -> IO (Maybe Datum)) -> [a] -> [Ptr CBool -> IO Datum]
mkResultList encodeResult = map (\result pIsNull -> encodeResult result >>= writeResult pIsNull)

foreign import ccall "wrapper"
    wrapFunction :: (Ptr NullableDatum -> Ptr CBool -> IO Datum) -> IO (FunPtr (Ptr NullableDatum -> Ptr CBool -> IO Datum))
