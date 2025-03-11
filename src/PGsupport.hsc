{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Trustworthy #-}

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

#{include "plhaskell.h"}

module PGsupport (Datum (Datum), BaseType (read, write, decode, encode), encodeVoid, maybeWrap, mkResultList, readComposite, wrapFunction, writeComposite, writeResult) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString, packCStringLen, useAsCStringLen)
import Data.Functor ((<$>), (<&>))
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, head, singleton)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.C.Types (CBool (CBool), CSize (CSize))
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr (FunPtr, Ptr, WordPtr (WordPtr), ptrToWordPtr)
import Foreign.Storable (poke)
import PGcommon (Datum (Datum), NullableDatum, TypeInfo, getCount, pWithArray, palloc, pallocArray, voidDatum)
import Prelude (Bool (False, True), Char, Double, Float, IO, Maybe (Just, Nothing), fromIntegral, map, return, zipWith, ($), (+), (.), (>>=))

encodeVoid :: () -> IO (Maybe Datum)
encodeVoid () = return Nothing

maybeWrap :: (a -> IO b) -> Maybe a -> IO (Maybe b)
maybeWrap _ Nothing = return Nothing
maybeWrap func (Just value) = Just <$> func value

foreign import capi safe "plhaskell.h datum_SPI_copy"
  datumSPICopy :: Ptr TypeInfo -> Datum -> IO Datum

class BaseType a where
  read :: Datum -> IO a
  write :: a -> IO Datum

  decode :: Maybe Datum -> IO (Maybe a)
  decode = maybeWrap read

  encode :: Ptr TypeInfo -> Maybe a -> IO (Maybe Datum)
  encode pTypeInfo = maybeWrap $ write >=> datumSPICopy pTypeInfo

writeResult :: Ptr CBool -> Maybe Datum -> IO Datum
writeResult pIsNull Nothing = do
  poke pIsNull (fromBool True)
  return voidDatum
writeResult pIsNull (Just result) = do
  poke pIsNull (fromBool False)
  return result

foreign import capi safe "plhaskell.h read_composite"
  cReadComposite :: Ptr TypeInfo -> Datum -> Ptr Datum -> Ptr CBool -> IO ()

readComposite :: Ptr TypeInfo -> Datum -> IO [Maybe Datum]
readComposite pTypeInfo datum = do
  count <- getCount pTypeInfo <&> fromIntegral
  pallocArray count $ \pDatums -> pallocArray count $ \pIsNulls -> do
    cReadComposite pTypeInfo datum pDatums pIsNulls
    datums <- peekArray count pDatums
    isNulls <- peekArray count pIsNulls
    return $ zipWith (\fieldDatum isNull -> (if (toBool isNull) then Nothing else Just fieldDatum)) datums isNulls

foreign import capi safe "plhaskell.h write_composite"
  cWriteComposite :: Ptr TypeInfo -> Ptr Datum -> Ptr CBool -> IO Datum

writeComposite :: Ptr TypeInfo -> [Maybe Datum] -> IO Datum
writeComposite pTypeInfo fields = do
  let datums = map (fromMaybe voidDatum) fields
  let isNulls = map (CBool . fromBool . isNothing) fields
  pWithArray datums $ pWithArray isNulls . cWriteComposite pTypeInfo

-- Get the size of a variable length array
foreign import capi safe "plhaskell.h VARSIZE_ANY_EXHDR"
  getVarSize :: Datum -> IO CSize

-- Set the size of a variable length array
foreign import capi safe "plhaskell.h SET_VARSIZE"
  cSetVarSize :: Datum -> CSize -> IO ()

setVarSize :: Datum -> CSize -> IO ()
setVarSize datum len = cSetVarSize datum $ #{const VARHDRSZ} + len

-- Get the start of a variable length array
foreign import capi safe "plhaskell.h VARDATA_ANY"
  getVarData :: Datum -> IO (Ptr b)

foreign import capi safe "plhaskell.h detoast_datum"
  detoastDatum :: Datum -> IO Datum

instance BaseType ByteString where
  read datum = do
    datum' <- detoastDatum datum
    len <- getVarSize datum'
    pData <- getVarData datum'
    packCStringLen (pData, fromIntegral len)

  write result =
    useAsCStringLen
      result
      ( \(src, len) -> do
          value <- palloc (fromIntegral (len + #{const VARHDRSZ})) <&> Datum . ptrToWordPtr
          setVarSize value (fromIntegral len)
          pData <- getVarData value
          copyBytes pData src len
          return value
      )

instance BaseType Text where
  read value = decodeUtf8 <$> read value
  write = write . encodeUtf8

instance BaseType Char where
  read value = head <$> read value
  write = write . singleton

foreign import capi safe "plhaskell.h DatumGetBool"
  datumGetBool :: Datum -> IO CBool

foreign import capi safe "plhaskell.h BoolGetDatum"
  boolGetDatum :: CBool -> IO Datum

instance BaseType Bool where
  read value = toBool <$> datumGetBool value
  write = boolGetDatum . CBool . fromBool

foreign import capi safe "plhaskell.h DatumGetInt16"
  datumGetInt16 :: Datum -> IO Int16

foreign import capi safe "plhaskell.h Int16GetDatum"
  int16GetDatum :: Int16 -> IO Datum

instance BaseType Int16 where
  read = datumGetInt16
  write = int16GetDatum

foreign import capi safe "plhaskell.h DatumGetInt32"
  datumGetInt32 :: Datum -> IO Int32

foreign import capi safe "plhaskell.h Int32GetDatum"
  int32GetDatum :: Int32 -> IO Datum

instance BaseType Int32 where
  read = datumGetInt32
  write = int32GetDatum

foreign import capi safe "plhaskell.h DatumGetInt64"
  datumGetInt64 :: Datum -> IO Int64

foreign import capi safe "plhaskell.h Int64GetDatum"
  int64GetDatum :: Int64 -> IO Datum

instance BaseType Int64 where
  read = datumGetInt64
  write = int64GetDatum

foreign import capi safe "plhaskell.h DatumGetFloat4"
  datumGetFloat4 :: Datum -> IO Float

foreign import capi safe "plhaskell.h Float4GetDatum"
  float4GetDatum :: Float -> IO Datum

instance BaseType Float where
  read = datumGetFloat4
  write = float4GetDatum

foreign import capi safe "plhaskell.h DatumGetFloat8"
  datumGetFloat8 :: Datum -> IO Double

foreign import capi safe "plhaskell.h Float8GetDatum"
  float8GetDatum :: Double -> IO Datum

instance BaseType Double where
  read = datumGetFloat8
  write = float8GetDatum

mkResultList :: (a -> IO (Maybe Datum)) -> [a] -> [Ptr CBool -> IO Datum]
mkResultList encodeResult = map (\result pIsNull -> encodeResult result >>= writeResult pIsNull)

foreign import ccall "wrapper"
  wrapFunction :: (Ptr NullableDatum -> Ptr CBool -> IO Datum) -> IO (FunPtr (Ptr NullableDatum -> Ptr CBool -> IO Datum))
