{-# LANGUAGE CApiFFI #-}
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

#{include "postgres.h"}

module PGsupport
  ( Datum
      ( Datum
      ),
    BaseType
      ( read,
        write,
        decode,
        encode
      ),
    callFunc0,
    callFunc1,
    callFunc2,
    callFunc3,
    callFunc4,
    callFunc5,
    callFunc6,
    callFunc7,
    encodeVoid,
    maybeWrap,
    mkResultList,
    readComposite,
    wrapFunction,
    writeComposite,
    writeResult,
  )
where

import Control.Monad
  ( (>=>),
  )
import Data.ByteString
  ( ByteString,
    packCStringLen,
    useAsCStringLen,
  )
import Data.Functor
  ( (<$>),
  )
import Data.Int
  ( Int16,
    Int32,
    Int64,
  )
import Data.Maybe
  ( fromMaybe,
    isNothing,
  )
import Data.Text
  ( Text,
    head,
    singleton,
  )
import Data.Text.Encoding
  ( decodeUtf8,
    encodeUtf8,
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
  ( peekArray,
  )
import Foreign.Marshal.Utils
  ( copyBytes,
    fromBool,
    toBool,
  )
import Foreign.Ptr
  ( FunPtr,
    Ptr,
    WordPtr
      ( WordPtr
      ),
    nullPtr,
    ptrToWordPtr,
  )
import Foreign.Storable
  ( peek,
    poke,
  )
import PGcommon
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
    getCount,
    pWithArray,
    pWithArrayLen,
    palloc,
    pallocArray,
    palloca,
    voidDatum,
  )
import Prelude
  ( Bool
      ( False,
        True
      ),
    Char,
    Double,
    Float,
    IO,
    Maybe
      ( Just,
        Nothing
      ),
    fromIntegral,
    map,
    otherwise,
    return,
    zipWith,
    ($),
    (+),
    (.),
    (==),
    (>>=),
  )

-- Dummy type to make pointers
data MemoryContextData

type MemoryContext = Ptr MemoryContextData

encodeVoid :: () -> IO (Maybe Datum)
encodeVoid () = return Nothing

maybeWrap :: (a -> IO b) -> Maybe a -> IO (Maybe b)
maybeWrap _ Nothing = return Nothing
maybeWrap func (Just value) = Just <$> func value

foreign import capi safe "spi_plh.h datum_SPI_copy"
  datumSPICopy :: Ptr TypeInfo -> Datum -> IO Datum

class BaseType a where
  read :: Datum -> IO a
  write :: a -> IO Datum

  decode :: Maybe Datum -> IO (Maybe a)
  decode = maybeWrap read

  encode :: Ptr TypeInfo -> Maybe a -> IO (Maybe Datum)
  encode pTypeInfo
    | pTypeInfo == nullPtr = maybeWrap write
    | otherwise = maybeWrap $ write >=> datumSPICopy pTypeInfo

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
  count <- fromIntegral <$> getCount pTypeInfo
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
foreign import capi safe "postgres.h VARSIZE_ANY_EXHDR"
  getVarSize :: Datum -> IO CSize

-- Set the size of a variable length array
foreign import capi safe "postgres.h SET_VARSIZE"
  cSetVarSize :: Datum -> CSize -> IO ()

setVarSize :: Datum -> CSize -> IO ()
setVarSize datum len = cSetVarSize datum $ #{const VARHDRSZ} + len

-- Get the start of a variable length array
foreign import capi safe "postgres.h VARDATA_ANY"
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
          value <- Datum . ptrToWordPtr <$> palloc (fromIntegral (len + #{const VARHDRSZ}))
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

foreign import capi safe "postgres.h DatumGetBool"
  datumGetBool :: Datum -> IO CBool

foreign import capi safe "postgres.h BoolGetDatum"
  boolGetDatum :: CBool -> IO Datum

instance BaseType Bool where
  read value = toBool <$> datumGetBool value
  write = boolGetDatum . CBool . fromBool

foreign import capi safe "postgres.h DatumGetInt16"
  datumGetInt16 :: Datum -> IO Int16

foreign import capi safe "postgres.h Int16GetDatum"
  int16GetDatum :: Int16 -> IO Datum

instance BaseType Int16 where
  read = datumGetInt16
  write = int16GetDatum

foreign import capi safe "postgres.h DatumGetInt32"
  datumGetInt32 :: Datum -> IO Int32

foreign import capi safe "postgres.h Int32GetDatum"
  int32GetDatum :: Int32 -> IO Datum

instance BaseType Int32 where
  read = datumGetInt32
  write = int32GetDatum

foreign import capi safe "postgres.h DatumGetInt64"
  datumGetInt64 :: Datum -> IO Int64

foreign import capi safe "postgres.h Int64GetDatum"
  int64GetDatum :: Int64 -> IO Datum

instance BaseType Int64 where
  read = datumGetInt64
  write = int64GetDatum

foreign import capi safe "postgres.h DatumGetFloat4"
  datumGetFloat4 :: Datum -> IO Float

foreign import capi safe "postgres.h Float4GetDatum"
  float4GetDatum :: Float -> IO Datum

instance BaseType Float where
  read = datumGetFloat4
  write = float4GetDatum

foreign import capi safe "postgres.h DatumGetFloat8"
  datumGetFloat8 :: Datum -> IO Double

foreign import capi safe "postgres.h Float8GetDatum"
  float8GetDatum :: Double -> IO Datum

instance BaseType Double where
  read = datumGetFloat8
  write = float8GetDatum

mkResultList :: (a -> IO (Maybe Datum)) -> [a] -> [Ptr CBool -> IO Datum]
mkResultList encodeResult = map (\result pIsNull -> encodeResult result >>= writeResult pIsNull)

foreign import ccall "wrapper"
  wrapFunction :: (Ptr NullableDatum -> Ptr CBool -> IO Datum) -> IO (FunPtr (Ptr NullableDatum -> Ptr CBool -> IO Datum))

foreign import ccall safe "&CurrentMemoryContext"
  pCurrentMemoryContext :: Ptr MemoryContext

foreign import ccall safe "plhaskell.h alloc_set_context_create_small_temp"
  allocSetContextCreateSmallTemp :: MemoryContext -> IO MemoryContext

foreign import ccall safe "palloc.h MemoryContextDelete"
  memoryContextDelete :: MemoryContext -> IO ()

withTempContext :: IO a -> IO a
withTempContext action = do
  oldMemoryContext <- peek pCurrentMemoryContext
  newMemoryContext <- allocSetContextCreateSmallTemp oldMemoryContext
  poke pCurrentMemoryContext newMemoryContext
  retVal <- action
  poke pCurrentMemoryContext oldMemoryContext
  memoryContextDelete newMemoryContext
  return retVal

foreign import capi safe "plhaskell.h call_func"
  cCallFunc :: Oid -> Int16 -> Ptr NullableDatum -> Ptr CBool -> IO Datum

callFunc :: Ptr Oid -> [Maybe Datum] -> IO (Maybe Datum)
callFunc pFunctionId mDatums =
  palloca $ \pIsNull -> do
    pWithArrayLen (map NullableDatum mDatums) $ \nargs pNullableDatums -> do
      functionId <- peek pFunctionId
      dResult <- cCallFunc functionId (fromIntegral nargs) pNullableDatums pIsNull
      isNull <- peek pIsNull
      if (toBool isNull)
        then return Nothing
        else return $ Just dResult

callFunc0 :: (BaseType b) => Ptr Oid -> IO (Maybe b)
callFunc0 pFunctionId = withTempContext $ do
  callFunc pFunctionId [] >>= decode

callFunc1 :: (BaseType a0, BaseType b) => Ptr Oid -> Maybe a0 -> IO (Maybe b)
callFunc1 pFunctionId arg0 = withTempContext $ do
  mDatum0 <- encode' arg0

  callFunc pFunctionId [mDatum0] >>= decode
  where
    encode' :: (BaseType c) => Maybe c -> IO (Maybe Datum)
    encode' = encode nullPtr

callFunc2 :: (BaseType a0, BaseType a1, BaseType b) => Ptr Oid -> Maybe a0 -> Maybe a1 -> IO (Maybe b)
callFunc2 pFunctionId arg0 arg1 = withTempContext $ do
  mDatum0 <- encode' arg0
  mDatum1 <- encode' arg1

  callFunc pFunctionId [mDatum0, mDatum1] >>= decode
  where
    encode' :: (BaseType c) => Maybe c -> IO (Maybe Datum)
    encode' = encode nullPtr

callFunc3 :: (BaseType a0, BaseType a1, BaseType a2, BaseType b) => Ptr Oid -> Maybe a0 -> Maybe a1 -> Maybe a2 -> IO (Maybe b)
callFunc3 pFunctionId arg0 arg1 arg2 = withTempContext $ do
  mDatum0 <- encode' arg0
  mDatum1 <- encode' arg1
  mDatum2 <- encode' arg2

  callFunc pFunctionId [mDatum0, mDatum1, mDatum2] >>= decode
  where
    encode' :: (BaseType c) => Maybe c -> IO (Maybe Datum)
    encode' = encode nullPtr

callFunc4 :: (BaseType a0, BaseType a1, BaseType a2, BaseType a3, BaseType b) => Ptr Oid -> Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> IO (Maybe b)
callFunc4 pFunctionId arg0 arg1 arg2 arg3 = withTempContext $ do
  mDatum0 <- encode' arg0
  mDatum1 <- encode' arg1
  mDatum2 <- encode' arg2
  mDatum3 <- encode' arg3

  callFunc pFunctionId [mDatum0, mDatum1, mDatum2, mDatum3] >>= decode
  where
    encode' :: (BaseType c) => Maybe c -> IO (Maybe Datum)
    encode' = encode nullPtr

callFunc5 :: (BaseType a0, BaseType a1, BaseType a2, BaseType a3, BaseType a4, BaseType b) => Ptr Oid -> Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> IO (Maybe b)
callFunc5 pFunctionId arg0 arg1 arg2 arg3 arg4 = withTempContext $ do
  mDatum0 <- encode' arg0
  mDatum1 <- encode' arg1
  mDatum2 <- encode' arg2
  mDatum3 <- encode' arg3
  mDatum4 <- encode' arg4

  callFunc pFunctionId [mDatum0, mDatum1, mDatum2, mDatum3, mDatum4] >>= decode
  where
    encode' :: (BaseType c) => Maybe c -> IO (Maybe Datum)
    encode' = encode nullPtr

callFunc6 :: (BaseType a0, BaseType a1, BaseType a2, BaseType a3, BaseType a4, BaseType a5, BaseType b) => Ptr Oid -> Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> IO (Maybe b)
callFunc6 pFunctionId arg0 arg1 arg2 arg3 arg4 arg5 = withTempContext $ do
  mDatum0 <- encode' arg0
  mDatum1 <- encode' arg1
  mDatum2 <- encode' arg2
  mDatum3 <- encode' arg3
  mDatum4 <- encode' arg4
  mDatum5 <- encode' arg5

  callFunc pFunctionId [mDatum0, mDatum1, mDatum2, mDatum3, mDatum4, mDatum5] >>= decode
  where
    encode' :: (BaseType c) => Maybe c -> IO (Maybe Datum)
    encode' = encode nullPtr

callFunc7 :: (BaseType a0, BaseType a1, BaseType a2, BaseType a3, BaseType a4, BaseType a5, BaseType a6, BaseType b) => Ptr Oid -> Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe a6 -> IO (Maybe b)
callFunc7 pFunctionId arg0 arg1 arg2 arg3 arg4 arg5 arg6 = withTempContext $ do
  mDatum0 <- encode' arg0
  mDatum1 <- encode' arg1
  mDatum2 <- encode' arg2
  mDatum3 <- encode' arg3
  mDatum4 <- encode' arg4
  mDatum5 <- encode' arg5
  mDatum6 <- encode' arg6

  callFunc pFunctionId [mDatum0, mDatum1, mDatum2, mDatum3, mDatum4, mDatum5, mDatum6] >>= decode
  where
    encode' :: (BaseType c) => Maybe c -> IO (Maybe Datum)
    encode' = encode nullPtr
