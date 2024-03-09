{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
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

#include "plhaskell.h"

module PGsupport (Array (..), Datum (Datum), BaseType (decode, encode), TypeInfo, arrayMap, arrayMapM, encodeVoid, maybeWrap, mkResultList, readArray, readComposite, unNullableDatum, wrapFunction, writeArray, writeComposite, writeResult) where

import Control.Monad         ((>=>))
import Data.ByteString       (packCStringLen, useAsCStringLen, ByteString)
import Data.Functor          ((<$>))
import Data.Int              (Int16, Int32, Int64)
import Data.Maybe            (fromMaybe, isNothing)
import Data.Text             (head, pack, singleton, Text)
import Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import Foreign.C.String      (CString)
import Foreign.C.Types       (CBool (CBool), CInt (CInt), CSize (CSize))
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (copyBytes, fromBool, toBool)
import Foreign.Ptr           (FunPtr, Ptr, WordPtr (WordPtr), ptrToWordPtr)
import Foreign.Storable      (poke)
import Prelude               (Bool (False, True), Char, Double, Float, Int, IO, Maybe (Just, Nothing), Monad, Num, Show, concat, fromIntegral, length, map, mapM, mapM_, product, return, show, splitAt, undefined, zipWith, ($), (+), (++), (.), (>>=), (==))

import PGcommon              (ArrayType, Datum (Datum), NullableDatum, TypeInfo, getCount, palloc, pallocArray, pUseAsCString, pWithArray, unNullableDatum, voidDatum)

encodeVoid :: () -> IO (Maybe Datum)
encodeVoid () =  return Nothing

maybeWrap :: (a -> IO b) -> Maybe a -> IO (Maybe b)
maybeWrap _ Nothing = return Nothing
maybeWrap func (Just value) = Just <$> func value

foreign import capi unsafe "plhaskell.h datum_SPI_copy"
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

foreign import capi unsafe "plhaskell.h read_composite"
    c_readComposite :: Ptr TypeInfo -> Datum -> Ptr Datum -> Ptr CBool -> IO ()

readComposite :: Ptr TypeInfo -> Datum -> IO [Maybe Datum]
readComposite pTypeInfo datum = do
    count <- getCount pTypeInfo
    let count' = fromIntegral count
    pallocArray count' $ \pDatums -> pallocArray count' $ \pIsNulls -> do
        c_readComposite pTypeInfo datum pDatums pIsNulls
        datums  <- peekArray count' pDatums
        isNulls <- peekArray count' pIsNulls
        return $ zipWith (\fieldDatum isNull -> (if (toBool isNull) then Nothing else Just fieldDatum)) datums isNulls

foreign import capi unsafe "plhaskell.h write_composite"
    c_writeComposite :: Ptr TypeInfo -> Ptr Datum -> Ptr CBool -> IO Datum

writeComposite :: Ptr TypeInfo -> [Maybe Datum] -> IO Datum
writeComposite pTypeInfo fields = do
    let datums  = map (fromMaybe voidDatum)          fields
    let isNulls = map (CBool . fromBool . isNothing) fields
    pWithArray datums $ pWithArray isNulls . c_writeComposite pTypeInfo

-- Get the size of a variable length array
foreign import capi unsafe "plhaskell.h VARSIZE_ANY_EXHDR"
    getVarSize :: Datum -> IO CSize

-- Set the size of a variable length array
foreign import capi unsafe "plhaskell.h SET_VARSIZE"
    c_setVarSize :: Datum -> CSize -> IO ()

setVarSize :: Datum -> CSize -> IO ()
setVarSize datum len = c_setVarSize datum ((#const VARHDRSZ) + len)

-- Get the start of a variable length array
foreign import capi unsafe "plhaskell.h VARDATA_ANY"
    getVarData :: Datum -> IO (Ptr b)

foreign import capi unsafe "plhaskell.h detoast_datum"
    detoastDatum :: Datum -> IO Datum

instance BaseType ByteString where
    read datum = do
        datum' <- detoastDatum datum
        len <- getVarSize datum'
        pData <- getVarData datum'
        packCStringLen (pData, fromIntegral len)

    write result = useAsCStringLen result (\(src, len) -> do
        ptr <- palloc $ fromIntegral (len + (#const VARHDRSZ))
        let value = Datum $ ptrToWordPtr ptr
        setVarSize value (fromIntegral len)
        pData <- getVarData value
        copyBytes pData src len
        return value)

instance BaseType Text where
    read value = decodeUtf8 <$> read value
    write = write . encodeUtf8

instance BaseType Char where
    read value = head <$> read value
    write = write . singleton

foreign import capi unsafe "plhaskell.h DatumGetBool"
    datumGetBool :: Datum -> IO CBool

foreign import capi unsafe "plhaskell.h BoolGetDatum"
    boolGetDatum :: CBool -> IO Datum

instance BaseType Bool where
    read value = toBool <$> datumGetBool value
    write = boolGetDatum . CBool . fromBool

foreign import capi unsafe "plhaskell.h DatumGetInt16"
    datumGetInt16 :: Datum -> IO Int16

foreign import capi unsafe "plhaskell.h Int16GetDatum"
    int16GetDatum :: Int16 -> IO Datum

instance BaseType Int16 where
    read = datumGetInt16
    write = int16GetDatum

foreign import capi unsafe "plhaskell.h DatumGetInt32"
    datumGetInt32 :: Datum -> IO Int32

foreign import capi unsafe "plhaskell.h Int32GetDatum"
    int32GetDatum :: Int32 -> IO Datum

instance BaseType Int32 where
    read = datumGetInt32
    write = int32GetDatum

foreign import capi unsafe "plhaskell.h DatumGetInt64"
    datumGetInt64 :: Datum -> IO Int64

foreign import capi unsafe "plhaskell.h Int64GetDatum"
    int64GetDatum :: Int64 -> IO Datum

instance BaseType Int64 where
    read = datumGetInt64
    write = int64GetDatum

foreign import capi unsafe "plhaskell.h DatumGetFloat4"
    datumGetFloat4 :: Datum -> IO Float

foreign import capi unsafe "plhaskell.h Float4GetDatum"
    float4GetDatum :: Float -> IO Datum

instance BaseType Float where
    read = datumGetFloat4
    write = float4GetDatum

foreign import capi unsafe "plhaskell.h DatumGetFloat8"
    datumGetFloat8 :: Datum -> IO Double

foreign import capi unsafe "plhaskell.h Float8GetDatum"
    float8GetDatum :: Double -> IO Datum

instance BaseType Double where
    read = datumGetFloat8
    write = float8GetDatum

foreign import capi safe "plhaskell.h plhaskell_report"
    plhaskellReport :: CInt -> CString -> IO ()

assert :: Bool -> Text -> IO ()
assert True _msg = return ()
assert False msg = pUseAsCString (encodeUtf8 msg) (plhaskellReport (#const ERROR))

data Array a = ArrayEmpty
             | Array1D  Int32                                          [a]
             | Array2D (Int32, Int32)                                 [[a]]
             | Array3D (Int32, Int32, Int32)                         [[[a]]]
             | Array4D (Int32, Int32, Int32, Int32)                 [[[[a]]]]
             | Array5D (Int32, Int32, Int32, Int32, Int32)         [[[[[a]]]]]
             | Array6D (Int32, Int32, Int32, Int32, Int32, Int32) [[[[[[a]]]]]] deriving stock Show

arrayMapM :: Monad m => (a -> m b) -> Array a -> m (Array b)
arrayMapM _ ArrayEmpty = return ArrayEmpty
arrayMapM func (Array1D lbs elems) =  mapM                                     func elems >>= return <$> Array1D lbs
arrayMapM func (Array2D lbs elems) = (mapM . mapM)                             func elems >>= return <$> Array2D lbs
arrayMapM func (Array3D lbs elems) = (mapM . mapM . mapM)                      func elems >>= return <$> Array3D lbs
arrayMapM func (Array4D lbs elems) = (mapM . mapM . mapM . mapM)               func elems >>= return <$> Array4D lbs
arrayMapM func (Array5D lbs elems) = (mapM . mapM . mapM . mapM . mapM)        func elems >>= return <$> Array5D lbs
arrayMapM func (Array6D lbs elems) = (mapM . mapM . mapM . mapM . mapM . mapM) func elems >>= return <$> Array6D lbs

arrayMap :: (a -> b) -> Array a -> Array b
arrayMap _ ArrayEmpty = ArrayEmpty
arrayMap func (Array1D lbs elems) = Array1D lbs $  map                                func elems
arrayMap func (Array2D lbs elems) = Array2D lbs $ (map . map)                         func elems
arrayMap func (Array3D lbs elems) = Array3D lbs $ (map . map . map)                   func elems
arrayMap func (Array4D lbs elems) = Array4D lbs $ (map . map . map . map)             func elems
arrayMap func (Array5D lbs elems) = Array5D lbs $ (map . map . map . map . map)       func elems
arrayMap func (Array6D lbs elems) = Array6D lbs $ (map . map . map . map . map . map) func elems

arrayDims1 :: [a] -> Int
arrayDims1 = length

arrayDims2 :: [[a]] -> (Int, Int)
arrayDims2 elems = case elems of
    [] -> (0, undefined)
    first:_ -> (length elems, len1)
        where len1 = arrayDims1 first

arrayDims3 :: [[[a]]] -> (Int, Int, Int)
arrayDims3 elems = case elems of
    [] -> (0, undefined, undefined)
    first:_ -> (length elems, len1, len2)
        where (len1, len2) = arrayDims2 first

arrayDims4 :: [[[[a]]]] -> (Int, Int, Int, Int)
arrayDims4 elems = case elems of
    [] -> (0, undefined, undefined, undefined)
    first:_ -> (length elems, len1, len2, len3)
        where (len1, len2, len3) = arrayDims3 first

arrayDims5 :: [[[[[a]]]]] -> (Int, Int, Int, Int, Int)
arrayDims5 elems = case elems of
    [] -> (0, undefined, undefined, undefined, undefined)
    first:_ -> (length elems, len1, len2, len3, len4)
        where (len1, len2, len3, len4) = arrayDims4 first

arrayDims6 :: [[[[[[a]]]]]] -> (Int, Int, Int, Int, Int, Int)
arrayDims6 elems = case elems of
    [] -> (0, undefined, undefined, undefined, undefined, undefined)
    first:_ -> (length elems, len1, len2, len3, len4, len5)
        where (len1, len2, len3, len4, len5) = arrayDims5 first

arrayNDim :: Array a -> Int
arrayNDim ArrayEmpty             = 0
arrayNDim (Array1D _lbs _elems) = 1
arrayNDim (Array2D _lbs _elems) = 2
arrayNDim (Array3D _lbs _elems) = 3
arrayNDim (Array4D _lbs _elems) = 4
arrayNDim (Array5D _lbs _elems) = 5
arrayNDim (Array6D _lbs _elems) = 6

isRectangular1 :: Int -> [a] -> IO ()
isRectangular1 len0 elems = do
    assert (length elems == len0) "Multidimensional arrays must have sub-arrays with matching dimensions."

isRectangular2 :: (Int, Int) -> [[a]] -> IO ()
isRectangular2 (len0, len1) elems = do
    assert (length elems == len0) "Multidimensional arrays must have sub-arrays with matching dimensions."
    mapM_ (isRectangular1 len1) elems

isRectangular3 :: (Int, Int, Int) -> [[[a]]] -> IO ()
isRectangular3 (len0, len1, len2) elems = do
    assert (length elems == len0) "Multidimensional arrays must have sub-arrays with matching dimensions."
    mapM_ (isRectangular2 (len1, len2)) elems

isRectangular4 :: (Int, Int, Int, Int) -> [[[[a]]]] -> IO ()
isRectangular4 (len0, len1, len2, len3) elems = do
    assert (length elems == len0) "Multidimensional arrays must have sub-arrays with matching dimensions."
    mapM_ (isRectangular3 (len1, len2, len3)) elems

isRectangular5 :: (Int, Int, Int, Int, Int) -> [[[[[a]]]]] -> IO ()
isRectangular5 (len0, len1, len2, len3, len4) elems = do
    assert (length elems == len0) "Multidimensional arrays must have sub-arrays with matching dimensions."
    mapM_ (isRectangular4 (len1, len2, len3, len4)) elems

isRectangular6 :: (Int, Int, Int, Int, Int, Int) -> [[[[[[a]]]]]] -> IO ()
isRectangular6 (len0, len1, len2, len3, len4, len5) elems = do
    assert (length elems == len0) "Multidimensional arrays must have sub-arrays with matching dimensions."
    mapM_ (isRectangular5 (len1, len2, len3, len4, len5)) elems

linearize :: Array a -> [a]
linearize ArrayEmpty = []
linearize (Array1D _lbs elems) =                                              elems
linearize (Array2D _lbs elems) =  concat                                      elems
linearize (Array3D _lbs elems) = (concat . concat)                            elems
linearize (Array4D _lbs elems) = (concat . concat . concat)                   elems
linearize (Array5D _lbs elems) = (concat . concat . concat . concat)          elems
linearize (Array6D _lbs elems) = (concat . concat . concat . concat . concat) elems

getDims :: Array a -> IO [Int]
getDims ArrayEmpty = return []
getDims (Array1D _lbs elems) = do
    let  dim0                                = arrayDims1 elems
    isRectangular1  dim0                                elems
    return [dim0]

getDims (Array2D _lbs elems) = do
    let (dim0, dim1)                         = arrayDims2 elems
    isRectangular2 (dim0, dim1)                         elems
    return [dim0, dim1]

getDims (Array3D _lbs elems) = do
    let (dim0, dim1, dim2)                   = arrayDims3 elems
    isRectangular3 (dim0, dim1, dim2)                   elems
    return [dim0, dim1, dim2]

getDims (Array4D _lbs elems) = do
    let (dim0, dim1, dim2, dim3)             = arrayDims4 elems
    isRectangular4 (dim0, dim1, dim2, dim3)             elems
    return [dim0, dim1, dim2, dim3]

getDims (Array5D _lbs elems) = do
    let (dim0, dim1, dim2, dim3, dim4)       = arrayDims5 elems
    isRectangular5 (dim0, dim1, dim2, dim3, dim4)       elems
    return [dim0, dim1, dim2, dim3, dim4]

getDims (Array6D _lbs elems) = do
    let (dim0, dim1, dim2, dim3, dim4, dim5) = arrayDims6 elems
    isRectangular6 (dim0, dim1, dim2, dim3, dim4, dim5) elems
    return [dim0, dim1, dim2, dim3, dim4, dim5]

getLbs :: Array a -> [Int32]
getLbs ArrayEmpty = []
getLbs (Array1D  lb0                           _elems) = [lb0]
getLbs (Array2D (lb0, lb1)                     _elems) = [lb0, lb1]
getLbs (Array3D (lb0, lb1, lb2)                _elems) = [lb0, lb1, lb2]
getLbs (Array4D (lb0, lb1, lb2, lb3)           _elems) = [lb0, lb1, lb2, lb3]
getLbs (Array5D (lb0, lb1, lb2, lb3, lb4)      _elems) = [lb0, lb1, lb2, lb3, lb4]
getLbs (Array6D (lb0, lb1, lb2, lb3, lb4, lb5) _elems) = [lb0, lb1, lb2, lb3, lb4, lb5]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = fxs : chunksOf n sxs
    where (fxs, sxs) = splitAt n xs

listTo1Tuple :: [a] -> a
listTo1Tuple [x0] = x0
listTo1Tuple _ = undefined

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple [x0, x1] = (x0, x1)
listTo2Tuple _ = undefined

listTo3Tuple :: [a] -> (a, a, a)
listTo3Tuple [x0, x1, x2] = (x0, x1, x2)
listTo3Tuple _ = undefined

listTo4Tuple :: [a] -> (a, a, a, a)
listTo4Tuple [x0, x1, x2, x3] = (x0, x1, x2, x3)
listTo4Tuple _ = undefined

listTo5Tuple :: [a] -> (a, a, a, a, a)
listTo5Tuple [x0, x1, x2, x3, x4] = (x0, x1, x2, x3, x4)
listTo5Tuple _ = undefined

listTo6Tuple :: [a] -> (a, a, a, a, a, a)
listTo6Tuple [x0, x1, x2, x3, x4, x5] = (x0, x1, x2, x3, x4, x5)
listTo6Tuple _ = undefined

foreign import capi unsafe "plhaskell.h get_array_type"
    getArrayType :: Datum -> Ptr ArrayType

foreign import capi unsafe "plhaskell.h get_ndim"
    getNdim :: Ptr ArrayType -> IO CInt

foreign import capi unsafe "plhaskell.h get_lbs_ptr"
    getLbsPtr :: Ptr ArrayType -> IO (Ptr CInt)

foreign import capi unsafe "plhaskell.h get_dims_ptr"
    getDimsPtr :: Ptr ArrayType -> IO (Ptr CInt)

foreign import capi unsafe "plhaskell.h get_array_elems"
    c_getArrayElems :: Ptr TypeInfo -> Ptr ArrayType -> CInt -> Ptr Datum -> Ptr CBool -> IO ()

getArrayElems :: Ptr TypeInfo -> Ptr ArrayType -> Int -> IO [Maybe Datum]
getArrayElems pTypeInfo array nelems = do
    pallocArray nelems $ \pDatums -> pallocArray nelems $ \pIsNulls -> do
        c_getArrayElems pTypeInfo array (fromIntegral nelems) pDatums pIsNulls
        datums  <- peekArray nelems pDatums
        isNulls <- peekArray nelems pIsNulls
        return $ zipWith (\elemDatum isNull -> (if (toBool isNull) then Nothing else Just elemDatum)) datums isNulls

product' :: Num a => [a] -> a
product' [] = 0 -- Wrong but it's what postgres needs
product' dims = product dims

readArray :: Ptr TypeInfo -> Datum -> IO (Array (Maybe Datum))
readArray pTypeInfo datum = do
    let pArray = getArrayType datum
    ndim <- getNdim pArray
    let ndim' = fromIntegral ndim
    lbs <- getLbsPtr pArray >>= peekArray ndim'
    dims <- getDimsPtr pArray >>= peekArray ndim'
    let dims' = map fromIntegral dims
    let nelems = product' dims'
    elems <- getArrayElems pTypeInfo pArray nelems
    case ndim of
        0 -> return ArrayEmpty
        1 -> return $ Array1D (listTo1Tuple (map fromIntegral lbs)) elems
        2 -> return $ Array2D (listTo2Tuple (map fromIntegral lbs))  (chunksOf dim1 elems)
            where (_dim0, dim1) = listTo2Tuple dims'
        3 -> return $ Array3D (listTo3Tuple (map fromIntegral lbs)) ((chunksOf dim1) $ (chunksOf dim2) elems)
            where (_dim0, dim1, dim2) = listTo3Tuple dims'
        4 -> return $ Array4D (listTo4Tuple (map fromIntegral lbs)) ((chunksOf dim1) $ (chunksOf dim2) $ (chunksOf dim3) elems)
            where (_dim0, dim1, dim2, dim3) = listTo4Tuple dims'
        5 -> return $ Array5D (listTo5Tuple (map fromIntegral lbs)) ((chunksOf dim1) $ (chunksOf dim2) $ (chunksOf dim3) $ (chunksOf dim4) elems)
            where (_dim0, dim1, dim2, dim3, dim4) = listTo5Tuple dims'
        6 -> return $ Array6D (listTo6Tuple (map fromIntegral lbs)) ((chunksOf dim1) $ (chunksOf dim2) $ (chunksOf dim3) $ (chunksOf dim4) $ (chunksOf dim5) elems)
            where (_dim0, dim1, dim2, dim3, dim4, dim5) = listTo6Tuple dims'
        _ -> do
            pUseAsCString (encodeUtf8 (pack ("PL/Haskell does not support " ++ show ndim ++ "-dimensional arrays"))) (plhaskellReport (#const ERROR))
            undefined -- Never called

foreign import capi unsafe "plhaskell.h write_array"
    c_writeArray :: Ptr TypeInfo -> Ptr Datum -> Ptr CBool -> CInt -> Ptr CInt -> Ptr CInt -> IO Datum

writeArray :: Ptr TypeInfo -> Array (Maybe Datum) -> IO Datum
writeArray pTypeInfo array = do
    let ndims = arrayNDim array
    let lbs = getLbs array
    dims <- getDims array
    let elems = linearize array
    let datums  = map (fromMaybe voidDatum)          elems
    let isNulls = map (CBool . fromBool . isNothing) elems
    pWithArray datums $ \pDatums ->
        pWithArray isNulls $ \pIsNulls ->
            pWithArray (map fromIntegral dims) $ \pDims ->
                pWithArray (map fromIntegral lbs) $ \pLbs ->
                    c_writeArray pTypeInfo pDatums pIsNulls (fromIntegral ndims) pDims pLbs

mkResultList :: (a -> IO (Maybe Datum)) -> [a] -> [Ptr CBool -> IO Datum]
mkResultList encodeResult = map (\result pIsNull -> encodeResult result >>= writeResult pIsNull)

foreign import ccall "wrapper"
    wrapFunction :: (Ptr NullableDatum -> Ptr CBool -> IO Datum) -> IO (FunPtr (Ptr NullableDatum -> Ptr CBool -> IO Datum))
