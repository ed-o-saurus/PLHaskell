{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- This module is designed to be imported by each PostgreSQL function

#include "plhaskell.h"

module PGutils (PGm, ErrorLevel, debug5, debug4, debug3, debug2, debug1, log, info, notice, warning, exception, report, raiseError, unPGm, QueryParam (..), query, QueryResultValue (..), QueryResults (..)) where

import Control.Monad.Fail    (MonadFail (fail))
import Data.ByteString       (useAsCString, ByteString)
import Data.Int              (Int16, Int32, Int64)
import Data.Functor          ((<&>))
import Data.Text             (Text, pack)
import Data.Text.Encoding    (encodeUtf8)
import Data.Word             (Word16, Word64)
import Foreign.C.String      (peekCString, CString)
import Foreign.C.Types       (CBool (CBool), CInt (CInt), CUInt (CUInt))
import Foreign.Marshal.Array (allocaArray, withArray, withArrayLen)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr           (Ptr)
import Foreign.Storable      (Storable, peek, peekByteOff, peekElemOff)
import Prelude               (Applicative, Bool (False, True), Char, Double, Eq, Float, Functor, IO, Maybe (Nothing, Just), Monad, Num, Show, fromIntegral, map, mapM, mapM_, return, undefined, unzip, zip, ($), (.), (-))
import System.IO.Unsafe      (unsafePerformIO)

import PGsupport             (Datum, ReadWrite (readType, write), ValueInfo, getField, voidDatum)

newtype Oid = Oid CUInt deriving newtype (Eq, Num, Storable)
data TupleTable
newtype PGm a = PGm {unPGm :: IO a} deriving newtype (Functor, Applicative, Monad)

newtype ErrorLevel = ErrorLevel {unErrorLevel :: CInt}
#{enum ErrorLevel, ErrorLevel,
    debug5    = DEBUG5,
    debug4    = DEBUG4,
    debug3    = DEBUG3,
    debug2    = DEBUG2,
    debug1    = DEBUG1,
    log       = LOG,
    info      = INFO,
    notice    = NOTICE,
    warning   = WARNING,
    exception = ERROR
}

foreign import capi safe "plhaskell.h plhaskell_report"
    plhaskellReport :: CInt -> CString -> IO ()

-- useAsCString leaks memory and should be replaced
report :: ErrorLevel -> Text -> PGm ()
report elevel msg = PGm $ useAsCString (encodeUtf8 msg) (plhaskellReport (unErrorLevel elevel))

raiseError :: Text -> a
raiseError msg = unsafePerformIO $ do
    unPGm $ report exception msg
    undefined

instance MonadFail PGm where
    fail = raiseError . pack

-- Parameter to pass to query
data QueryParam = QueryParamByteA  (Maybe ByteString)
                | QueryParamText   (Maybe Text)
                | QueryParamChar   (Maybe Char)
                | QueryParamBool   (Maybe Bool)
                | QueryParamInt2   (Maybe Int16)
                | QueryParamInt4   (Maybe Int32)
                | QueryParamInt8   (Maybe Int64)
                | QueryParamFloat4 (Maybe Float)
                | QueryParamFloat8 (Maybe Double)

-- Transform QueryParam to is_null and datum
encode :: QueryParam -> IO (CBool, Datum)
encode (QueryParamByteA Nothing) = return (fromBool True, voidDatum)
encode (QueryParamByteA (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamText Nothing) = return (fromBool True, voidDatum)
encode (QueryParamText (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamChar Nothing) = return (fromBool True, voidDatum)
encode (QueryParamChar (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamBool Nothing) = return (fromBool True, voidDatum)
encode (QueryParamBool (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamInt2 Nothing) = return (fromBool True, voidDatum)
encode (QueryParamInt2 (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamInt4 Nothing) = return (fromBool True, voidDatum)
encode (QueryParamInt4 (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamInt8 Nothing) = return (fromBool True, voidDatum)
encode (QueryParamInt8 (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamFloat4 Nothing) = return (fromBool True, voidDatum)
encode (QueryParamFloat4 (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

encode (QueryParamFloat8 Nothing) = return (fromBool True, voidDatum)
encode (QueryParamFloat8 (Just val)) = do
    datum <- write val
    return (fromBool False, datum)

getOid :: QueryParam -> Oid
getOid (QueryParamByteA  _) = (#const BYTEAOID)
getOid (QueryParamText   _) = (#const TEXTOID)
getOid (QueryParamChar   _) = (#const CHAROID)
getOid (QueryParamBool   _) = (#const BOOLOID)
getOid (QueryParamInt2   _) = (#const INT2OID)
getOid (QueryParamInt4   _) = (#const INT4OID)
getOid (QueryParamInt8   _) = (#const INT8OID)
getOid (QueryParamFloat4 _) = (#const FLOAT4OID)
getOid (QueryParamFloat8 _) = (#const FLOAT8OID)

-- Value returned by query
data QueryResultValue = QueryResultValueByteA     (Maybe ByteString)
                      | QueryResultValueText      (Maybe Text)
                      | QueryResultValueChar      (Maybe Char)
                      | QueryResultValueBool      (Maybe Bool)
                      | QueryResultValueInt2      (Maybe Int16)
                      | QueryResultValueInt4      (Maybe Int32)
                      | QueryResultValueInt8      (Maybe Int64)
                      | QueryResultValueFloat4    (Maybe Float)
                      | QueryResultValueFloat8    (Maybe Double)
                      | QueryResultValueComposite (Maybe [QueryResultValue]) deriving stock Show

-- Various query results
data QueryResults = SelectResults          Word64 [Text] [[QueryResultValue]]
                  | SelectIntoResults      Word64
                  | InsertResults          Word64
                  | DeleteResults          Word64
                  | UpdateResults          Word64
                  | InsertReturningResults Word64 [Text] [[QueryResultValue]]
                  | DeleteReturningResults Word64 [Text] [[QueryResultValue]]
                  | UpdateReturningResults Word64 [Text] [[QueryResultValue]]
                  | UtilityResults         Word64
                  | RewrittenResults       Word64 deriving stock Show

foreign import capi safe "plhaskell.h get_header_field"
    c_getHeaderField :: Ptr TupleTable -> CString -> CInt -> IO ()

getHeaderField :: Ptr TupleTable -> Int16 -> IO Text
getHeaderField pTupleTable fnumber = allocaArray (#const NAMEDATALEN) $ \pName -> do
    c_getHeaderField pTupleTable pName (fromIntegral fnumber)
    peekCString pName <&> pack

getHeader :: Ptr TupleTable -> IO [Text]
getHeader pTupleTable = do
    tupDesc <- (#peek struct SPITupleTable, tupdesc) pTupleTable
    natts <- (#peek struct TupleDescData, natts) tupDesc
    mapM (getHeaderField pTupleTable) [1 .. natts]

foreign import capi safe "plhaskell.h get_oids"
    c_getOids :: Ptr TupleTable -> Ptr Oid -> IO ()

getOids :: Ptr TupleTable -> IO [Oid]
getOids pTupleTable = do
    tupDesc <- (#peek struct SPITupleTable, tupdesc) pTupleTable
    natts <- (#peek struct TupleDescData, natts) tupDesc :: IO Int16
    allocaArray (fromIntegral natts) $ \oids -> do
        c_getOids pTupleTable oids
        mapM (peekElemOff oids) [0 .. (fromIntegral natts)-1]

foreign import capi safe "plhaskell.h new_value_info"
    newValueInfo :: Oid -> IO (Ptr ValueInfo)

foreign import capi safe "plhaskell.h delete_value_info"
    deleteValueInfo :: Ptr ValueInfo -> IO ()

foreign import capi safe "plhaskell.h fill_value_info"
    fillValueInfo :: Ptr TupleTable -> Ptr ValueInfo -> Word64 -> CInt -> IO ()

mkQueryResultValueTypOid :: Oid -> Ptr ValueInfo -> IO QueryResultValue
mkQueryResultValueTypOid (#const BYTEAOID)  pValueInfo = readType pValueInfo <&> QueryResultValueByteA
mkQueryResultValueTypOid (#const TEXTOID)   pValueInfo = readType pValueInfo <&> QueryResultValueText
mkQueryResultValueTypOid (#const CHAROID)   pValueInfo = readType pValueInfo <&> QueryResultValueChar
mkQueryResultValueTypOid (#const BOOLOID)   pValueInfo = readType pValueInfo <&> QueryResultValueBool
mkQueryResultValueTypOid (#const INT2OID)   pValueInfo = readType pValueInfo <&> QueryResultValueInt2
mkQueryResultValueTypOid (#const INT4OID)   pValueInfo = readType pValueInfo <&> QueryResultValueInt4
mkQueryResultValueTypOid (#const INT8OID)   pValueInfo = readType pValueInfo <&> QueryResultValueInt8
mkQueryResultValueTypOid (#const FLOAT4OID) pValueInfo = readType pValueInfo <&> QueryResultValueFloat4
mkQueryResultValueTypOid (#const FLOAT8OID) pValueInfo = readType pValueInfo <&> QueryResultValueFloat8
mkQueryResultValueTypOid _typeOid _pValueInfo = undefined

mkQueryResultValueTyp :: Word16 -> Ptr ValueInfo -> IO QueryResultValue
mkQueryResultValueTyp (#const BASE_TYPE) pValueInfo = do
    typeOid <- (#peek struct ValueInfo, type_oid) pValueInfo
    mkQueryResultValueTypOid typeOid pValueInfo

mkQueryResultValueTyp (#const COMPOSITE_TYPE) pValueInfo = do
    CBool is_null <- (#peek struct ValueInfo, is_null) pValueInfo
    if (toBool is_null)
    then return (QueryResultValueComposite Nothing)
    else do
        count <- (#peek struct ValueInfo, count) pValueInfo
        fields <- mapM (getField pValueInfo) [0 .. count-1]
        mapM mkQueryResultValue fields <&> Just <&> QueryResultValueComposite

mkQueryResultValueTyp _ _pValueInfo = undefined

mkQueryResultValue :: Ptr ValueInfo -> IO QueryResultValue
mkQueryResultValue pValueInfo = do
    typ <- (#peek struct ValueInfo, type) pValueInfo
    mkQueryResultValueTyp typ pValueInfo

getRow :: Ptr TupleTable -> [Ptr ValueInfo] -> Word64 -> IO [QueryResultValue]
getRow pTupleTable pValueInfos rowNumber = do
    mapM_ (\(pValueInfo, fnumber) -> fillValueInfo pTupleTable pValueInfo rowNumber fnumber) (zip pValueInfos [1 ..])
    mapM mkQueryResultValue pValueInfos

getRows :: Ptr TupleTable -> Word64 -> IO [[QueryResultValue]]
getRows _pTupleTable 0 = return []

getRows pTupleTable processed = do
    oids <- getOids pTupleTable
    pValueInfos <- mapM newValueInfo oids
    rows <- mapM (getRow pTupleTable pValueInfos) [0 .. processed-1]
    mapM_ deleteValueInfo pValueInfos
    return rows

mkResults :: CInt -> Word64 -> Ptr TupleTable -> IO QueryResults
mkResults (#const SPI_OK_SELECT) processed pTupleTable = do
    header <- getHeader pTupleTable
    rows <- getRows pTupleTable processed
    return (SelectResults processed header rows)

mkResults (#const SPI_OK_SELINTO)   processed _pTupleTable = return (SelectIntoResults processed)
mkResults (#const SPI_OK_INSERT)    processed _pTupleTable = return (InsertResults     processed)
mkResults (#const SPI_OK_DELETE)    processed _pTupleTable = return (DeleteResults     processed)
mkResults (#const SPI_OK_UPDATE)    processed _pTupleTable = return (UpdateResults     processed)

mkResults (#const SPI_OK_INSERT_RETURNING) processed pTupleTable = do
    header <- getHeader pTupleTable
    rows <- getRows pTupleTable processed
    return (InsertReturningResults processed header rows)

mkResults (#const SPI_OK_DELETE_RETURNING) processed pTupleTable = do
    header <- getHeader pTupleTable
    rows <- getRows pTupleTable processed
    return (DeleteReturningResults processed header rows)

mkResults (#const SPI_OK_UPDATE_RETURNING) processed pTupleTable = do
    header <- getHeader pTupleTable
    rows <- getRows pTupleTable processed
    return (UpdateReturningResults processed header rows)

mkResults (#const SPI_OK_UTILITY)   processed _pTupleTable = return (UtilityResults    processed)
mkResults (#const SPI_OK_REWRITTEN) processed _pTupleTable = return (RewrittenResults  processed)

mkResults _spiCode _processed _pTupleTable = undefined

foreign import capi safe "plhaskell.h run_query"
    runQuery :: CString -> CInt -> Ptr Oid -> Ptr Datum -> Ptr CBool -> IO CInt

foreign import ccall safe "executor/spi.h &SPI_processed"
    pSPIProcessed :: Ptr Word64

foreign import ccall safe "executor/spi.h &SPI_tuptable"
    pSPITupTable :: Ptr (Ptr TupleTable)

foreign import capi safe "plhaskell.h free_tuptable"
    freeTupTable :: Ptr TupleTable -> IO ()

query :: Text -> [QueryParam] -> PGm QueryResults
query q params = PGm $ do
    let argtypes = map getOid params
    isNullsValues <- mapM encode params
    let (isNulls, values) = unzip isNullsValues
    useAsCString (encodeUtf8 q) $ \ptr -> do
        withArrayLen argtypes $ \nargs ptrArgtypes -> do -- Leaks memory
            withArray values $ \ptrValues -> do
                withArray isNulls $ \ptrIsNulls -> do
                    spiCode <- runQuery ptr (fromIntegral nargs) ptrArgtypes ptrValues ptrIsNulls
                    processed <- peek pSPIProcessed
                    pTupleTable <- peek pSPITupTable
                    queryResult <- mkResults spiCode processed pTupleTable
                    freeTupTable pTupleTable
                    return queryResult
