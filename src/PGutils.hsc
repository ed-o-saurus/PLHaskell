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

import Control.Monad         (zipWithM)
import Control.Monad.Fail    (MonadFail (fail))
import Data.ByteString       (ByteString)
import Data.Int              (Int16, Int32, Int64)
import Data.Functor          ((<$>))
import Data.Text             (Text, pack)
import Data.Text.Encoding    (encodeUtf8)
import Data.Word             (Word16, Word64)
import Foreign.C.String      (peekCString, CString)
import Foreign.C.Types       (CBool, CInt (CInt), CUInt (CUInt))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr           (Ptr, WordPtr (WordPtr))
import Foreign.Storable      (peek, peekByteOff, peekElemOff)
import Prelude               (Applicative, Bool (False, True), Char, Double, Float, Functor, IO, Maybe (Nothing, Just), Monad, Show, fromIntegral, length, map, mapM, mapM_, return, undefined, ($), (.), (-), (>>=))
import System.IO.Unsafe      (unsafePerformIO)

import PGsupport             (Datum (Datum), ReadWrite (decode, encode), TypeInfo, decodeCompositeDatum)
import PGcommon              (Oid (Oid), getFields, pUseAsCString, pWithArray, pWithArrayLen, voidDatum)

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

report :: ErrorLevel -> Text -> PGm ()
report elevel msg = PGm $ pUseAsCString (encodeUtf8 msg) (plhaskellReport (unErrorLevel elevel))

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

-- Extract the value type from TypeInfo struct
getValueType :: Ptr TypeInfo -> IO Word16
getValueType = (#peek struct TypeInfo, value_type)

getTypeOid :: Ptr TypeInfo -> IO Oid
getTypeOid = (#peek struct TypeInfo, type_oid)

getNatts :: Ptr TupleTable -> IO Int16
getNatts pTupleTable = (#peek struct SPITupleTable, tupdesc) pTupleTable >>= (#peek struct TupleDescData, natts)

-- Transform QueryParam to is_null and datum
encode' :: QueryParam -> IO (Maybe Datum)
encode' (QueryParamByteA  value) = encode value
encode' (QueryParamText   value) = encode value
encode' (QueryParamChar   value) = encode value
encode' (QueryParamBool   value) = encode value
encode' (QueryParamInt2   value) = encode value
encode' (QueryParamInt4   value) = encode value
encode' (QueryParamInt8   value) = encode value
encode' (QueryParamFloat4 value) = encode value
encode' (QueryParamFloat8 value) = encode value

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

foreign import capi unsafe "plhaskell.h get_header_field"
    c_getHeaderField :: Ptr TupleTable -> CString -> CInt -> IO ()

getHeaderField :: Ptr TupleTable -> Int16 -> IO Text
getHeaderField pTupleTable fnumber = allocaArray (#const NAMEDATALEN) $ \pName -> do
    c_getHeaderField pTupleTable pName (fromIntegral fnumber)
    pack <$> peekCString pName

getHeader :: Ptr TupleTable -> IO [Text]
getHeader pTupleTable = do
    natts <- getNatts pTupleTable
    mapM (getHeaderField pTupleTable) [1 .. natts]

foreign import capi unsafe "plhaskell.h get_oids"
    c_getOids :: Ptr TupleTable -> Ptr Oid -> IO ()

getOids :: Ptr TupleTable -> IO [Oid]
getOids pTupleTable = do
    natts <- getNatts pTupleTable
    allocaArray (fromIntegral natts) $ \oids -> do
        c_getOids pTupleTable oids
        mapM (peekElemOff oids) [0 .. (fromIntegral natts)-1]

foreign import capi safe "plhaskell.h new_type_info"
    newTypeInfo :: Oid -> IO (Ptr TypeInfo)

foreign import capi unsafe "plhaskell.h delete_type_info"
    deleteTypeInfo :: Ptr TypeInfo -> IO ()

foreign import capi unsafe "plhaskell.h get_tuple_datum"
    c_getTupleDatum :: Ptr TupleTable -> Word64 -> CInt -> Ptr CBool -> IO Datum

getTupleDatum :: Ptr TupleTable -> Word64 -> CInt -> IO (Maybe Datum)
getTupleDatum pTupleTable rowNumber fnumber = pWithArray [0] $ \pIsNull -> do
        datum <- c_getTupleDatum pTupleTable rowNumber fnumber pIsNull
        isNull <- peek pIsNull
        if (toBool isNull)
        then return Nothing
        else return $ Just datum

mkQueryResultValue :: Ptr TypeInfo -> Maybe Datum -> IO QueryResultValue
mkQueryResultValue pTypeInfo mDatum = do
    valueType <- getValueType pTypeInfo
    case valueType of
        (#const BASE_TYPE) -> do
            typeOid <- getTypeOid pTypeInfo
            case typeOid of
                (#const BYTEAOID)  -> QueryResultValueByteA  <$> decode mDatum
                (#const TEXTOID)   -> QueryResultValueText   <$> decode mDatum
                (#const CHAROID)   -> QueryResultValueChar   <$> decode mDatum
                (#const BOOLOID)   -> QueryResultValueBool   <$> decode mDatum
                (#const INT2OID)   -> QueryResultValueInt2   <$> decode mDatum
                (#const INT4OID)   -> QueryResultValueInt4   <$> decode mDatum
                (#const INT8OID)   -> QueryResultValueInt8   <$> decode mDatum
                (#const FLOAT4OID) -> QueryResultValueFloat4 <$> decode mDatum
                (#const FLOAT8OID) -> QueryResultValueFloat8 <$> decode mDatum
                _                  -> undefined
        (#const COMPOSITE_TYPE) -> case mDatum of
            Nothing -> return (QueryResultValueComposite Nothing)
            Just datum -> do
                fieldPTypeInfos <- getFields pTypeInfo
                fieldMDatums <- decodeCompositeDatum pTypeInfo datum
                QueryResultValueComposite . Just <$> zipWithM mkQueryResultValue fieldPTypeInfos fieldMDatums
        _ -> undefined

getRow :: Ptr TupleTable -> [Ptr TypeInfo] -> Word64 -> IO [QueryResultValue]
getRow pTupleTable pTypeInfos rowNumber = do
    mDatums <- mapM (getTupleDatum pTupleTable rowNumber) [1 .. fromIntegral (length pTypeInfos)]
    zipWithM mkQueryResultValue pTypeInfos mDatums

getRows :: Ptr TupleTable -> Word64 -> IO [[QueryResultValue]]
getRows _pTupleTable 0 = return []

getRows pTupleTable processed = do
    oids <- getOids pTupleTable
    pTypeInfos <- mapM newTypeInfo oids
    rows <- mapM (getRow pTupleTable pTypeInfos) [0 .. processed-1]
    mapM_ deleteTypeInfo pTypeInfos
    return rows

foreign import capi safe "plhaskell.h run_query"
    runQuery :: CString -> CInt -> Ptr Oid -> Ptr Datum -> Ptr CBool -> IO CInt

foreign import ccall safe "executor/spi.h &SPI_processed"
    pSPIProcessed :: Ptr Word64

foreign import ccall safe "executor/spi.h &SPI_tuptable"
    pSPITupTable :: Ptr (Ptr TupleTable)

foreign import capi unsafe "plhaskell.h free_tuptable"
    freeTupTable :: Ptr TupleTable -> IO ()

getIsNull :: Maybe Datum -> CBool
getIsNull Nothing  = fromBool True
getIsNull (Just _) = fromBool False

getValue :: Maybe Datum -> Datum
getValue Nothing  = voidDatum
getValue (Just datum) = datum

query :: Text -> [QueryParam] -> PGm QueryResults
query q params = PGm $ do
    let argtypes = map getOid params
    mDatums <- mapM encode' params
    let isNulls = map getIsNull mDatums
    let values  = map getValue  mDatums
    pUseAsCString (encodeUtf8 q) $ \ptrQuery -> do
        pWithArrayLen argtypes $ \nargs ptrArgtypes -> do
            pWithArray values $ \ptrValues -> do
                pWithArray isNulls $ \ptrIsNulls -> do
                    spiCode <- runQuery ptrQuery (fromIntegral nargs) ptrArgtypes ptrValues ptrIsNulls
                    processed <- peek pSPIProcessed
                    pTupleTable <- peek pSPITupTable
                    queryResult <- case spiCode of
                        (#const SPI_OK_SELECT) -> do
                            header <- getHeader pTupleTable
                            rows <- getRows pTupleTable processed
                            return (SelectResults processed header rows)
                        (#const SPI_OK_SELINTO) -> return (SelectIntoResults processed)
                        (#const SPI_OK_INSERT)  -> return (InsertResults     processed)
                        (#const SPI_OK_DELETE)  -> return (DeleteResults     processed)
                        (#const SPI_OK_UPDATE)  -> return (UpdateResults     processed)
                        (#const SPI_OK_INSERT_RETURNING) -> do
                            header <- getHeader pTupleTable
                            rows <- getRows pTupleTable processed
                            return (InsertReturningResults processed header rows)
                        (#const SPI_OK_DELETE_RETURNING) -> do
                            header <- getHeader pTupleTable
                            rows <- getRows pTupleTable processed
                            return (DeleteReturningResults processed header rows)
                        (#const SPI_OK_UPDATE_RETURNING) -> do
                            header <- getHeader pTupleTable
                            rows <- getRows pTupleTable processed
                            return (UpdateReturningResults processed header rows)
                        (#const SPI_OK_UTILITY)   -> return (UtilityResults    processed)
                        (#const SPI_OK_REWRITTEN) -> return (RewrittenResults  processed)
                        _ -> undefined
                    freeTupTable pTupleTable
                    return queryResult
