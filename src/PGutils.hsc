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

module PGutils (PGm, ErrorLevel, assert, debug5, debug4, debug3, debug2, debug1, log, info, notice, warning, exception, report, raiseError, unPGm, QueryParam (..), query, QueryResultValue (..), QueryResults (..)) where

import Control.Monad         (zipWithM)
import Control.Monad.Fail    (MonadFail (fail))
import Data.ByteString       (ByteString)
import Data.Int              (Int16, Int32, Int64)
import Data.Functor          ((<$>))
import Data.Text             (Text, pack, unpack)
import Data.Text.Encoding    (encodeUtf8)
import Data.Word             (Word16, Word64)
import Foreign.C.String      (peekCString, CString)
import Foreign.C.Types       (CBool, CInt (CInt), CUInt (CUInt))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr           (Ptr, WordPtr (WordPtr))
import Foreign.Storable      (peek, peekByteOff, peekElemOff)
import Prelude               (Applicative, Bool (False, True), Char, Double, Float, Functor, IO, Maybe (Nothing, Just), Monad, Show, flip, fromIntegral, length, map, mapM, mapM_, mconcat, return, undefined, ($), (.), (>>=), (==))
import System.IO.Unsafe      (unsafePerformIO)

import PGsupport             (Datum (Datum), ReadWrite (decode, encode), TypeInfo, decodeComposite, encodeComposite, maybeWrap)
import PGcommon              (Oid (Oid), getCount, getFields, pUseAsCString, pWithArray, pWithArrayLen, pWithCString, pWithCString2, range, voidDatum)

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

assert :: Bool -> Text -> PGm ()
assert True _msg = return ()
assert False msg = report exception msg

instance MonadFail PGm where
    fail = raiseError . pack

-- Parameter to pass to query
data QueryParam = QueryParamByteA                        (Maybe ByteString)
                | QueryParamText                         (Maybe Text)
                | QueryParamChar                         (Maybe Char)
                | QueryParamBool                         (Maybe Bool)
                | QueryParamInt2                         (Maybe Int16)
                | QueryParamInt4                         (Maybe Int32)
                | QueryParamInt8                         (Maybe Int64)
                | QueryParamFloat4                       (Maybe Float)
                | QueryParamFloat8                       (Maybe Double)
                | QueryParamComposite (Maybe Text, Text) (Maybe [QueryParam])

-- Extract the value type from TypeInfo struct
getValueType :: Ptr TypeInfo -> IO Word16
getValueType = (#peek struct TypeInfo, value_type)

getTypeOid :: Ptr TypeInfo -> IO Oid
getTypeOid = (#peek struct TypeInfo, type_oid)

getNatts :: Ptr TupleTable -> IO Int16
getNatts pTupleTable = (#peek struct SPITupleTable, tupdesc) pTupleTable >>= (#peek struct TupleDescData, natts)

-- Get Oid base on the constructor or stated type
-- Does not verify contents comply with the Oid
getOid :: QueryParam -> IO Oid
getOid (QueryParamByteA  _) = return (#const BYTEAOID)
getOid (QueryParamText   _) = return (#const TEXTOID)
getOid (QueryParamChar   _) = return (#const CHAROID)
getOid (QueryParamBool   _) = return (#const BOOLOID)
getOid (QueryParamInt2   _) = return (#const INT2OID)
getOid (QueryParamInt4   _) = return (#const INT4OID)
getOid (QueryParamInt8   _) = return (#const INT8OID)
getOid (QueryParamFloat4 _) = return (#const FLOAT4OID)
getOid (QueryParamFloat8 _) = return (#const FLOAT8OID)
getOid (QueryParamComposite schemaType _) = getCompositeOid schemaType

-- Verify that the TypeInfo struct oid is expected
-- return encoded Maybe Datum
encode' :: Ptr TypeInfo -> QueryParam -> IO (Maybe Datum)
encode' pTypeInfo (QueryParamByteA  value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const BYTEAOID))  "Expected type bytea in query"
    encode value

encode' pTypeInfo (QueryParamText   value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const TEXTOID))   "Expected type text in query"
    encode value

encode' pTypeInfo (QueryParamChar   value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const CHAROID))   "Expected type char in query"
    encode value

encode' pTypeInfo (QueryParamBool   value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const BOOLOID))   "Expected type bool in query"
    encode value

encode' pTypeInfo (QueryParamInt2   value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const INT2OID))   "Expected type int2 in query"
    encode value

encode' pTypeInfo (QueryParamInt4   value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const INT4OID))   "Expected type int4 in query"
    encode value

encode' pTypeInfo (QueryParamInt8   value) =  do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const INT8OID))   "Expected type int8 in query"
    encode value

encode' pTypeInfo (QueryParamFloat4 value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const FLOAT4OID)) "Expected type float4 in query"
    encode value

encode' pTypeInfo (QueryParamFloat8 value) = do
    oid <- getTypeOid pTypeInfo
    unPGm $ assert (oid == (#const FLOAT8OID)) "Expected type float8 in query"
    encode value

encode' pTypeInfo (QueryParamComposite schemaType mFields) = do
    valueType <- getValueType pTypeInfo
    unPGm $ assert (valueType == (#const COMPOSITE_TYPE)) "Expected composite type"
    oid <- getTypeOid pTypeInfo
    oid' <- getCompositeOid schemaType
    if oid == oid'
    then return ()
    else do
        name <- getSchemaTypeName pTypeInfo
        unPGm $ report exception (mconcat ["Expected type ", name, " in query"])
    (flip maybeWrap) mFields $ \queryParamFields -> do
        count <- getCount pTypeInfo
        if (length queryParamFields) == (fromIntegral count)
        then return ()
        else do
            name <- getSchemaTypeName pTypeInfo
            unPGm $ report exception (mconcat ["Type ", name, " incorrect length"])
        typeInfoFields <- getFields pTypeInfo
        zipWithM encode' typeInfoFields queryParamFields >>= encodeComposite pTypeInfo

-- Value returned by query
data QueryResultValue = QueryResultValueByteA                  (Maybe ByteString)
                      | QueryResultValueText                   (Maybe Text)
                      | QueryResultValueChar                   (Maybe Char)
                      | QueryResultValueBool                   (Maybe Bool)
                      | QueryResultValueInt2                   (Maybe Int16)
                      | QueryResultValueInt4                   (Maybe Int32)
                      | QueryResultValueInt8                   (Maybe Int64)
                      | QueryResultValueFloat4                 (Maybe Float)
                      | QueryResultValueFloat8                 (Maybe Double)
                      | QueryResultValueComposite (Text, Text) (Maybe [QueryResultValue]) deriving stock Show

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
        mapM (peekElemOff oids) (range $ fromIntegral natts)

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

decode' :: Ptr TypeInfo -> Maybe Datum -> IO QueryResultValue
decode' pTypeInfo mDatum = do
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
        (#const COMPOSITE_TYPE) -> do
            schemaType <- getSchemaType pTypeInfo
            case mDatum of
                Nothing -> return $ QueryResultValueComposite schemaType Nothing
                Just datum -> do
                    fieldPTypeInfos <- getFields pTypeInfo
                    fieldMDatums <- decodeComposite pTypeInfo datum
                    (QueryResultValueComposite schemaType) . Just <$> zipWithM decode' fieldPTypeInfos fieldMDatums
        _ -> undefined

getRow :: Ptr TupleTable -> [Ptr TypeInfo] -> Word64 -> IO [QueryResultValue]
getRow pTupleTable pTypeInfos rowNumber = do
    mDatums <- mapM (getTupleDatum pTupleTable rowNumber) [1 .. fromIntegral (length pTypeInfos)]
    zipWithM decode' pTypeInfos mDatums

getRows :: Ptr TupleTable -> Word64 -> IO [[QueryResultValue]]
getRows pTupleTable processed = do
    oids <- getOids pTupleTable
    pTypeInfos <- mapM newTypeInfo oids
    rows <- mapM (getRow pTupleTable pTypeInfos) $ range processed
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
    oids <- mapM getOid params
    pTypeInfos <- mapM newTypeInfo oids
    mDatums <- zipWithM encode' pTypeInfos params
    mapM_ deleteTypeInfo pTypeInfos
    let isNulls = map getIsNull mDatums
    let values  = map getValue  mDatums
    pUseAsCString (encodeUtf8 q) $ \ptrQuery -> do
        pWithArrayLen oids $ \nargs ptrArgtypes -> do
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

getSchemaType :: Ptr TypeInfo -> IO (Text, Text)
getSchemaType pTypeInfo = do
    nspname <- (#peek struct TypeInfo, nspname) pTypeInfo >>= peekCString
    typname <- (#peek struct TypeInfo, typname) pTypeInfo >>= peekCString
    return (pack nspname, pack typname)

getSchemaTypeName :: Ptr TypeInfo -> IO Text
getSchemaTypeName pTypeInfo = do
    (nspname, typname) <- getSchemaType pTypeInfo
    return $ mconcat [nspname, ".", typname]

foreign import capi unsafe "plhaskell.h get_composite_oid"
    c_getCompositeOid :: CString -> CString -> IO Oid

foreign import capi unsafe "plhaskell.h find_composite_oid"
    c_findCompositeOid :: CString -> IO Oid

getCompositeOid :: (Maybe Text, Text) -> IO Oid
getCompositeOid (Nothing,      typname) = pWithCString                   (unpack typname) c_findCompositeOid
getCompositeOid (Just nspname, typname) = pWithCString2 (unpack nspname) (unpack typname) c_getCompositeOid
