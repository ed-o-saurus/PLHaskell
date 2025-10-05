{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2025 Edward F. Behn, Jr.
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

#{include "plhaskell.h"}
#{include "executor/spi.h"}

module PGutils
  ( PGm,
    ErrorLevel,
    Date
      ( DateNInfinity,
        DatePInfinity
      ),
    Time,
    Timestamp
      ( TimestampNInfinity,
        TimestampPInfinity
      ),
    Interval,
    Weekday (..),
    Month (..),
    HasDate (..),
    HasTime (..),
    arrayMap,
    arrayMapM,
    commit,
    debug5,
    debug4,
    debug3,
    debug2,
    debug1,
    log',
    info,
    notice,
    warning,
    exception,
    report,
    fatal,
    raiseError,
    raiseFatal,
    rollback,
    unPGm,
    Array (..),
    QueryParam (..),
    query,
    QueryResultValue (..),
    QueryResults (..),
    IntervalDiff (..),
    mkDate,
    mkTime,
    mkTimestamp,
    mkInterval,
    combineTimestamp,
    separateTimestamp,
    dateMinusDate,
    dateMinusInt,
    datePlusInt,
    doubleTimesInterval,
    dateMinusInterval,
    datePlusInterval,
    years,
    months,
    days,
    hours,
    minutes,
    seconds,
    microseconds,
    transactionTimestampUTC,
    statementTimestampUTC,
    LockMode (..),
    LockLevel (..),
    lock,
    tryLock,
    unlock,
    unlockAll,
  )
where

import Control.Monad
  ( zipWithM,
    (>=>),
  )
import Control.Monad.Fail
  ( MonadFail
      ( fail
      ),
  )
import Data.ByteString
  ( ByteString,
  )
import Data.Functor
  ( (<$>),
  )
import Data.Int
  ( Int16,
    Int32,
    Int64,
  )
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import Data.Text.Encoding
  ( encodeUtf8,
  )
import Data.Word
  ( Word64,
  )
import Foreign.C.String
  ( CString,
    peekCString,
  )
import Foreign.C.Types
  ( CBool
      ( CBool
      ),
    CInt
      ( CInt
      ),
    CUInt
      ( CUInt
      ),
  )
import Foreign.Marshal.Array
  ( allocaArray,
  )
import Foreign.Marshal.Utils
  ( fromBool,
    toBool,
  )
import Foreign.Ptr
  ( Ptr,
    WordPtr
      ( WordPtr
      ),
  )
import Foreign.Storable
  ( peek,
    peekByteOff,
    peekElemOff,
  )
import PGarray
  ( Array (..),
    arrayMap,
    arrayMapM,
    readArray,
    writeArray,
  )
import PGcommon
  ( Oid
      ( Oid
      ),
    TypeInfo,
    assert,
    getCount,
    getElement,
    getFields,
    getTypeOid,
    getValueType,
    pUseAsCString,
    pWithArray,
    pWithArrayLen,
    pWithCString,
    pWithCString2,
    range,
    voidDatum,
  )
import PGdatetime
  ( Date
      ( DateNInfinity,
        DatePInfinity
      ),
    HasDate (..),
    HasTime (..),
    Interval,
    IntervalDiff (..),
    Month (..),
    Time,
    Timestamp
      ( TimestampNInfinity,
        TimestampPInfinity
      ),
    Weekday (..),
    combineTimestamp,
    dateMinusDate,
    dateMinusInt,
    dateMinusInterval,
    datePlusInt,
    datePlusInterval,
    days,
    doubleTimesInterval,
    hours,
    microseconds,
    minutes,
    mkDate,
    mkInterval,
    mkTime,
    mkTimestamp,
    months,
    seconds,
    separateTimestamp,
    statementTimestampUTC',
    transactionTimestampUTC',
    years,
  )
import PGlock
  ( LockLevel (..),
    LockMode (..),
    Lockable,
    lock',
    tryLock',
    unlock',
    unlockAll',
  )
import PGsupport
  ( BaseType
      ( decode,
        encode
      ),
    Datum
      ( Datum
      ),
    maybeWrap,
    readComposite,
    writeComposite,
  )
import System.IO.Unsafe
  ( unsafePerformIO,
  )
import Prelude
  ( Applicative,
    Bool
      ( False,
        True
      ),
    Char,
    Double,
    Float,
    Functor,
    IO,
    Maybe
      ( Just,
        Nothing
      ),
    Monad,
    Show,
    flip,
    fromIntegral,
    length,
    map,
    mapM,
    mapM_,
    return,
    undefined,
    ($),
    (.),
    (==),
    (>>=),
  )

data TupleTable

newtype PGm a = PGm {unPGm :: IO a} deriving newtype (Functor, Applicative, Monad)

newtype ErrorLevel = ErrorLevel CInt

debug5 :: ErrorLevel
debug5 = ErrorLevel #{const DEBUG5}

debug4 :: ErrorLevel
debug4 = ErrorLevel #{const DEBUG4}

debug3 :: ErrorLevel
debug3 = ErrorLevel #{const DEBUG3}

debug2 :: ErrorLevel
debug2 = ErrorLevel #{const DEBUG2}

debug1 :: ErrorLevel
debug1 = ErrorLevel #{const DEBUG1}

log' :: ErrorLevel
log' = ErrorLevel #{const LOG}

info :: ErrorLevel
info = ErrorLevel #{const INFO}

notice :: ErrorLevel
notice = ErrorLevel #{const NOTICE}

warning :: ErrorLevel
warning = ErrorLevel #{const WARNING}

exception :: ErrorLevel
exception = ErrorLevel #{const ERROR}

fatal :: ErrorLevel
fatal = ErrorLevel #{const FATAL}

foreign import capi safe "plhaskell.h plhaskell_report"
  plhaskellReport :: ErrorLevel -> CString -> IO ()

report :: ErrorLevel -> Text -> PGm ()
report elevel msg = PGm $ pUseAsCString (encodeUtf8 msg) (plhaskellReport elevel)

raise :: ErrorLevel -> Text -> a
raise elevel msg = unsafePerformIO $ do
  unPGm $ report elevel msg
  undefined -- Never reached

raiseError :: Text -> a
raiseError = raise exception

raiseFatal :: Text -> a
raiseFatal = raise fatal

instance MonadFail PGm where
  fail = raiseError . pack

-- Parameter to pass to query
data QueryParam
  = QueryParamByteA (Maybe ByteString)
  | QueryParamText (Maybe Text)
  | QueryParamChar (Maybe Char)
  | QueryParamBool (Maybe Bool)
  | QueryParamInt2 (Maybe Int16)
  | QueryParamInt4 (Maybe Int32)
  | QueryParamInt8 (Maybe Int64)
  | QueryParamFloat4 (Maybe Float)
  | QueryParamFloat8 (Maybe Double)
  | QueryParamDate (Maybe Date)
  | QueryParamTime (Maybe Time)
  | QueryParamTimestamp (Maybe Timestamp)
  | QueryParamInterval (Maybe Interval)
  | QueryParamComposite (Maybe Text, Text) (Maybe [QueryParam])
  | QueryParamArray (Maybe Text, Text) (Maybe (Array QueryParam))

getNatts :: Ptr TupleTable -> IO Int16
getNatts = #{peek SPITupleTable, tupdesc} >=> #{peek TupleDescData, natts}

-- Get Oid base on the constructor or stated type
-- Does not verify contents comply with the Oid
getOid :: QueryParam -> IO Oid
getOid (QueryParamByteA _) = return #{const BYTEAOID}
getOid (QueryParamText _) = return #{const TEXTOID}
getOid (QueryParamChar _) = return #{const CHAROID}
getOid (QueryParamBool _) = return #{const BOOLOID}
getOid (QueryParamInt2 _) = return #{const INT2OID}
getOid (QueryParamInt4 _) = return #{const INT4OID}
getOid (QueryParamInt8 _) = return #{const INT8OID}
getOid (QueryParamFloat4 _) = return #{const FLOAT4OID}
getOid (QueryParamFloat8 _) = return #{const FLOAT8OID}
getOid (QueryParamDate _) = return #{const DATEOID}
getOid (QueryParamTime _) = return #{const TIMEOID}
getOid (QueryParamTimestamp _) = return #{const TIMESTAMPOID}
getOid (QueryParamInterval _) = return #{const INTERVALOID}
getOid (QueryParamComposite schemaType _) = getCompositeOid schemaType
getOid (QueryParamArray schemaType _) = getArrayOid schemaType

foreign import capi safe "error_plh.h expected_type"
  expectedType :: Oid -> IO ()

foreign import capi safe "error_plh.h expected_composite"
  expectedComposite :: IO ()

foreign import capi safe "error_plh.h expected_array"
  expectedArray :: IO ()

foreign import capi safe "error_plh.h expected_type_in_query"
  expectedTypeInQuery :: Ptr TypeInfo -> IO ()

foreign import capi safe "error_plh.h incorrect_length"
  incorrectLength :: Ptr TypeInfo -> IO ()

-- Verify that the TypeInfo oid is expected
-- return encoded Maybe Datum
encode' :: Ptr TypeInfo -> QueryParam -> IO (Maybe Datum)
encode' pTypeInfo (QueryParamByteA value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const BYTEAOID}) $ expectedType #{const BYTEAOID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamText value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const TEXTOID}) $ expectedType #{const TEXTOID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamChar value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const CHAROID}) $ expectedType #{const CHAROID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamBool value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const BOOLOID}) $ expectedType #{const BOOLOID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamInt2 value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const INT2OID}) $ expectedType #{const INT2OID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamInt4 value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const INT4OID}) $ expectedType #{const INT4OID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamInt8 value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const INT8OID}) $ expectedType #{const INT8OID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamFloat4 value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const FLOAT4OID}) $ expectedType #{const FLOAT4OID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamFloat8 value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const FLOAT8OID}) $ expectedType #{const FLOAT8OID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamDate value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const DATEOID}) $ expectedType #{const DATEOID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamTime value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const TIMEOID}) $ expectedType #{const TIMEOID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamTimestamp value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const TIMESTAMPOID}) $ expectedType #{const TIMESTAMPOID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamInterval value) = do
  oid <- getTypeOid pTypeInfo
  assert (oid == #{const INTERVALOID}) $ expectedType #{const INTERVALOID}
  encode pTypeInfo value
encode' pTypeInfo (QueryParamComposite schemaType mFields) = do
  valueType <- getValueType pTypeInfo
  assert (valueType == #{const COMPOSITE_TYPE}) expectedComposite
  oid <- getTypeOid pTypeInfo
  oid' <- getCompositeOid schemaType
  assert (oid == oid') $ expectedTypeInQuery pTypeInfo
  (flip maybeWrap) mFields $ \queryParamFields -> do
    count <- getCount pTypeInfo
    assert ((length queryParamFields) == (fromIntegral count)) $ incorrectLength pTypeInfo
    typeInfoFields <- getFields pTypeInfo
    zipWithM encode' typeInfoFields queryParamFields >>= writeComposite pTypeInfo
encode' pTypeInfo (QueryParamArray schemaType mElems) = do
  valueType <- getValueType pTypeInfo
  assert (valueType == #{const ARRAY_TYPE}) expectedArray
  oid <- getTypeOid pTypeInfo
  oid' <- getArrayOid schemaType
  assert (oid == oid') $ expectedTypeInQuery pTypeInfo
  (flip maybeWrap) mElems $ \queryParamElems -> do
    pElemTypeInfo <- getElement pTypeInfo
    arrayMapM (encode' pElemTypeInfo) queryParamElems >>= writeArray pTypeInfo

-- Value returned by query
data QueryResultValue
  = QueryResultValueByteA (Maybe ByteString)
  | QueryResultValueText (Maybe Text)
  | QueryResultValueChar (Maybe Char)
  | QueryResultValueBool (Maybe Bool)
  | QueryResultValueInt2 (Maybe Int16)
  | QueryResultValueInt4 (Maybe Int32)
  | QueryResultValueInt8 (Maybe Int64)
  | QueryResultValueFloat4 (Maybe Float)
  | QueryResultValueFloat8 (Maybe Double)
  | QueryResultValueDate (Maybe Date)
  | QueryResultValueTime (Maybe Time)
  | QueryResultValueTimestamp (Maybe Timestamp)
  | QueryResultValueInterval (Maybe Interval)
  | QueryResultValueComposite (Text, Text) (Maybe [QueryResultValue])
  | QueryResultValueArray (Text, Text) (Maybe (Array QueryResultValue))
  deriving stock (Show)

-- Various query results
data QueryResults
  = SelectResults Word64 [Text] [[QueryResultValue]]
  | SelectIntoResults Word64
  | InsertResults Word64
  | DeleteResults Word64
  | UpdateResults Word64
  | InsertReturningResults Word64 [Text] [[QueryResultValue]]
  | DeleteReturningResults Word64 [Text] [[QueryResultValue]]
  | UpdateReturningResults Word64 [Text] [[QueryResultValue]]
  | UtilityResults Word64
  | RewrittenResults Word64
  deriving stock (Show)

foreign import capi safe "spi_plh.h get_header_field"
  cGetHeaderField :: Ptr TupleTable -> CString -> CInt -> IO ()

getHeaderField :: Ptr TupleTable -> Int16 -> IO Text
getHeaderField pTupleTable fnumber = allocaArray #{const NAMEDATALEN} $ \pName -> do
  cGetHeaderField pTupleTable pName (fromIntegral fnumber)
  pack <$> peekCString pName

getHeader :: Ptr TupleTable -> IO [Text]
getHeader pTupleTable = do
  natts <- getNatts pTupleTable
  mapM (getHeaderField pTupleTable) [1 .. natts]

foreign import capi safe "spi_plh.h get_oids"
  cGetOids :: Ptr TupleTable -> Ptr Oid -> IO ()

getOids :: Ptr TupleTable -> IO [Oid]
getOids pTupleTable = do
  natts <- getNatts pTupleTable
  allocaArray (fromIntegral natts) $ \oids -> do
    cGetOids pTupleTable oids
    mapM (peekElemOff oids) (range $ fromIntegral natts)

foreign import capi safe "plhaskell.h new_type_info"
  newTypeInfo :: Oid -> IO (Ptr TypeInfo)

foreign import capi safe "plhaskell.h delete_type_info"
  deleteTypeInfo :: Ptr TypeInfo -> IO ()

foreign import capi safe "spi_plh.h get_tuple_datum"
  cGetTupleDatum :: Ptr TupleTable -> Word64 -> CInt -> Ptr CBool -> IO Datum

getTupleDatum :: Ptr TupleTable -> Word64 -> CInt -> IO (Maybe Datum)
getTupleDatum pTupleTable rowNumber fnumber = pWithArray [0] $ \pIsNull -> do
  datum <- cGetTupleDatum pTupleTable rowNumber fnumber pIsNull
  isNull <- peek pIsNull
  if (toBool isNull)
    then return Nothing
    else return $ Just datum

decode' :: Ptr TypeInfo -> Maybe Datum -> IO QueryResultValue
decode' pTypeInfo mDatum = do
  valueType <- getValueType pTypeInfo
  case valueType of
    #{const BASE_TYPE} -> do
      typeOid <- getTypeOid pTypeInfo
      case typeOid of
        #{const BYTEAOID} -> QueryResultValueByteA <$> decode mDatum
        #{const TEXTOID} -> QueryResultValueText <$> decode mDatum
        #{const CHAROID} -> QueryResultValueChar <$> decode mDatum
        #{const BOOLOID} -> QueryResultValueBool <$> decode mDatum
        #{const INT2OID} -> QueryResultValueInt2 <$> decode mDatum
        #{const INT4OID} -> QueryResultValueInt4 <$> decode mDatum
        #{const INT8OID} -> QueryResultValueInt8 <$> decode mDatum
        #{const FLOAT4OID} -> QueryResultValueFloat4 <$> decode mDatum
        #{const FLOAT8OID} -> QueryResultValueFloat8 <$> decode mDatum
        #{const DATEOID} -> QueryResultValueDate <$> decode mDatum
        #{const TIMEOID} -> QueryResultValueTime <$> decode mDatum
        #{const TIMESTAMPOID} -> QueryResultValueTimestamp <$> decode mDatum
        #{const INTERVALOID} -> QueryResultValueInterval <$> decode mDatum
        _ -> undefined
    #{const COMPOSITE_TYPE} -> do
      schemaType <- getSchemaType pTypeInfo
      case mDatum of
        Nothing -> return $ QueryResultValueComposite schemaType Nothing
        Just datum -> do
          fieldPTypeInfos <- getFields pTypeInfo
          fieldMDatums <- readComposite pTypeInfo datum
          (QueryResultValueComposite schemaType) . Just <$> zipWithM decode' fieldPTypeInfos fieldMDatums
    #{const ARRAY_TYPE} -> do
      pElemTypeInfo <- getElement pTypeInfo
      schemaType <- getSchemaType pElemTypeInfo
      case mDatum of
        Nothing -> return $ QueryResultValueArray schemaType Nothing
        Just datum -> (QueryResultValueArray schemaType) . Just <$> (readArray pTypeInfo datum >>= arrayMapM (decode' pElemTypeInfo))
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

foreign import capi safe "spi_plh.h run_query"
  runQuery :: CString -> CInt -> Ptr Oid -> Ptr Datum -> Ptr CBool -> IO CInt

foreign import ccall safe "executor/spi.h &SPI_processed"
  pSPIProcessed :: Ptr Word64

foreign import ccall safe "executor/spi.h &SPI_tuptable"
  pSPITupTable :: Ptr (Ptr TupleTable)

foreign import capi safe "executor/spi.h SPI_freetuptable"
  freeTupTable :: Ptr TupleTable -> IO ()

getIsNull :: Maybe Datum -> CBool
getIsNull Nothing = fromBool True
getIsNull (Just _) = fromBool False

getValue :: Maybe Datum -> Datum
getValue Nothing = voidDatum
getValue (Just datum) = datum

query :: Text -> [QueryParam] -> PGm QueryResults
query q params = PGm $ do
  oids <- mapM getOid params
  pTypeInfos <- mapM newTypeInfo oids
  mDatums <- zipWithM encode' pTypeInfos params
  mapM_ deleteTypeInfo pTypeInfos
  let isNulls = map getIsNull mDatums
  let values = map getValue mDatums
  pUseAsCString (encodeUtf8 q) $ \ptrQuery -> do
    pWithArrayLen oids $ \nargs ptrArgtypes -> do
      pWithArray values $ \ptrValues -> do
        pWithArray isNulls $ \ptrIsNulls -> do
          spiCode <- runQuery ptrQuery (fromIntegral nargs) ptrArgtypes ptrValues ptrIsNulls
          processed <- peek pSPIProcessed
          pTupleTable <- peek pSPITupTable
          queryResult <- case spiCode of
            #{const SPI_OK_SELECT} -> do
              header <- getHeader pTupleTable
              rows <- getRows pTupleTable processed
              return (SelectResults processed header rows)
            #{const SPI_OK_SELINTO} -> return (SelectIntoResults processed)
            #{const SPI_OK_INSERT} -> return (InsertResults processed)
            #{const SPI_OK_DELETE} -> return (DeleteResults processed)
            #{const SPI_OK_UPDATE} -> return (UpdateResults processed)
            #{const SPI_OK_INSERT_RETURNING} -> do
              header <- getHeader pTupleTable
              rows <- getRows pTupleTable processed
              return (InsertReturningResults processed header rows)
            #{const SPI_OK_DELETE_RETURNING} -> do
              header <- getHeader pTupleTable
              rows <- getRows pTupleTable processed
              return (DeleteReturningResults processed header rows)
            #{const SPI_OK_UPDATE_RETURNING} -> do
              header <- getHeader pTupleTable
              rows <- getRows pTupleTable processed
              return (UpdateReturningResults processed header rows)
            #{const SPI_OK_UTILITY} -> return (UtilityResults processed)
            #{const SPI_OK_REWRITTEN} -> return (RewrittenResults processed)
            _ -> undefined
          freeTupTable pTupleTable
          return queryResult

getSchemaType :: Ptr TypeInfo -> IO (Text, Text)
getSchemaType pTypeInfo = do
  nspname <- #{peek TypeInfo, nspname} pTypeInfo >>= peekCString
  typname <- #{peek TypeInfo, typname} pTypeInfo >>= peekCString
  return (pack nspname, pack typname)

foreign import capi safe "plhaskell.h get_oid"
  cGetOid :: CBool -> CString -> CString -> IO Oid

foreign import capi safe "plhaskell.h find_oid"
  cFindOid :: CBool -> CString -> IO Oid

getCompositeOid :: (Maybe Text, Text) -> IO Oid
getCompositeOid (Nothing, typname) = pWithCString (unpack typname) $ cFindOid (fromBool False)
getCompositeOid (Just nspname, typname) = pWithCString2 (unpack nspname) (unpack typname) $ cGetOid (fromBool False)

getArrayOid :: (Maybe Text, Text) -> IO Oid
getArrayOid (Nothing, typname) = pWithCString (unpack typname) $ cFindOid (fromBool True)
getArrayOid (Just nspname, typname) = pWithCString2 (unpack nspname) (unpack typname) $ cGetOid (fromBool True)

foreign import capi safe "spi_plh.h commit_rollback"
  commitRollback :: CBool -> CBool -> PGm ()

commit :: Bool -> PGm ()
commit = commitRollback (fromBool True) . fromBool

rollback :: Bool -> PGm ()
rollback = commitRollback (fromBool False) . fromBool

transactionTimestampUTC :: PGm Timestamp
transactionTimestampUTC = PGm transactionTimestampUTC'

statementTimestampUTC :: PGm Timestamp
statementTimestampUTC = PGm statementTimestampUTC'

lock :: (Lockable a) => LockMode -> LockLevel -> a -> PGm ()
lock lockMode lockLevel key = PGm $ lock' lockMode lockLevel key

tryLock :: (Lockable a) => LockMode -> LockLevel -> a -> PGm Bool
tryLock lockMode lockLevel key = PGm $ tryLock' lockMode lockLevel key

unlock :: (Lockable a) => LockMode -> a -> PGm Bool
unlock lockMode key = PGm $ unlock' lockMode key

unlockAll :: PGm ()
unlockAll = PGm unlockAll'
