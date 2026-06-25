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

#{include "../plhaskell.h"}

#{include "catalog/pg_type_d.h"}

module PLHaskell () where

import Control.Exception
  ( SomeException,
    handle,
  )
import Control.Monad
  ( mapM,
    mapM_,
    void,
    zipWithM,
    (>=>),
  )
import Data.Functor
  ( (<$>),
  )
import Data.Int
  ( Int16,
  )
import Data.List
  ( intercalate,
    isPrefixOf,
  )
import Data.Maybe
  ( fromMaybe,
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
import Foreign.Marshal.Utils
  ( fromBool,
    toBool,
  )
import Foreign.Ptr
  ( Ptr,
    nullPtr,
    plusPtr,
    ptrToWordPtr,
  )
import Foreign.StablePtr
  ( deRefStablePtr,
    freeStablePtr,
    newStablePtr,
  )
import Foreign.Storable
  ( peek,
    peekByteOff,
    peekElemOff,
    poke,
  )
import Language.Haskell.Interpreter
  ( Extension
      ( OverloadedStrings,
        Safe
      ),
    ImportList
      ( ImportList
      ),
    Interpreter,
    InterpreterError
      ( GhcException,
        NotAllowed,
        UnknownError,
        WontCompile
      ),
    ModuleImport
      ( ModuleImport
      ),
    ModuleQualification
      ( NotQualified,
        QualifiedAs
      ),
    OptionVal
      ( (:=)
      ),
    errMsg,
    installedModulesInScope,
    languageExtensions,
    liftIO,
    loadModules,
    runStmt,
    set,
    setImportsF,
    typeChecks,
  )
import Language.Haskell.Interpreter.Unsafe
  ( unsafeRunInterpreterWithArgs,
  )
import PGutils.Common
  ( Datum
      ( Datum
      ),
    NullableDatum,
    Oid
      ( Oid
      ),
    TypeInfo,
    assert,
    getCount,
    getElement,
    getFields,
    getTypeOid,
    getValueType,
    handler,
    numRange,
    pWithCString,
    voidDatum,
  )
import System.Directory
  ( removeFile,
  )
import Text.Read
  ( readMaybe,
  )
import Prelude
  ( Bool
      ( False,
        True
      ),
    Either
      ( Left,
        Right
      ),
    IO,
    Int,
    Maybe
      ( Just,
        Nothing
      ),
    String,
    concat,
    concatMap,
    const,
    fromIntegral,
    length,
    map,
    not,
    null,
    return,
    show,
    undefined,
    ($),
    (&&),
    (++),
    (-),
    (.),
    (==),
    (>>),
    (>>=),
    (||),
  )

-- Dummy type to make pointers
data CallInfo

-- Replace all instances of ? with i
interpolate :: String -> Int16 -> String
interpolate "" _n = ""
interpolate ('?' : ss) n = show n ++ interpolate ss n
interpolate (s : ss) n = s : interpolate ss n

getDataType :: Oid -> Maybe String
getDataType #{const BYTEAOID} = Just "ByteString"
getDataType #{const TEXTOID} = Just "Text"
getDataType #{const BPCHAROID} = Just "Char"
getDataType #{const BOOLOID} = Just "Bool"
getDataType #{const INT2OID} = Just "Int16"
getDataType #{const INT4OID} = Just "Int32"
getDataType #{const INT8OID} = Just "Int64"
getDataType #{const FLOAT4OID} = Just "Float"
getDataType #{const FLOAT8OID} = Just "Double"
getDataType #{const DATEOID} = Just "Date"
getDataType #{const TIMEOID} = Just "Time"
getDataType #{const TIMESTAMPOID} = Just "Timestamp"
getDataType #{const INTERVALOID} = Just "Interval"
getDataType _oid = Nothing

-- Is a type supported
typeAvailable :: Oid -> Bool
typeAvailable oid = not $ null $ getDataType oid

-- Run the function pointed to by the stable pointer
foreign export capi "type_available" cTypeAvailable :: Oid -> CBool

cTypeAvailable :: Oid -> CBool
cTypeAvailable oid = CBool $ fromBool $ typeAvailable oid

-- Name of corresponding Haskell type
baseName :: Oid -> String
baseName oid = fromMaybe undefined (getDataType oid)

-- Extract module file name from CallInfo
getModFileName :: Ptr CallInfo -> Interpreter String
getModFileName pCallInfo = liftIO $ #{peek CallInfo, mod_file_name} pCallInfo >>= peekCString

-- Extract function name from CallInfo
getFuncName :: Ptr CallInfo -> Interpreter String
getFuncName pCallInfo = liftIO $ #{peek CallInfo, func_name} pCallInfo >>= peekCString

setPtr :: String -> Ptr a -> Interpreter ()
setPtr name ptr = runStmt $ "let " ++ name ++ " = wordPtrToPtr " ++ show (ptrToWordPtr ptr)

-- Get argument TypeInfo from CallInfo
getArgTypeInfo :: Ptr CallInfo -> Int16 -> IO (Ptr TypeInfo)
getArgTypeInfo pCallInfo i = do
  pArgs <- #{peek CallInfo, args} pCallInfo
  peekElemOff pArgs (fromIntegral i)

-- Get type signature of function needed based on CallInfo
getSignature :: Ptr CallInfo -> Interpreter String
getSignature pCallInfo = liftIO $ do
  nargs <- #{peek CallInfo, nargs} pCallInfo
  argTypeNames <- mapM (getArgTypeInfo pCallInfo >=> getMTypeName) $ numRange nargs
  resultTypeName <- #{peek CallInfo, result} pCallInfo >>= getMTypeName

  CBool trusted <- #{peek CallInfo, trusted} pCallInfo
  CBool returnSet <- #{peek CallInfo, return_set} pCallInfo
  if toBool trusted
    then
      if toBool returnSet
        then return $ intercalate " -> " (argTypeNames ++ ["PGm [" ++ resultTypeName ++ "]"])
        else return $ intercalate " -> " (argTypeNames ++ ["PGm (" ++ resultTypeName ++ ")"])
    else
      if toBool returnSet
        then return $ intercalate " -> " (argTypeNames ++ ["IO [" ++ resultTypeName ++ "]"])
        else return $ intercalate " -> " (argTypeNames ++ ["IO (" ++ resultTypeName ++ ")"])
  where
    -- Get Haskell type name based on TypeInfo
    getMTypeName :: Ptr TypeInfo -> IO String
    getMTypeName pTypeInfo = do
      valueType <- getValueType pTypeInfo
      case valueType of
        #{const VOID_TYPE} -> return "()"
        _ -> do
          typeName <- getTypeName pTypeInfo
          return $ "Maybe (" ++ typeName ++ ")"
    getTypeName :: Ptr TypeInfo -> IO String
    getTypeName pTypeInfo = do
      valueType <- getValueType pTypeInfo
      case valueType of
        #{const BASE_TYPE} -> do
          typeOid <- getTypeOid pTypeInfo
          return $ baseName typeOid
        #{const COMPOSITE_TYPE} -> do
          names <- getFields pTypeInfo >>= (mapM getMTypeName)
          return $ intercalate ", " names
        #{const ARRAY_TYPE} -> do
          elemName <- getElement pTypeInfo >>= getMTypeName
          return $ "Array (" ++ elemName ++ ")"
        #{const RANGE_TYPE} -> do
          elemName <- getElement pTypeInfo >>= getTypeName
          return $ "Range (" ++ elemName ++ ")"
        #{const MULTIRANGE_TYPE} -> do
          elemName <- getElement pTypeInfo >>= getTypeName
          return $ "MultiRange (" ++ elemName ++ ")"
        _ -> undefined

foreign import capi safe "../plhaskell.h error_func_sig"
  cErrorFuncSig :: CString -> IO ()

errorFuncSig :: String -> IO ()
errorFuncSig funcSig = pWithCString funcSig cErrorFuncSig

-- Set up interpreter to evaluate a function
setUpEvalInt :: Ptr CallInfo -> Interpreter ([Int16], String, Bool)
setUpEvalInt pCallInfo = do
  set [languageExtensions := [OverloadedStrings, Safe], installedModulesInScope := False]
  modFileName <- getModFileName pCallInfo
  loadModules [modFileName]

  funcName <- getFuncName pCallInfo
  setImportsF
    [ ModuleImport "Prelude" NotQualified (ImportList ["Bool(False, True)", "Char", "Double", "Float", "IO", "Maybe(Just, Nothing)", "return", "($)", "(.)", "(>>=)", "mapM"]),
      ModuleImport "Control.Exception" NotQualified (ImportList ["handle"]),
      ModuleImport "Control.Monad" NotQualified (ImportList ["(>=>)"]),
      ModuleImport "Data.ByteString" NotQualified (ImportList ["ByteString"]),
      ModuleImport "Data.Int" NotQualified (ImportList ["Int16", "Int32", "Int64"]),
      ModuleImport "Data.Text" NotQualified (ImportList ["Text"]),
      ModuleImport "Foreign.Ptr" NotQualified (ImportList ["Ptr", "wordPtrToPtr"]),
      ModuleImport "Foreign.StablePtr" NotQualified (ImportList ["newStablePtr"]),
      ModuleImport "Foreign.Storable" NotQualified (ImportList ["peekElemOff", "poke"]),
      ModuleImport "PGutils.Common" NotQualified (ImportList ["handler", "unNullableDatum"]),
      ModuleImport "PGutils" NotQualified (ImportList ["PGm", "unPGm"]),
      ModuleImport "PGutils.Support" NotQualified (ImportList ["Datum", "BaseType (encode, decode)", "encodeVoid", "maybeWrap", "readComposite", "wrapFunction", "writeResult", "mkResultList", "writeComposite"]),
      ModuleImport "PGutils.Array" NotQualified (ImportList ["Array", "readArray", "writeArray"]),
      ModuleImport "PGutils.Range" NotQualified (ImportList ["Range", "MultiRange", "rangeMapMm", "readRange", "writeRange", "multiRangeMapMm", "readMultiRange", "writeMultiRange"]),
      ModuleImport "PGutils.Datetime" NotQualified (ImportList ["Date", "Time", "Timestamp", "Interval"]),
      ModuleImport "PGutils.Func" (QualifiedAs Nothing) (ImportList [funcName])
    ]

  CBool trusted <- liftIO $ #{peek CallInfo, trusted} pCallInfo

  -- Check signature
  signature <- getSignature pCallInfo
  r <- typeChecks $ "PGutils.Func." ++ funcName ++ "::" ++ signature
  liftIO $ assert r $ errorFuncSig $ funcName ++ " :: " ++ signature

  -- Number of arguments
  nargs <- liftIO $ #{peek CallInfo, nargs} pCallInfo
  let argIndexes = numRange nargs

  -- Fill all decodeArg? values with functions to decode arguments
  mapM_ defineDecodeArg argIndexes

  -- Fill encodeResult value with function to return Maybe Datum
  pResultTypeInfo <- liftIO $ #{peek CallInfo, result} pCallInfo
  encodeResultDef <- liftIO $ makeEncodeResultDef pResultTypeInfo
  runStmt $ "let encodeResult = " ++ encodeResultDef

  return (argIndexes, funcName, toBool trusted)
  where
    -- Return a string representing a function to take an argument from a Maybe Datum and return the Haskell value
    -- returned code :: Maybe Datum -> IO (Maybe a)
    -- where a is the type represented by TypeInfo
    decodeFieldDef j fieldPTypeInfo = do
      def <- makeDecodeArgDef fieldPTypeInfo
      return $ interpolate ("field? <- " ++ def ++ " fieldMDatum?;") j
    makeDecodeArgDef pTypeInfo =
      do
        let pTypeInfoAddr = "(wordPtrToPtr " ++ show (ptrToWordPtr pTypeInfo) ++ ")"
        valueType <- getValueType pTypeInfo
        case valueType of
          #{const BASE_TYPE} -> do
            typeOid <- getTypeOid pTypeInfo
            return $ "(decode :: Maybe Datum -> IO (Maybe " ++ baseName typeOid ++ "))"
          #{const COMPOSITE_TYPE} -> do
            fieldIndexes <- numRange <$> getCount pTypeInfo
            decodeFieldDefs <- getFields pTypeInfo >>= zipWithM decodeFieldDef [0 ..]
            let fieldDatumsList = "[" ++ (intercalate ", " (map (interpolate "fieldMDatum?") fieldIndexes)) ++ "]"
            let fieldsTuple = "(" ++ (intercalate ", " (map (interpolate "field?") fieldIndexes)) ++ ")"
            return $
              "(maybeWrap $ \\datum -> do {"
                ++ fieldDatumsList
                ++ " <- readComposite "
                ++ pTypeInfoAddr
                ++ " datum;"
                ++ concat decodeFieldDefs
                ++ "return "
                ++ fieldsTuple
                ++ ";})"
          #{const ARRAY_TYPE} -> do
            decodeResultDefElem <- getElement pTypeInfo >>= makeDecodeArgDef
            return $ "(maybeWrap $ readArray " ++ pTypeInfoAddr ++ " >=> mapM " ++ decodeResultDefElem ++ ")"
          #{const RANGE_TYPE} -> do
            decodeResultDefElem <- getElement pTypeInfo >>= makeDecodeArgDef
            return $ "(maybeWrap $ readRange " ++ pTypeInfoAddr ++ " >=> rangeMapMm " ++ decodeResultDefElem ++ ")"
          #{const MULTIRANGE_TYPE} -> do
            decodeResultDefElem <- getElement pTypeInfo >>= makeDecodeArgDef
            return $ "(maybeWrap $ readMultiRange " ++ pTypeInfoAddr ++ " >=> multiRangeMapMm " ++ decodeResultDefElem ++ ")"
          _ -> undefined
    -- Return a string representing a function to take the result from a Haskell value and return Maybe Datum
    -- returned code :: Maybe a -> IO (Maybe Datum)
    -- where a is the type represented by TypeInfo
    encodeFieldDef j fieldPTypeInfo = do
      def <- makeEncodeResultDef fieldPTypeInfo
      return $ interpolate ("fieldMDatum? <- " ++ def ++ " field?;") j
    makeEncodeResultDef pTypeInfo =
      do
        let pTypeInfoAddr = "(wordPtrToPtr " ++ show (ptrToWordPtr pTypeInfo) ++ ")"
        valueType <- getValueType pTypeInfo
        case valueType of
          #{const VOID_TYPE} -> return "encodeVoid"
          #{const BASE_TYPE} -> do
            typeOid <- getTypeOid pTypeInfo
            return $ "((encode " ++ pTypeInfoAddr ++ ") :: Maybe " ++ baseName typeOid ++ " -> IO (Maybe Datum))"
          #{const COMPOSITE_TYPE} -> do
            fieldIndexes <- numRange <$> getCount pTypeInfo
            encodeFieldDefs <- getFields pTypeInfo >>= zipWithM encodeFieldDef [0 ..]
            let fieldDatumsList = " [" ++ (intercalate ", " (map (interpolate "fieldMDatum?") fieldIndexes)) ++ "]"
            let fieldsTuple = "(" ++ (intercalate ", " (map (interpolate "field?") fieldIndexes)) ++ ")"
            return $
              "(maybeWrap $ \\"
                ++ fieldsTuple
                ++ " -> do {"
                ++ concat encodeFieldDefs
                ++ "writeComposite "
                ++ pTypeInfoAddr
                ++ fieldDatumsList
                ++ "})"
          #{const ARRAY_TYPE} -> do
            encodeResultDefElem <- getElement pTypeInfo >>= makeEncodeResultDef
            return $ "(maybeWrap $ mapM " ++ encodeResultDefElem ++ " >=> writeArray " ++ pTypeInfoAddr ++ ")"
          #{const RANGE_TYPE} -> do
            encodeResultDefElem <- getElement pTypeInfo >>= makeEncodeResultDef
            return $ "(maybeWrap $ rangeMapMm " ++ encodeResultDefElem ++ " >=> writeRange " ++ pTypeInfoAddr ++ ")"
          #{const MULTIRANGE_TYPE} -> do
            encodeResultDefElem <- getElement pTypeInfo >>= makeEncodeResultDef
            return $ "(maybeWrap $ multiRangeMapMm " ++ encodeResultDefElem ++ " >=> writeMultiRange " ++ pTypeInfoAddr ++ ")"
          _ -> undefined
    defineDecodeArg i = do
      decodeArg <- liftIO $ getArgTypeInfo pCallInfo i >>= makeDecodeArgDef
      runStmt $ interpolate ("let decodeArg? = " ++ decodeArg) i

foreign import capi safe "../error_plh.h language_error"
  cLanguageError :: CInt -> CString -> IO ()

languageError :: String -> IO ()
languageError msg = (pWithCString msg) (cLanguageError #{const ERROR})

foreign import ccall safe "&pkglib_path"
  pPkgLibPath :: CString

-- Execute an interpreter monad and handle the result
execute :: CString -> Ptr CallInfo -> Interpreter () -> IO ()
execute pPackagePath pCallInfo int = do
  pkgLibPath <- peekCString pPkgLibPath
  packagePath <- peekCString pPackagePath
  modFileName <- #{peek CallInfo, mod_file_name} pCallInfo >>= peekCString
  funcName <- #{peek CallInfo, func_name} pCallInfo >>= peekCString
  r <- unsafeRunInterpreterWithArgs (mkInterpreterArgs pkgLibPath packagePath) int
  case r of
    Left (UnknownError msg) -> removeModFile >> (languageError $ cleanErr modFileName funcName msg)
    Left (WontCompile errs) -> removeModFile >> (languageError $ concatMap (cleanErr modFileName funcName . errMsg) errs)
    Left (NotAllowed msg) -> removeModFile >> (languageError $ cleanErr modFileName funcName msg)
    Left (GhcException msg) -> removeModFile >> (languageError $ cleanErr modFileName funcName msg)
    Right () -> void removeModFile
  where
    removeModFile = handle ignore $ #{peek CallInfo, mod_file_name} pCallInfo >>= peekCString >>= removeFile
    ignore :: SomeException -> IO ()
    ignore = const $ return ()
    cleanErr :: String -> String -> String -> String
    cleanErr modFileName funcName err = '\n' : (intercalate ":" $ subFuncName modFileName funcName $ splitOnColon err)
    subFuncName modFileName funcName (fileName : t) = if (fileName == modFileName) then funcName : (subLineNum t) else fileName : (subLineNum t)
    subFuncName _modFileName _funcName [] = []
    subLineNum (lineNum : t) = case (readMaybe lineNum :: Maybe Int) of
      Nothing -> lineNum : t
      Just lineNum' -> (show (lineNum' - 1)) : t
    subLineNum [] = []

mkInterpreterArgs :: String -> String -> [String]
mkInterpreterArgs pkgLibPath "" = ["-clear-package-db", "-package-db", pkgLibPath ++ "/plhaskell_pkg_db"]
mkInterpreterArgs pkgLibPath ":" = ["-clear-package-db", "-package-db", pkgLibPath ++ "/plhaskell_pkg_db", "-global-package-db"]
mkInterpreterArgs pkgLibPath q = ["-clear-package-db", "-package-db", pkgLibPath ++ "/plhaskell_pkg_db"] ++ (mkInterpreterArgs' $ splitOnColon q)
  where
    mkInterpreterArgs' [] = []
    mkInterpreterArgs' ("" : zs) = "-global-package-db" : mkInterpreterArgs' zs
    mkInterpreterArgs' (z : zs) = "-package-db" : interpolate' z : mkInterpreterArgs' zs
    interpolate' z@('$' : _) = pkgLibPath ++ trim (length "$pkglibdir") z
    interpolate' z = z
    trim 0 t = t
    trim n (_t : ts) = trim (n - 1) ts
    trim _ "" = ""

-- Check the type signature of the function against what is expected
-- Raise and Error if they don't match
foreign export capi "check_signature" checkSignature :: CString -> Ptr CallInfo -> IO ()

checkSignature :: CString -> Ptr CallInfo -> IO ()
checkSignature pPackagePath pCallInfo = execute pPackagePath pCallInfo $ do
  set [languageExtensions := [OverloadedStrings, Safe], installedModulesInScope := False]
  modFileName <- getModFileName pCallInfo
  loadModules [modFileName]

  funcName <- getFuncName pCallInfo
  setImportsF
    [ ModuleImport "Prelude" NotQualified (ImportList ["Bool", "Char", "Double", "Float", "Maybe", "IO"]),
      ModuleImport "Data.ByteString" NotQualified (ImportList ["ByteString"]),
      ModuleImport "Data.Int" NotQualified (ImportList ["Int16", "Int32", "Int64"]),
      ModuleImport "Data.Text" NotQualified (ImportList ["Text"]),
      ModuleImport "PGutils" NotQualified (ImportList ["PGm"]),
      ModuleImport "PGutils.Array" NotQualified (ImportList ["Array"]),
      ModuleImport "PGutils.Range" NotQualified (ImportList ["Range", "MultiRange"]),
      ModuleImport "PGutils.Datetime" NotQualified (ImportList ["Date", "Time", "Timestamp", "Interval"]),
      ModuleImport "PGutils.Func" (QualifiedAs Nothing) (ImportList [funcName])
    ]

  signature <- getSignature pCallInfo
  r <- typeChecks $ "PGutils.Func." ++ funcName ++ "::" ++ signature
  liftIO $ assert r $ errorFuncSig $ funcName ++ " :: " ++ signature

-- Set the Function field of the CallInfo to a function that
-- will read the arguments, call the function, and write the result
foreign export capi "mk_function" mkFunction :: CString -> Ptr CallInfo -> IO ()

mkFunction :: CString -> Ptr CallInfo -> IO ()
mkFunction pPackagePath pCallInfo = execute pPackagePath pCallInfo $ do
  (argIndexes, funcName, trusted) <- setUpEvalInt pCallInfo

  -- Build the Function
  let prog_decode_args = concatMap (interpolate "arg? <- peekElemOff pArgs ? >>= return . unNullableDatum >>= decodeArg?;") argIndexes
  let argsNames = concatMap (interpolate " arg?") argIndexes
  let prog_call =
        if trusted
          then "result <- unPGm $ PGutils.Func." ++ funcName ++ argsNames ++ ";"
          else "result <-         PGutils.Func." ++ funcName ++ argsNames ++ ";"
  let prog_encode_result = "encodeResult result >>= writeResult pResultIsNull"
  runStmt $ "function <- wrapFunction $ (\\pArgs pResultIsNull -> handle handler $ do {" ++ prog_decode_args ++ prog_call ++ prog_encode_result ++ "})"

  -- Poke the value of the pointer into the Function field of the CallInfo
  setPtr "pFunction" $ #{ptr CallInfo, function} pCallInfo
  runStmt "poke pFunction function"

-- Set the List field of the CallInfo to the list returns by the function
foreign export capi "mk_list" mkList :: CString -> Ptr CallInfo -> Ptr NullableDatum -> IO ()

mkList :: CString -> Ptr CallInfo -> Ptr NullableDatum -> IO ()
mkList pPackagePath pCallInfo pArgs = execute pPackagePath pCallInfo $ do
  (argIndexes, funcName, trusted) <- setUpEvalInt pCallInfo

  -- Get the arguments
  setPtr "pArgs" pArgs
  mapM_ (runStmt . (interpolate "arg? <- peekElemOff pArgs ? >>= return . unNullableDatum >>= decodeArg?")) argIndexes

  -- Set returnResultList to be a list of actions each of which loads a result into the result TypeInfo
  let argsNames = concatMap (interpolate " arg?") argIndexes
  if trusted
    then runStmt $ "results <- unPGm $ PGutils.Func." ++ funcName ++ argsNames
    else runStmt $ "results <-         PGutils.Func." ++ funcName ++ argsNames

  runStmt "let returnResultList = mkResultList encodeResult results"

  -- poke the stable pointer value into the List field of the CallInfo
  runStmt "spList <- newStablePtr returnResultList"
  setPtr "pList" $ #{ptr CallInfo, list} pCallInfo
  runStmt "poke pList spList"

foreign export capi "iterate" iterate :: CString -> Ptr CallInfo -> Ptr CBool -> IO Datum

iterate :: CString -> Ptr CallInfo -> Ptr CBool -> IO Datum
iterate _pPackagePath pCallInfo pResultIsNull = handle handler $ do
  let pList = #{ptr CallInfo, list} pCallInfo
  spList <- peek pList
  returnResultList <- deRefStablePtr spList
  freeStablePtr spList
  poke pList nullPtr
  case returnResultList of
    [] -> return voidDatum
    (returnResult : tail) -> do
      (newStablePtr tail) >>= (poke pList)
      returnResult pResultIsNull

splitOnColon :: String -> [String]
splitOnColon "" = [""]
splitOnColon (':' : xs) = "" : splitOnColon xs
splitOnColon (x : xs) = case splitOnColon xs of
  [] -> undefined -- splitOnColon never returns an empty list
  y : ys -> (x : y) : ys

isValidPackagePath :: String -> Bool
isValidPackagePath "" = True
isValidPackagePath ":" = True
isValidPackagePath q = isValidPackagePath' $ splitOnColon q
  where
    isValidPackagePath' [] = undefined -- splitOnColon never returns an empty list
    isValidPackagePath' [p] = isValidPath p
    isValidPackagePath' [p, ""] = isValidPath p
    isValidPackagePath' (p : ps) = isValidPath p && isValidPackagePath' ps
    isValidPath "$pkglibdir" = True
    isValidPath p = isPrefixOf "$pkglibdir/" p || isPrefixOf "/" p

foreign export capi "is_valid_package_path" cIsValidPackagePath :: CString -> IO CBool

cIsValidPackagePath :: CString -> IO CBool
cIsValidPackagePath pQ = (fromBool . isValidPackagePath) <$> peekCString pQ
