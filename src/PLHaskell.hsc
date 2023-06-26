{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
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

module PLHaskell () where

import Control.Monad                (forM, forM_, (>=>))
import Data.Int                     (Int16, Int32)
import Data.List                    (intercalate)
import Data.Maybe                   (fromMaybe)
import Data.Word                    (Word16)
import Foreign.C.String             (CString, peekCString)
import Foreign.C.Types              (CBool (CBool), CInt (CInt), CUInt (CUInt))
import Foreign.Marshal.Utils        (fromBool, toBool)
import Foreign.Ptr                  (Ptr, plusPtr, ptrToWordPtr)
import Foreign.Storable             (Storable, peekByteOff, peekElemOff)
import Language.Haskell.Interpreter (Extension (OverloadedStrings, Safe), ImportList (ImportList), Interpreter, InterpreterError (GhcException, NotAllowed, UnknownError, WontCompile), ModuleImport (ModuleImport), ModuleQualification (NotQualified, QualifiedAs), OptionVal ((:=)), errMsg, ghcVersion, installedModulesInScope, languageExtensions, liftIO, loadModules, runInterpreter, runStmt, set, setImportsF, typeChecks)
import Prelude                      (Bool (False), Either (Left, Right), Eq, IO, Maybe (Just, Nothing), Num, String, concat, concatMap, fromIntegral, map, not, null, return, show, undefined, ($), (++), (-), (.), (>>=))

import MemoryUtils                  (pWithCString)

-- Dummy types to make pointers
data CallInfo
data ValueInfo
newtype Oid = Oid CUInt deriving newtype (Eq, Num, Storable)

-- Replace all instances of ? with i
interpolate :: String -> Int16 -> String
interpolate "" _ = ""
interpolate ('?':ss) i = show i ++ interpolate ss i
interpolate (s:ss) i = s : interpolate ss i

-- Function to record message or raise exception
foreign import capi safe "plhaskell.h plhaskell_report"
    plhaskellReport :: CInt -> CString -> IO ()

raise :: CInt -> String -> IO ()
raise level msg = pWithCString msg (plhaskellReport level)

raiseError :: String -> IO ()
raiseError = raise (#const ERROR)

getDataType :: Oid -> Maybe String
getDataType (#const BYTEAOID)  = Just "ByteString"
getDataType (#const TEXTOID)   = Just "Text"
getDataType (#const BPCHAROID) = Just "Char"
getDataType (#const BOOLOID)   = Just "Bool"
getDataType (#const INT2OID)   = Just "Int16"
getDataType (#const INT4OID)   = Just "Int32"
getDataType (#const INT8OID)   = Just "Int64"
getDataType (#const FLOAT4OID) = Just "Float"
getDataType (#const FLOAT8OID) = Just "Double"
getDataType _oid = Nothing

-- Is a type supported
typeAvailable :: Oid -> Bool
typeAvailable oid = not $ null $ getDataType oid

-- Run the function pointed to by the stable pointer
foreign export capi "type_available" c_typeAvailable :: Oid -> IO CBool
c_typeAvailable :: Oid -> IO CBool
c_typeAvailable oid = return $ CBool $ fromBool $ typeAvailable oid

-- Name of corresponding Haskell type
baseName :: Oid -> String
baseName oid = fromMaybe undefined (getDataType oid)

-- Extract module file name from CallInfo struct
getModFileName :: Ptr CallInfo -> Interpreter String
getModFileName pCallInfo = liftIO $ (#peek struct CallInfo, mod_file_name) pCallInfo >>= peekCString

-- Extract function name from CallInfo struct
getFuncName :: Ptr CallInfo -> Interpreter String
getFuncName pCallInfo = liftIO $ (#peek struct CallInfo, func_name) pCallInfo >>= peekCString

-- Get field of ValueInfo struct
getField :: Ptr ValueInfo -> Int16 -> IO (Ptr ValueInfo)
getField pValueInfo i = do
    fields <- (#peek struct ValueInfo, fields) pValueInfo
    peekElemOff fields (fromIntegral i)

getTypeNameTyp :: Word16 -> Ptr ValueInfo -> IO String
getTypeNameTyp (#const VOID_TYPE) _pValueInfo = return "()"

getTypeNameTyp (#const BASE_TYPE) pValueInfo = do
    typeOid <- (#peek struct ValueInfo, type_oid) pValueInfo
    return $ "Maybe " ++ baseName typeOid

getTypeNameTyp (#const COMPOSITE_TYPE) pValueInfo = do
    count <- (#peek struct ValueInfo, count) pValueInfo
    names <- forM [0 .. count-1] (getField pValueInfo >=> getTypeName)
    return $ "Maybe (" ++ intercalate ", " names ++ ")"

getTypeNameTyp _typ _pValueInfo = undefined

-- Get Haskell type name based on ValueInfo struct
getTypeName :: Ptr ValueInfo -> IO String
getTypeName pValueInfo = do
    typ <- (#peek struct ValueInfo, type) pValueInfo
    getTypeNameTyp typ pValueInfo

-- Get argument ValueInfo struct from CallInfo struct
getArgValueInfo :: Ptr CallInfo -> Int16 -> IO (Ptr ValueInfo)
getArgValueInfo pCallInfo i = do
    pArgs <- (#peek struct CallInfo, args) pCallInfo
    peekElemOff pArgs (fromIntegral i)

-- Get type signature of function needed based on CallInfo struct
getSignature :: Ptr CallInfo -> Interpreter String
getSignature pCallInfo = liftIO $ do
    nargs <- (#peek struct CallInfo, nargs) pCallInfo
    argTypeNames <- forM [0 .. nargs-1] (getArgValueInfo pCallInfo >=> getTypeName)
    resultTypeName <- (#peek struct CallInfo, result) pCallInfo >>= getTypeName

    CBool trusted <- (#peek struct CallInfo, trusted) pCallInfo
    CBool returnSet <- (#peek struct CallInfo, return_set) pCallInfo
    if toBool trusted
        then if toBool returnSet
            then return $ intercalate " -> " (argTypeNames ++ ["PGm [" ++ resultTypeName ++ "]"])
            else return $ intercalate " -> " (argTypeNames ++ ["PGm (" ++ resultTypeName ++ ")"])
        else if toBool returnSet
            then return $ intercalate " -> " (argTypeNames ++ ["IO [" ++ resultTypeName ++ "]"])
            else return $ intercalate " -> " (argTypeNames ++ ["IO (" ++ resultTypeName ++ ")"])

writeResultDefTyp :: Word16 -> Ptr ValueInfo -> IO String
writeResultDefTyp (#const VOID_TYPE) _pValueInfo = return "writeVoid"

writeResultDefTyp (#const BASE_TYPE) pValueInfo = do
    typeOid <- (#peek struct ValueInfo, type_oid) pValueInfo
    return $ "(writeType :: Maybe " ++ baseName typeOid ++ " -> Ptr ValueInfo -> IO ())"

writeResultDefTyp (#const COMPOSITE_TYPE) pValueInfo = do
    count <- (#peek struct ValueInfo, count) pValueInfo
    fieldsDef <- forM [0 .. count-1] (writeGetFieldDef pValueInfo)
    let fieldsList = intercalate ", " (map (interpolate "field?") [0 .. count-1])
    return $ "\\result pValueInfo -> case result of Nothing -> writeNull pValueInfo;\
    \                                              Just (" ++ fieldsList ++ ") -> do;\
    \                                                                              writeNotNull pValueInfo;" ++
                                                                                   concat fieldsDef

writeResultDefTyp _typ _pValueInfo = undefined

writeGetFieldDef :: Ptr ValueInfo -> Int16 -> IO String
writeGetFieldDef pValueInfo i = do
    writeFieldDef <- getField pValueInfo i >>= writeResultDef;
    return $ interpolate ("getField pValueInfo ? >>= (" ++ writeFieldDef ++ ") field?;") i

-- Return a string representing a function to take a Haskell result and write it to a ValueInfo struct
writeResultDef :: Ptr ValueInfo -> IO String
writeResultDef pValueInfo = do
    typ <- (#peek struct ValueInfo, type) pValueInfo
    writeResultDefTyp typ pValueInfo

readArgDefTyp :: Word16 -> Ptr ValueInfo -> IO String
readArgDefTyp (#const BASE_TYPE) pValueInfo = do
    typeOid <- (#peek struct ValueInfo, type_oid) pValueInfo
    return $ "(readType :: Ptr ValueInfo -> IO (Maybe " ++ baseName typeOid ++ "))"

readArgDefTyp (#const COMPOSITE_TYPE) pValueInfo = do
    count <- (#peek struct ValueInfo, count) pValueInfo
    fieldsDef <- forM [0 .. count-1] (readGetFieldDef pValueInfo)
    let fieldsList = intercalate ", " (map (interpolate "field?") [0 .. count-1])
    return $ "\\pValueInfo -> do {\
    \                                isNull <- readIsNull pValueInfo;\
    \                                if isNull; \
    \                                    then return Nothing;\
    \                                    else do {" ++
                                             concat fieldsDef ++
                                             "return (Just (" ++ fieldsList ++ "))\
    \                                    }\
    \                        }"

readArgDefTyp _typ _pValueInfo = undefined

readGetFieldDef :: Ptr ValueInfo -> Int16 -> IO String
readGetFieldDef pValueInfo i = do
    readFieldDef <- getField pValueInfo i >>= readArgDef
    return $ interpolate ("field? <- getField pValueInfo ? >>= (" ++ readFieldDef ++ ");") i

-- Return a string representing a function to take and argument from a ValueInfo struct and return the Haskell value
readArgDef :: Ptr ValueInfo -> IO String
readArgDef pValueInfo = do
    typ <- (#peek struct ValueInfo, type) pValueInfo
    readArgDefTyp typ pValueInfo

defineReadArg :: Ptr CallInfo -> Int16 -> Interpreter ()
defineReadArg pCallInfo i = do
    argDef <- liftIO $ getArgValueInfo pCallInfo i >>= readArgDef
    runStmt $ interpolate ("let readArg? = " ++ argDef) i

definePArgValueInfo :: Ptr CallInfo -> Int16 -> Interpreter ()
definePArgValueInfo pCallInfo i = do
    pArgValueInfo <- liftIO $ getArgValueInfo pCallInfo i
    runStmt $ interpolate "let pArgValueInfo? = wordPtrToPtr " i ++ (show $ ptrToWordPtr pArgValueInfo)

-- Set up interpreter to evaluate a function
setUpEvalInt :: Ptr CallInfo -> Interpreter (Int16, String, Bool)
setUpEvalInt pCallInfo = do
    set [languageExtensions := [OverloadedStrings, Safe], installedModulesInScope := False]
    modFileName <- getModFileName pCallInfo
    loadModules [modFileName]

    --Name of function
    funcName <- getFuncName pCallInfo
    setImportsF [ModuleImport "Prelude"           NotQualified (ImportList ["Bool", "Char", "Double", "Float", "IO", "Maybe(Just, Nothing)", "return", "($)", "(>>=)"]),
                 ModuleImport "Data.ByteString"   NotQualified (ImportList ["ByteString"]),
                 ModuleImport "Data.Int"          NotQualified (ImportList ["Int16", "Int32", "Int64"]),
                 ModuleImport "Data.Text"         NotQualified (ImportList ["Text"]),
                 ModuleImport "Foreign.Ptr"       NotQualified (ImportList ["Ptr", "wordPtrToPtr"]),
                 ModuleImport "Foreign.StablePtr" NotQualified (ImportList ["newStablePtr"]),
                 ModuleImport "Foreign.Storable"  NotQualified (ImportList ["poke"]),
                 ModuleImport "PGutils"           NotQualified (ImportList ["PGm", "unPGm"]),
                 ModuleImport "PGsupport"         NotQualified (ImportList ["ReadWrite (readType, writeType)", "ValueInfo", "getField", "readIsNull", "wrapVoidFunc", "writeNull", "writeNotNull", "writeVoid", "iterate", "mkResultList"]),
                 ModuleImport "PGmodule" (QualifiedAs Nothing) (ImportList [funcName])]

    CBool trusted <- liftIO $ (#peek struct CallInfo, trusted) pCallInfo

    -- Check signature
    signature <- getSignature pCallInfo
    r <- typeChecks ("PGmodule." ++ funcName ++ "::" ++ signature)
    if r
        then return ()
        else liftIO $ raiseError ("Expected Signature : " ++ funcName ++ " :: " ++ signature)

    -- Number of arguments
    nargs <- liftIO $ (#peek struct CallInfo, nargs) pCallInfo

    -- Fill all readArg? values with functions to read arguments from ValueInfo structs
    forM_ [0 .. nargs-1] (defineReadArg pCallInfo)

    -- Fill writeResult value with function to write result to ValueInfo struct
    pResultValueInfo <- liftIO $ (#peek struct CallInfo, result) pCallInfo
    resultDef <- liftIO $ writeResultDef pResultValueInfo
    runStmt $ "let writeResult = " ++ resultDef

    -- Set pArgValueInfo? and pResultValueInfo to point to ValueInfo structs
    forM_ [0 .. nargs-1] (definePArgValueInfo pCallInfo)
    runStmt $ "let pResultValueInfo = wordPtrToPtr " ++ (show $ ptrToWordPtr pResultValueInfo)

    return (nargs, funcName, toBool trusted)

-- Execute an interpreter monad and handle the result
execute :: Interpreter () -> IO ()
execute int = do
    r <- runInterpreter int
    case r of
        Left (UnknownError msg)    -> raiseError msg
        Left (WontCompile [])      -> raiseError "PL/Haskell : Unknown Compiler Error"
        Left (WontCompile (err:_)) -> raiseError $ errMsg err
        Left (NotAllowed msg)      -> raiseError msg
        Left (GhcException msg)    -> raiseError msg
        Right ()                   -> return ()

-- Check the type signature of the function against what is expected
-- Raise and Error if they don't match
foreign export capi "check_signature" checkSignature :: Ptr CallInfo -> IO ()
checkSignature :: Ptr CallInfo -> IO ()
checkSignature pCallInfo = execute $ do
    set [languageExtensions := [OverloadedStrings, Safe], installedModulesInScope := False]
    modFileName <- getModFileName pCallInfo
    loadModules [modFileName]

    funcName <- getFuncName pCallInfo
    setImportsF [ModuleImport "Prelude"         NotQualified (ImportList ["Bool", "Char", "Double", "Float", "Maybe", "IO"]),
                 ModuleImport "Data.ByteString" NotQualified (ImportList ["ByteString"]),
                 ModuleImport "Data.Int"        NotQualified (ImportList ["Int16", "Int32", "Int64"]),
                 ModuleImport "Data.Text"       NotQualified (ImportList ["Text"]),
                 ModuleImport "PGutils"         NotQualified (ImportList ["PGm"]),
                 ModuleImport "PGmodule" (QualifiedAs Nothing) (ImportList [funcName])]

    signature <- getSignature pCallInfo
    r <- typeChecks $ "PGmodule." ++ funcName ++ "::" ++ signature
    if r
        then return ()
        else liftIO $ raiseError $ "Expected Signature : " ++ funcName ++ " :: " ++ signature

-- Set the Function field of the CallInfo struct to a function that
-- will read the arguments, call the function, and write the result
foreign export capi "mk_function" mkFunction :: Ptr CallInfo -> IO ()
mkFunction :: Ptr CallInfo -> IO ()
mkFunction pCallInfo = execute $ do
    (nargs, funcName, trusted) <- setUpEvalInt pCallInfo

    -- Build the Function
    let prog_read_args = concatMap (interpolate "arg? <- readArg? pArgValueInfo?;") [0 .. nargs-1]
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    let prog_call = if trusted
        then "result <- unPGm $ PGmodule." ++ funcName ++ argsNames ++ ";"
        else "result <-         PGmodule." ++ funcName ++ argsNames ++ ";"
    let prog_write_result = "writeResult result pResultValueInfo"
    runStmt $ "function <- wrapVoidFunc $ do {" ++ prog_read_args ++ prog_call ++ prog_write_result ++ "}"

    -- Poke the value of the pointer into the Function field of the CallInfo struct
    runStmt $ "let pFunction = wordPtrToPtr " ++ show ((#ptr struct CallInfo, function) pCallInfo)
    runStmt   "poke pFunction function"

-- Set the List field of the CallInfo struct to the list returns by the function
-- Set the Function field of the CallInfo struct to a function that will iterate through the list
-- Each call of Function :
--     Writes the head of the list to the Result ValueInfo struct
--     Advances the List pointer
foreign export capi "mk_iterator" mkIterator :: Ptr CallInfo -> IO ()
mkIterator :: Ptr CallInfo -> IO ()
mkIterator pCallInfo = execute $ do
    (nargs, funcName, trusted) <- setUpEvalInt pCallInfo

    -- Get the arguments
    forM_ [0 .. nargs-1] (runStmt . (interpolate "arg? <- readArg? pArgValueInfo?"))

    -- Set writeResultList to be a list of actions each of which loads a result into the result ValueInfo struct
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    if trusted
        then runStmt $ "results <- unPGm $ PGmodule." ++ funcName ++ argsNames
        else runStmt $ "results <-         PGmodule." ++ funcName ++ argsNames

    runStmt "let writeResultList = mkResultList writeResult results pResultValueInfo"

    -- poke the stable pointer value into the List field of the CallInfo struct
    runStmt   "spList <- newStablePtr writeResultList"
    runStmt $ "let pList = wordPtrToPtr " ++ show ((#ptr struct CallInfo, list) pCallInfo)
    runStmt   "poke pList spList"

    -- poke the iterator into the Function field of the CallInfo struct
    runStmt   "function <- wrapVoidFunc (iterate pList)"
    runStmt $ "let pFunction = wordPtrToPtr " ++ show ((#ptr struct CallInfo, function) pCallInfo)
    runStmt   "poke pFunction function"

-- Version of the underlying GHC API.
foreign export capi "hint_ghc_version" hintGhcVersion :: Int32
hintGhcVersion :: Int32
hintGhcVersion = fromIntegral ghcVersion
