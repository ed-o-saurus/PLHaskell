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
data TypeInfo
newtype Oid = Oid CUInt deriving newtype (Eq, Num, Storable)

-- Replace all instances of ? with i
interpolate :: String -> Int16 -> String
interpolate "" _n = ""
interpolate ('?':ss) n = show n ++ interpolate ss n
interpolate (s:ss) n = s : interpolate ss n

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
foreign export capi "type_available" c_typeAvailable :: Oid -> CBool
c_typeAvailable :: Oid -> CBool
c_typeAvailable oid = CBool $ fromBool $ typeAvailable oid

-- Name of corresponding Haskell type
baseName :: Oid -> String
baseName oid = fromMaybe undefined (getDataType oid)

-- Extract module file name from CallInfo struct
getModFileName :: Ptr CallInfo -> Interpreter String
getModFileName pCallInfo = liftIO $ (#peek struct CallInfo, mod_file_name) pCallInfo >>= peekCString

-- Extract function name from CallInfo struct
getFuncName :: Ptr CallInfo -> Interpreter String
getFuncName pCallInfo = liftIO $ (#peek struct CallInfo, func_name) pCallInfo >>= peekCString

-- Extract the value type from TypeInfo struct
getValueType :: Ptr TypeInfo -> IO Word16
getValueType = (#peek struct TypeInfo, value_type)

-- Extract type_oid from TypeInfo struct
getTypeOid :: Ptr TypeInfo -> IO Oid
getTypeOid = (#peek struct TypeInfo, type_oid)

-- Extract count of sub-fields from TypeInfo sruct
getCount :: Ptr TypeInfo -> IO Int16
getCount = (#peek struct TypeInfo, count)

-- Get field of TypeInfo struct
getField :: Ptr TypeInfo -> Int16 -> IO (Ptr TypeInfo)
getField pTypeInfo j = do
    fields <- (#peek struct TypeInfo, fields) pTypeInfo
    peekElemOff fields (fromIntegral j)

setPtr :: String -> Ptr a -> Interpreter ()
setPtr name ptr = runStmt $ "let " ++ name ++ " = wordPtrToPtr " ++ show (ptrToWordPtr ptr)

-- Get Haskell type name based on TypeInfo struct
getTypeName :: Ptr TypeInfo -> IO String
getTypeName pTypeInfo = do
    valueType <- getValueType pTypeInfo
    case valueType of
        (#const VOID_TYPE) -> return "()"
        (#const BASE_TYPE) -> do
            typeOid <- getTypeOid pTypeInfo
            return $ "Maybe " ++ baseName typeOid
        (#const COMPOSITE_TYPE) -> do
            count <- getCount pTypeInfo
            names <- forM [0 .. count-1] (getField pTypeInfo >=> getTypeName)
            return $ "Maybe (" ++ intercalate ", " names ++ ")"
        _ -> undefined

-- Get argument TypeInfo struct from CallInfo struct
getArgTypeInfo :: Ptr CallInfo -> Int16 -> IO (Ptr TypeInfo)
getArgTypeInfo pCallInfo i = do
    pArgs <- (#peek struct CallInfo, args) pCallInfo
    peekElemOff pArgs (fromIntegral i)

-- Get type signature of function needed based on CallInfo struct
getSignature :: Ptr CallInfo -> Interpreter String
getSignature pCallInfo = liftIO $ do
    nargs <- (#peek struct CallInfo, nargs) pCallInfo
    argTypeNames <- forM [0 .. nargs-1] (getArgTypeInfo pCallInfo >=> getTypeName)
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

-- Return a string representing a function to take a Haskell result and write it to a TypeInfo struct
writeResultDef :: Ptr TypeInfo -> IO String
writeResultDef pTypeInfo = let
    getFieldDef j = do
        writeFieldDef <- getField pTypeInfo j >>= writeResultDef;
        return $ interpolate ("getField pTypeInfo ? >>= (" ++ writeFieldDef ++ ") field?;") j
    in do
        valueType <- getValueType pTypeInfo
        case valueType of
            (#const VOID_TYPE) -> return "writeVoid"
            (#const BASE_TYPE) -> do
                typeOid <- getTypeOid pTypeInfo
                return $ "(writeType :: Maybe " ++ baseName typeOid ++ " -> Ptr TypeInfo -> IO ())"
            (#const COMPOSITE_TYPE) -> do
                count <- getCount pTypeInfo
                fieldsDef <- forM [0 .. count-1] getFieldDef
                let fieldsList = intercalate ", " (map (interpolate "field?") [0 .. count-1])
                return $ "\\result pTypeInfo -> case result of Nothing -> writeIsNull True pTypeInfo;\
                \                                               Just (" ++ fieldsList ++ ") -> do;\
                \                                                                              writeIsNull False pTypeInfo;" ++
                                                                                            concat fieldsDef
            _ -> undefined

-- Return a string representing a function to take and argument from a TypeInfo struct and return the Haskell value
readArgDef :: Ptr TypeInfo -> IO String
readArgDef pTypeInfo = let
    getFieldDef j = do
        readFieldDef <- getField pTypeInfo j >>= readArgDef
        return $ interpolate ("field? <- getField pTypeInfo ? >>= (" ++ readFieldDef ++ ");") j
    in do
        valueType <- getValueType pTypeInfo
        case valueType of
            (#const BASE_TYPE) -> do
                typeOid <- getTypeOid pTypeInfo
                return $ "(readType :: Ptr TypeInfo -> IO (Maybe " ++ baseName typeOid ++ "))"
            (#const COMPOSITE_TYPE) -> do
                count <- getCount pTypeInfo
                fieldsDef <- forM [0 .. count-1] getFieldDef
                let fieldsList = intercalate ", " (map (interpolate "field?") [0 .. count-1])
                return $ "\\pTypeInfo -> do {\
                \                                isNull <- readIsNull pTypeInfo;\
                \                                if isNull; \
                \                                    then return Nothing;\
                \                                    else do {" ++
                                                        concat fieldsDef ++
                                                        "return (Just (" ++ fieldsList ++ "))\
                \                                    }\
                \                        }"
            _ -> undefined

defineReadArg :: Ptr CallInfo -> Int16 -> Interpreter ()
defineReadArg pCallInfo i = do
    argDef <- liftIO $ getArgTypeInfo pCallInfo i >>= readArgDef
    runStmt $ interpolate ("let readArg? = " ++ argDef) i

definePArgTypeInfo :: Ptr CallInfo -> Int16 -> Interpreter ()
definePArgTypeInfo pCallInfo i = do
    pArgTypeInfo <- liftIO $ getArgTypeInfo pCallInfo i
    setPtr (interpolate "pArgTypeInfo?" i) pArgTypeInfo

-- Set up interpreter to evaluate a function
setUpEvalInt :: Ptr CallInfo -> Interpreter (Int16, String, Bool)
setUpEvalInt pCallInfo = do
    set [languageExtensions := [OverloadedStrings, Safe], installedModulesInScope := False]
    modFileName <- getModFileName pCallInfo
    loadModules [modFileName]

    --Name of function
    funcName <- getFuncName pCallInfo
    setImportsF [ModuleImport "Prelude"           NotQualified (ImportList ["Bool(False, True)", "Char", "Double", "Float", "IO", "Maybe(Just, Nothing)", "return", "($)", "(>>=)"]),
                 ModuleImport "Data.ByteString"   NotQualified (ImportList ["ByteString"]),
                 ModuleImport "Data.Int"          NotQualified (ImportList ["Int16", "Int32", "Int64"]),
                 ModuleImport "Data.Text"         NotQualified (ImportList ["Text"]),
                 ModuleImport "Foreign.Ptr"       NotQualified (ImportList ["Ptr", "wordPtrToPtr"]),
                 ModuleImport "Foreign.StablePtr" NotQualified (ImportList ["newStablePtr"]),
                 ModuleImport "Foreign.Storable"  NotQualified (ImportList ["poke"]),
                 ModuleImport "PGutils"           NotQualified (ImportList ["PGm", "unPGm"]),
                 ModuleImport "PGsupport"         NotQualified (ImportList ["ReadWrite (readType, writeType)", "TypeInfo", "getField", "readIsNull", "wrapVoidFunc", "writeIsNull", "writeVoid", "iterate", "mkResultList"]),
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

    -- Fill all readArg? values with functions to read arguments from TypeInfo structs
    forM_ [0 .. nargs-1] (defineReadArg pCallInfo)

    -- Fill writeResult value with function to write result to TypeInfo struct
    pResultTypeInfo <- liftIO $ (#peek struct CallInfo, result) pCallInfo
    resultDef <- liftIO $ writeResultDef pResultTypeInfo
    runStmt $ "let writeResult = " ++ resultDef

    -- Set pArgTypeInfo? and pResultTypeInfo to point to TypeInfo structs
    forM_ [0 .. nargs-1] (definePArgTypeInfo pCallInfo)
    setPtr "pResultTypeInfo" pResultTypeInfo

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
    let prog_read_args = concatMap (interpolate "arg? <- readArg? pArgTypeInfo?;") [0 .. nargs-1]
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    let prog_call = if trusted
        then "result <- unPGm $ PGmodule." ++ funcName ++ argsNames ++ ";"
        else "result <-         PGmodule." ++ funcName ++ argsNames ++ ";"
    let prog_write_result = "writeResult result pResultTypeInfo"
    runStmt $ "function <- wrapVoidFunc $ do {" ++ prog_read_args ++ prog_call ++ prog_write_result ++ "}"

    -- Poke the value of the pointer into the Function field of the CallInfo struct
    setPtr "pFunction" ((#ptr struct CallInfo, function) pCallInfo)
    runStmt "poke pFunction function"

-- Set the List field of the CallInfo struct to the list returns by the function
-- Set the Function field of the CallInfo struct to a function that will iterate through the list
-- Each call of Function :
--     Writes the head of the list to the Result TypeInfo struct
--     Advances the List pointer
foreign export capi "mk_iterator" mkIterator :: Ptr CallInfo -> IO ()
mkIterator :: Ptr CallInfo -> IO ()
mkIterator pCallInfo = execute $ do
    (nargs, funcName, trusted) <- setUpEvalInt pCallInfo

    -- Get the arguments
    forM_ [0 .. nargs-1] (runStmt . (interpolate "arg? <- readArg? pArgTypeInfo?"))

    -- Set writeResultList to be a list of actions each of which loads a result into the result TypeInfo struct
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    if trusted
        then runStmt $ "results <- unPGm $ PGmodule." ++ funcName ++ argsNames
        else runStmt $ "results <-         PGmodule." ++ funcName ++ argsNames

    runStmt "let writeResultList = mkResultList writeResult results pResultTypeInfo"

    -- poke the stable pointer value into the List field of the CallInfo struct
    runStmt "spList <- newStablePtr writeResultList"
    setPtr "pList" ((#ptr struct CallInfo, list) pCallInfo)
    runStmt "poke pList spList"

    -- poke the iterator into the Function field of the CallInfo struct
    runStmt "function <- wrapVoidFunc (iterate pList)"
    setPtr "pFunction" ((#ptr struct CallInfo, function) pCallInfo)
    runStmt "poke pFunction function"

-- Version of the underlying GHC API.
foreign export capi "hint_ghc_version" hintGhcVersion :: Int32
hintGhcVersion :: Int32
hintGhcVersion = fromIntegral ghcVersion
