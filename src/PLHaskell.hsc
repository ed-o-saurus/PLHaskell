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
import Data.Int                     (Int16)
import Data.List                    (intercalate)
import Data.Word                    (Word16)
import Foreign.C.String             (CString, peekCString, withCString)
import Foreign.C.Types              (CBool (CBool), CInt (CInt), CUInt (CUInt))
import Foreign.Marshal.Utils        (fromBool, toBool)
import Foreign.Ptr                  (Ptr, plusPtr, ptrToWordPtr)
import Foreign.Storable             (Storable, peekByteOff, peekElemOff)
import Language.Haskell.Interpreter (Extension (OverloadedStrings, Safe), ImportList (ImportList), Interpreter, InterpreterError (GhcException, NotAllowed, UnknownError, WontCompile), ModuleImport (ModuleImport), ModuleQualification (NotQualified, QualifiedAs), OptionVal ((:=)), errMsg, installedModulesInScope, languageExtensions, liftIO, loadModules, runInterpreter, runStmt, set, setImportsF, typeChecks)
import Prelude                      (Bool (False, True), Either (Left, Right), Eq, IO, Maybe (Just, Nothing), Num, String, concat, concatMap, fromIntegral, map, maybe, return, show, undefined, ($), (++), (-), (.), (>>=))

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

-- withCString leaks memory and should be replaced
raise :: CInt -> String -> IO ()
raise level msg = withCString msg (plhaskellReport level)

raiseError :: String -> IO ()
raiseError = raise (#const ERROR)

data DataType = DataType {name          :: String,
                          writeFunction :: String,
                          readFunction  :: String}

getDataType :: Oid -> Maybe DataType
getDataType (#const BYTEAOID)  = Just (DataType {name = "ByteString", writeFunction="writeBytea",  readFunction="readBytea"})
getDataType (#const TEXTOID)   = Just (DataType {name = "Text",       writeFunction="writeText",   readFunction="readText"})
getDataType (#const BPCHAROID) = Just (DataType {name = "Char",       writeFunction="writeChar",   readFunction="readChar"})
getDataType (#const BOOLOID)   = Just (DataType {name = "Bool",       writeFunction="writeBool",   readFunction="readBool"})
getDataType (#const INT2OID)   = Just (DataType {name = "Int16",      writeFunction="writeInt2",   readFunction="readInt2"})
getDataType (#const INT4OID)   = Just (DataType {name = "Int32",      writeFunction="writeInt4",   readFunction="readInt4"})
getDataType (#const INT8OID)   = Just (DataType {name = "Int64",      writeFunction="writeInt8",   readFunction="readInt8"})
getDataType (#const FLOAT4OID) = Just (DataType {name = "Float",      writeFunction="writeFloat4", readFunction="readFloat4"})
getDataType (#const FLOAT8OID) = Just (DataType {name = "Double",     writeFunction="writeFloat8", readFunction="readFloat8"})
getDataType _ = Nothing

-- Is a type supported
typeAvailable :: Oid -> Bool
typeAvailable oid = case (getDataType oid) of Just _  -> True
                                              Nothing -> False

-- Run the function pointed to by the stable pointer
foreign export capi "type_available" c_typeAvailable :: Oid -> IO CBool
c_typeAvailable :: Oid -> IO CBool
c_typeAvailable oid = return $ CBool $ fromBool $ typeAvailable oid

-- Name of corresponding Haskell type
baseName :: Oid -> String
baseName oid = maybe undefined name (getDataType oid)

-- Name of function to write Haskell type to ValueInfo struct
writeFunctionName :: Oid -> String
writeFunctionName oid = maybe undefined writeFunction (getDataType oid)

-- Name of function to read Haskell type from ValueInfo struct
readFunctionName :: Oid -> String
readFunctionName oid = maybe undefined readFunction (getDataType oid)

-- Extract module file name from CallInfo struct
getModFileName :: Ptr CallInfo -> Interpreter String
getModFileName pCallInfo = liftIO ((#peek struct CallInfo, mod_file_name) pCallInfo >>= peekCString)

-- Extract function name from CallInfo struct
getFuncName :: Ptr CallInfo -> Interpreter String
getFuncName pCallInfo = liftIO ((#peek struct CallInfo, func_name) pCallInfo >>= peekCString)

-- Get field of ValueInfo struct
getField :: Ptr ValueInfo -> Int16 -> IO (Ptr ValueInfo)
getField pValueInfo i = do
    fields <- (#peek struct ValueInfo, fields) pValueInfo
    peekElemOff fields (fromIntegral i)

getTypeNameTyp :: Word16 -> Ptr ValueInfo -> IO String
getTypeNameTyp (#const VOID_TYPE) _pValueInfo = return "()"

getTypeNameTyp (#const BASE_TYPE) pValueInfo = do
    typeOid <- (#peek struct ValueInfo, type_oid) pValueInfo
    return ("Maybe " ++ baseName typeOid)

getTypeNameTyp (#const COMPOSITE_TYPE) pValueInfo = do
    count <- (#peek struct ValueInfo, count) pValueInfo
    names <- forM [0 .. count-1] (getField pValueInfo >=> getTypeName)
    return ("Maybe (" ++ intercalate ", " names ++ ")")

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

    CBool returnSet <- (#peek struct CallInfo, return_set) pCallInfo
    if toBool returnSet
        then return (intercalate " -> " (argTypeNames ++ ["PGm [" ++ resultTypeName ++ "]"]))
        else return (intercalate " -> " (argTypeNames ++ ["PGm (" ++ resultTypeName ++ ")"]))

writeResultDefTyp :: Word16 -> Ptr ValueInfo -> IO String
writeResultDefTyp (#const VOID_TYPE) _pValueInfo = return "writeVoid"

writeResultDefTyp (#const BASE_TYPE) pValueInfo = do
    typeOid <- (#peek struct ValueInfo, type_oid) pValueInfo
    return (writeFunctionName typeOid)

writeResultDefTyp (#const COMPOSITE_TYPE) pValueInfo = do
    let getFieldDef i = do
        writeFieldDef <- getField pValueInfo i >>= writeResultDef
        return $ interpolate ("getField pValueInfo ? >>= (" ++ writeFieldDef ++ ") field?;") i
    count <- (#peek struct ValueInfo, count) pValueInfo
    fieldsDef <- forM [0 .. count-1] getFieldDef
    let fieldsList = intercalate ", " (map (interpolate "field?") [0 .. count-1])
    return ("\\result pValueInfo -> case result of Nothing -> writeNull pValueInfo;\
    \                                              Just (" ++ fieldsList ++ ") -> do;\
    \                                                                              writeNotNull pValueInfo;" ++
                                                                                   concat fieldsDef)

writeResultDefTyp _typ _pValueInfo = undefined

-- Return a string representing a function to take a Haskell result and write it to a ValueInfo struct
writeResultDef :: Ptr ValueInfo -> IO String
writeResultDef pValueInfo = do
    typ <- (#peek struct ValueInfo, type) pValueInfo
    writeResultDefTyp typ pValueInfo

readArgDefTyp :: Word16 -> Ptr ValueInfo -> IO String
readArgDefTyp (#const BASE_TYPE) pValueInfo = do
    typeOid <- (#peek struct ValueInfo, type_oid) pValueInfo
    return (readFunctionName typeOid)

readArgDefTyp (#const COMPOSITE_TYPE) pValueInfo = do
    let getFieldDef i = do
        readFieldDef <- getField pValueInfo i >>= readArgDef
        return $ interpolate ("field? <- getField pValueInfo ? >>= (" ++ readFieldDef ++ ");") i
    count <- (#peek struct ValueInfo, count) pValueInfo
    fieldsDef <- forM [0 .. count-1] getFieldDef
    let fieldsList = intercalate ", " (map (interpolate "field?") [0 .. count-1])
    return ("\\pValueInfo -> do {\
    \                                isNull <- readIsNull pValueInfo;\
    \                                if isNull; \
    \                                    then return Nothing;\
    \                                    else do {" ++
                                             concat fieldsDef ++
                                             "return (Just (" ++ fieldsList ++ "))\
    \                                    }\
    \                        }")

readArgDefTyp _typ _pValueInfo = undefined

-- Return a string representing a function to take and argument from a ValueInfo struct and return the Haskell value
readArgDef :: Ptr ValueInfo -> IO String
readArgDef pValueInfo = do
    typ <- (#peek struct ValueInfo, type) pValueInfo
    readArgDefTyp typ pValueInfo

defineReadArg :: Ptr CallInfo -> Int16 -> Interpreter ()
defineReadArg pCallInfo i = do
    pArgValueInfo <- liftIO (getArgValueInfo pCallInfo i)
    argDef <- liftIO (readArgDef pArgValueInfo)
    runStmt (interpolate ("let readArg? = " ++ argDef) i)

definePArgValueInfo :: Ptr CallInfo -> Int16 -> Interpreter ()
definePArgValueInfo pCallInfo i = do
    pArgValueInfo <- liftIO (getArgValueInfo pCallInfo i)
    runStmt (interpolate "let pArgValueInfo? = wordPtrToPtr " i ++ (show . ptrToWordPtr) pArgValueInfo)

-- Set up interpreter to evaluate a function
setUpEvalInt :: Ptr CallInfo -> Interpreter (Int16, String)
setUpEvalInt pCallInfo = do
    set [languageExtensions := [OverloadedStrings, Safe], installedModulesInScope := False]
    modFileName <- getModFileName pCallInfo
    loadModules [modFileName]

    --Name of function
    funcName <- getFuncName pCallInfo
    setImportsF [ModuleImport "Prelude"           NotQualified (ImportList ["Bool", "Char", "Double", "Float", "IO", "Maybe(Just, Nothing)", "flip", "map", "return", "(>>=)"]),
                 ModuleImport "Data.ByteString"   NotQualified (ImportList ["ByteString"]),
                 ModuleImport "Data.Int"          NotQualified (ImportList ["Int16", "Int32", "Int64"]),
                 ModuleImport "Data.Text"         NotQualified (ImportList ["Text"]),
                 ModuleImport "Foreign.Ptr"       NotQualified (ImportList ["wordPtrToPtr"]),
                 ModuleImport "Foreign.StablePtr" NotQualified (ImportList ["newStablePtr"]),
                 ModuleImport "Foreign.Storable"  NotQualified (ImportList ["poke"]),
                 ModuleImport "PGutils"           NotQualified (ImportList ["PGm", "unPGm"]),
                 ModuleImport "PGsupport"         NotQualified (ImportList ["getField", "readIsNull", "readBytea", "readText", "readChar", "readBool", "readInt2", "readInt4", "readInt8", "readFloat4", "readFloat8", "wrapVoidFunc", "writeNull", "writeNotNull", "writeVoid", "writeBytea", "writeText", "writeChar", "writeBool", "writeInt2", "writeInt4", "writeInt8", "writeFloat4", "writeFloat8", "iterate"]),
                 ModuleImport "PGmodule" (QualifiedAs Nothing) (ImportList [funcName])]

    -- Check signature
    signature <- getSignature pCallInfo
    r <- typeChecks ("PGmodule." ++ funcName ++ "::" ++ signature)
    if r
        then return ()
        else (liftIO . raiseError) ("Expected Signature : " ++ funcName ++ " :: " ++ signature)

    -- Number of arguments
    nargs <- (liftIO . (#peek struct CallInfo, nargs)) pCallInfo

    -- Fill all readArg? values with functions to read arguments from ValueInfo structs
    forM_ [0 .. nargs-1] (defineReadArg pCallInfo)

    -- Fill writeResult value with function to write result to ValueInfo struct
    pResultValueInfo <- (liftIO . (#peek struct CallInfo, result)) pCallInfo
    resultDef <- (liftIO . writeResultDef) pResultValueInfo
    runStmt ("let writeResult = " ++ resultDef)

    -- Set pArgValueInfo? and pResultValueInfo to point to ValueInfo structs
    forM_ [0 .. nargs-1] (definePArgValueInfo pCallInfo)
    runStmt ("let pResultValueInfo = wordPtrToPtr " ++ (show . ptrToWordPtr) pResultValueInfo)

    return (nargs, funcName)

-- Execute and interpreter monad and handle the result
execute :: Interpreter () -> IO ()
execute int = do
    r <- runInterpreter int
    case r of
        Left (UnknownError msg)    -> raiseError msg
        Left (WontCompile [])      -> raiseError "Unknown Compiler Error"
        Left (WontCompile (err:_)) -> raiseError (errMsg err)
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
    setImportsF [ModuleImport "Prelude"         NotQualified (ImportList ["Bool", "Char", "Double", "Float", "Maybe"]),
                 ModuleImport "Data.ByteString" NotQualified (ImportList ["ByteString"]),
                 ModuleImport "Data.Int"        NotQualified (ImportList ["Int16", "Int32", "Int64"]),
                 ModuleImport "Data.Text"       NotQualified (ImportList ["Text"]),
                 ModuleImport "PGutils"         NotQualified (ImportList ["PGm"]),
                 ModuleImport "PGmodule" (QualifiedAs Nothing) (ImportList [funcName])]

    signature <- getSignature pCallInfo
    r <- typeChecks ("PGmodule." ++ funcName ++ "::" ++ signature)
    if r
        then return ()
        else (liftIO . raiseError) ("Expected Signature : " ++ funcName ++ " :: " ++ signature)

-- Set the Function field of the CallInfo struct to a function that
-- will read the arguments, call the function, and write the result
foreign export capi "mk_function" mkFunction :: Ptr CallInfo -> IO ()
mkFunction :: Ptr CallInfo -> IO ()
mkFunction pCallInfo = execute $ do
    (nargs, funcName) <- setUpEvalInt pCallInfo

    -- Build the Function
    let prog_read_args = concatMap (interpolate "arg? <- readArg? pArgValueInfo?;") [0 .. nargs-1]
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    let prog_call = "result <- unPGm (PGmodule." ++ funcName ++ argsNames ++ ");"
    let prog_write_result = "writeResult result pResultValueInfo"
    runStmt ("function <- wrapVoidFunc (do {" ++ prog_read_args ++ prog_call ++ prog_write_result ++ "})")

    -- Poke the value of the pointer into the Function field of the CallInfo struct
    runStmt ("let pFunction = wordPtrToPtr " ++ show ((#ptr struct CallInfo, function) pCallInfo))
    runStmt ("poke pFunction function")

-- Set the List field of the CallInfo struct to the list returns by the function
-- Set the Function field of the CallInfo struct to a function that will iterate through the list
-- Each call of Function :
--     Writes the head of the list to the Result ValueInfo struct
--     Advances the List pointer
foreign export capi "mk_iterator" mkIterator :: Ptr CallInfo -> IO ()
mkIterator :: Ptr CallInfo -> IO ()
mkIterator pCallInfo = execute $ do
    (nargs, funcName) <- setUpEvalInt pCallInfo

    -- Get the arguments
    forM_ [0 .. nargs-1] (runStmt . (interpolate "arg? <- readArg? pArgValueInfo?"))

    -- Set writeResultList to be a list of actions each of which loads a result into the result ValueInfo struct
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    runStmt ("results <- unPGm (PGmodule." ++ funcName ++ argsNames ++ ")")
    runStmt "let writeResultList = map ((flip writeResult) pResultValueInfo) results"

    -- poke the stable pointer value into the List field of the CallInfo struct
    runStmt "spList <- newStablePtr writeResultList"
    runStmt ("let pList = wordPtrToPtr " ++ show ((#ptr struct CallInfo, list) pCallInfo))
    runStmt "poke pList spList"

    runStmt ("let pMoreResults = wordPtrToPtr " ++ show ((#ptr struct CallInfo, more_results) pCallInfo))

    -- poke the iterator into the Function field of the CallInfo struct
    runStmt "function <- wrapVoidFunc (iterate pList pMoreResults)"
    runStmt ("let pFunction = wordPtrToPtr " ++ show ((#ptr struct CallInfo, function) pCallInfo))
    runStmt "poke pFunction function"
