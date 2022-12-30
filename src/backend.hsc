{-# LANGUAGE CApiFFI, Unsafe #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2022 Edward F. Behn, Jr.
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

import Control.Monad                (forM, forM_, join, (>=>))
import Data.Int                     (Int16, Int32)
import Data.List                    (intercalate)
import Foreign.C.String             (CString, peekCString, withCStringLen)
import Foreign.C.Types              (CBool (CBool), CShort (CShort), CInt (CInt), CSize (CSize))
import Foreign.Marshal.Utils        (copyBytes, fromBool, toBool)
import Foreign.Ptr                  (Ptr, castPtr, plusPtr, ptrToWordPtr)
import Foreign.StablePtr            (deRefStablePtr)
import Foreign.Storable             (peek, peekByteOff)
import Language.Haskell.Interpreter (Extension (OverloadedStrings, Safe), ImportList (ImportList), Interpreter, InterpreterError (GhcException, NotAllowed, UnknownError, WontCompile), ModuleImport (ModuleImport), ModuleQualification (NotQualified, QualifiedAs), OptionVal ((:=)), errMsg, installedModulesInScope, languageExtensions, liftIO, loadModules, runInterpreter, runStmt, set, setImportsF, typeChecks)
import Prelude                      (Bool (False, True), Either (Left, Right), Int, IO, Maybe (Nothing), String, concat, concatMap, error,  fromIntegral, map, otherwise, return, show, ($), (*), (+), (++), (-), (.), (==), (>>=))

-- Dummy types to make pointers
type CallInfo = ()
type ValueInfo = ()

-- Replace all instances of ? with i
interpolate :: String -> Int16 -> String
interpolate "" _ = ""
interpolate ('?':ss) i = show i ++ interpolate ss i
interpolate (s:ss) i = s : interpolate ss i

-- Function to record message or raise exception
foreign import capi safe "plhaskell.h PLHaskell_Report"
    c_Report :: CInt -> CString -> IO ()

raise :: Int32 -> String -> IO ()
raise level str = do
    ptr <- pallocString str
    c_Report (CInt level) ptr
    pfree ptr

raiseError :: String -> IO ()
raiseError = raise (#const ERROR)

-- Allocate memory using postgres' mechanism and zero the contents
foreign import capi safe "plhaskell.h palloc0"
    c_palloc0 :: CSize -> IO (Ptr a)

palloc0 :: Int -> IO (Ptr a)
palloc0 size = c_palloc0 (CSize (fromIntegral size))

-- Palloc CString
-- Copy a String's contents to palloc'd memory
pallocString :: String -> IO (Ptr a)
pallocString str = do
    withCStringLen str (\(ptr, len) -> do
        pallocPtr <- palloc0 (len+1) -- Add one to ensure \0 termination
        copyBytes pallocPtr ptr len
        return (castPtr pallocPtr))

-- Free memory using postgres' mechanism
foreign import capi safe "plhaskell.h pfree"
    pfree :: Ptr a -> IO ()

-- Is a type supported
typeAvailable :: Int32 -> Bool
typeAvailable (#const BYTEAOID)  = True
typeAvailable (#const TEXTOID)   = True
typeAvailable (#const BPCHAROID) = True
typeAvailable (#const BOOLOID)   = True
typeAvailable (#const INT2OID)   = True
typeAvailable (#const INT4OID)   = True
typeAvailable (#const INT8OID)   = True
typeAvailable (#const FLOAT4OID) = True
typeAvailable (#const FLOAT8OID) = True
typeAvailable _ = False

-- Run the function pointed to by the stable pointer
foreign export capi "TypeAvailable" c_TypeAvailable :: CInt -> IO CBool
c_TypeAvailable :: CInt -> IO CBool
c_TypeAvailable oid = do
    let CInt oid' = oid
    return  (CBool (fromBool (typeAvailable oid')))

-- Name of corresponding Haskell types
baseName :: Int32 -> String
baseName (#const BYTEAOID)  = "ByteString"
baseName (#const TEXTOID)   = "Text"
baseName (#const BPCHAROID) = "Char"
baseName (#const BOOLOID)   = "Bool"
baseName (#const INT2OID)   = "Int16"
baseName (#const INT4OID)   = "Int32"
baseName (#const INT8OID)   = "Int64"
baseName (#const FLOAT4OID) = "Float"
baseName (#const FLOAT8OID) = "Double"
baseName _ = error "Bad Oid"

-- Name of function to write Haskell type to ValueInfo struct
writeFunctionName :: Int32 -> String
writeFunctionName (#const BYTEAOID)  = "writeBytea"
writeFunctionName (#const TEXTOID)   = "writeText"
writeFunctionName (#const BPCHAROID) = "writeChar"
writeFunctionName (#const BOOLOID)   = "writeBool"
writeFunctionName (#const INT2OID)   = "writeInt2"
writeFunctionName (#const INT4OID)   = "writeInt4"
writeFunctionName (#const INT8OID)   = "writeInt8"
writeFunctionName (#const FLOAT4OID) = "writeFloat4"
writeFunctionName (#const FLOAT8OID) = "writeFloat8"
writeFunctionName _ = error "Bad Oid"

-- Name of function to read Haskell type from ValueInfo struct
readFunctionName :: Int32 -> String
readFunctionName (#const BYTEAOID)  = "readBytea"
readFunctionName (#const TEXTOID)   = "readText"
readFunctionName (#const BPCHAROID) = "readChar"
readFunctionName (#const BOOLOID)   = "readBool"
readFunctionName (#const INT2OID)   = "readInt2"
readFunctionName (#const INT4OID)   = "readInt4"
readFunctionName (#const INT8OID)   = "readInt8"
readFunctionName (#const FLOAT4OID) = "readFloat4"
readFunctionName (#const FLOAT8OID) = "readFloat8"
readFunctionName _ = error "Bad Oid"

-- Extract module file name from CallInfo struct
getModFileName :: Ptr CallInfo -> Interpreter String
getModFileName pCallInfo = liftIO ((#peek struct CallInfo, ModFileName) pCallInfo >>= peekCString)

-- Extract function name from CallInfo struct
getFuncName :: Ptr CallInfo -> Interpreter String
getFuncName pCallInfo = liftIO ((#peek struct CallInfo, FuncName) pCallInfo >>= peekCString)

-- Get field of ValueInfo struct
getField :: Ptr ValueInfo -> Int16 -> IO (Ptr ValueInfo)
getField pValueInfo i = do
    fields <- (#peek struct ValueInfo, Fields) pValueInfo
    peek (plusPtr fields (fromIntegral i * (#size struct ValueInfo*)))

-- Get Haskell type name based on ValueInfo struct
getTypeName :: Ptr ValueInfo -> IO String
getTypeName pValueInfo = do
    CInt typ <- (#peek struct ValueInfo, Type) pValueInfo
    let typeName | typ == (#const VOID_TYPE) = return "()"
                 | typ == (#const BASE_TYPE) = do
                     CInt typeOid <- (#peek struct ValueInfo, TypeOid) pValueInfo
                     return ("Maybe " ++ baseName typeOid)
                 | typ == (#const COMPOSITE_TYPE) = do
                     CShort count <- (#peek struct ValueInfo, Count) pValueInfo
                     names <- forM [0 .. count-1] (getField pValueInfo >=> getTypeName)
                     return ("Maybe (" ++ intercalate ", " names ++ ")")
                 | otherwise = error "Bad Type"
    typeName

-- Get argument ValueInfo struct from CallInfo struct
getArgValueInfo :: Ptr CallInfo -> Int16 -> IO (Ptr ValueInfo)
getArgValueInfo pCallInfo i = do
    pArgs <- (#peek struct CallInfo, Args) pCallInfo
    peek (plusPtr pArgs (fromIntegral i * (#size struct ValueInfo*)))

-- Get type signature of function needed based on CallInfo struct
getSignature :: Ptr CallInfo -> Interpreter String
getSignature pCallInfo = liftIO $ do
    CShort nargs <- (#peek struct CallInfo, nargs) pCallInfo
    argTypeNames <- forM [0 .. nargs-1] (getArgValueInfo pCallInfo >=> getTypeName)
    resultTypeName <- (#peek struct CallInfo, Result) pCallInfo >>= getTypeName

    CBool returnSet <- (#peek struct CallInfo, ReturnSet) pCallInfo
    if toBool returnSet
        then return (intercalate " -> " (argTypeNames ++ ["PGm [" ++ resultTypeName ++ "]"]))
        else return (intercalate " -> " (argTypeNames ++ ["PGm (" ++ resultTypeName ++ ")"]))

-- Return a string representing a function to take a Haskell result and write it to a ValueInfo struct
writeResultDef :: Ptr ValueInfo -> IO String
writeResultDef pValueInfo = do
    CInt typ <- (#peek struct ValueInfo, Type) pValueInfo
    let getFieldDef i = do
        writeFieldDef <- getField pValueInfo i >>= writeResultDef
        return $ interpolate ("getField pValueInfo ? >>= (" ++ writeFieldDef ++ ") field?;") i
    let def | typ == (#const VOID_TYPE) = return "writeVoid"
            | typ == (#const BASE_TYPE) = do
                CInt typeOid <- (#peek struct ValueInfo, TypeOid) pValueInfo
                return (writeFunctionName typeOid)
            | typ == (#const COMPOSITE_TYPE) = do
                CShort count <- (#peek struct ValueInfo, Count) pValueInfo
                fieldsDef <- forM [0 .. count-1] getFieldDef
                let fieldsList = intercalate ", " (map (interpolate "field?") [0 .. count-1])
                return ("\\result pValueInfo -> case result of Nothing -> writeNull pValueInfo;\
                \                                              Just (" ++ fieldsList ++ ") -> do;\
                \                                                                              writeNotNull pValueInfo;" ++
                                                                                               concat fieldsDef)
            | otherwise = error "Bad Type"
    def

-- Return a string representing a function to take and argument from a ValueInfo struct and return the Haskell value
readArgDef :: Ptr ValueInfo -> IO String
readArgDef pValueInfo = do
    CInt typ <- (#peek struct ValueInfo, Type) pValueInfo
    let getFieldDef i = do
        readFieldDef <- getField pValueInfo i >>= readArgDef
        return $ interpolate ("field? <- getField pValueInfo ? >>= (" ++ readFieldDef ++ ");") i
    let def | typ == (#const BASE_TYPE) = do
                CInt typeOid <- (#peek struct ValueInfo, TypeOid) pValueInfo
                return (readFunctionName typeOid)
            | typ == (#const COMPOSITE_TYPE) = do
                CShort count <- (#peek struct ValueInfo, Count) pValueInfo
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
            | otherwise = error "Bad Type"
    def

defineReadArg :: Ptr CallInfo -> Int16 -> Interpreter ()
defineReadArg pCallInfo i = do
    pArgValueInfo <- liftIO (getArgValueInfo pCallInfo i)
    argDef <- liftIO (readArgDef pArgValueInfo )
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
                 ModuleImport "PGsupport"         NotQualified (ImportList ["getField", "readIsNull", "readBytea", "readText", "readChar", "readBool", "readInt2", "readInt4", "readInt8", "readFloat4", "readFloat8", "writeNull", "writeNotNull", "writeVoid", "writeBytea", "writeText", "writeChar", "writeBool", "writeInt2", "writeInt4", "writeInt8", "writeFloat4", "writeFloat8", "iterate"]),
                 ModuleImport "PGmodule" (QualifiedAs Nothing) (ImportList [funcName])]

    -- Check signature
    signature <- getSignature pCallInfo
    r <- typeChecks ("PGmodule." ++ funcName ++ "::" ++ signature)
    if r
        then return ()
        else (liftIO . raiseError) ("Expected Signature : " ++ funcName ++ " :: " ++ signature)

    -- Number of arguments
    CShort nargs <- (liftIO . (#peek struct CallInfo, nargs)) pCallInfo

    -- Fill all readArg? values with functions to read arguments from ValueInfo structs
    forM_ [0 .. nargs-1] (defineReadArg pCallInfo)

    -- Fill writeResult value with function to write result to ValueInfo struct
    pResultValueInfo <- (liftIO . (#peek struct CallInfo, Result)) pCallInfo
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
foreign export capi checkSignature :: Ptr CallInfo -> IO ()
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
foreign export capi mkFunction :: Ptr CallInfo -> IO ()
mkFunction :: Ptr CallInfo -> IO ()
mkFunction pCallInfo = execute $ do
    (nargs, funcName) <- setUpEvalInt pCallInfo

    -- Build the Function
    let prog_read_args = concatMap (interpolate "arg? <- readArg? pArgValueInfo?;") [0 .. nargs-1]
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    let prog_call = "result <- unPGm (PGmodule." ++ funcName ++ argsNames ++ ");"
    let prog_write_result = "writeResult result pResultValueInfo"
    runStmt ("spFunction <- newStablePtr (do {" ++ prog_read_args ++ prog_call ++ prog_write_result ++ "})")

    -- Poke the value of the pointer into the Function field of the CallInfo struct
    runStmt ("let pFunction = wordPtrToPtr " ++ show ((#ptr struct CallInfo, Function) pCallInfo))
    runStmt ("poke pFunction spFunction")

-- Set the List field of the CallInfo struct to the list returns by the function
-- Set the Function field of the CallInfo struct to a function that will iterate through the list
-- Each call of Function :
--     Writes the head of the list to the Result ValueInfo struct
--     Advances the List pointer
foreign export capi mkIterator :: Ptr CallInfo -> IO ()
mkIterator :: Ptr CallInfo -> IO ()
mkIterator pCallInfo = execute $ do
    (nargs, funcName) <- setUpEvalInt pCallInfo

    -- Get the arguments
    forM_ [0 .. nargs-1] (runStmt . (interpolate "arg? <- readArg? pArgValueInfo?"))

    -- Set writeResultList to be a list of actions each of which loads a result into the result ValueInfo struct
    -- writeResultList :: [IO ()]  TODO : remove this line?
    let argsNames = concatMap (interpolate " arg?") [0 .. nargs-1]
    runStmt ("results <- unPGm (PGmodule." ++ funcName ++ argsNames ++ ")")
    runStmt "let writeResultList = map ((flip writeResult) pResultValueInfo) results"

    -- poke the stable pointer value into the List field of the CallInfo struct
    runStmt ("spList <- newStablePtr writeResultList")
    runStmt ("let pList = wordPtrToPtr " ++ show ((#ptr struct CallInfo, List) pCallInfo))
    runStmt "poke pList spList"

    runStmt ("let pMoreResults = wordPtrToPtr " ++ show ((#ptr struct CallInfo, MoreResults) pCallInfo))

    -- poke the iterator into the Function field of the CallInfo struct
    runStmt "spFunction <- newStablePtr (iterate pList pMoreResults)"
    runStmt ("let pFunction = wordPtrToPtr " ++ show ((#ptr struct CallInfo, Function) pCallInfo))
    runStmt ("poke pFunction spFunction")

-- Run the function pointed to by the Function stable pointer
foreign export capi runFunction :: Ptr CallInfo -> IO ()
runFunction :: Ptr CallInfo -> IO ()
runFunction pCallInfo = do
    spFunction <- (#peek struct CallInfo, Function) pCallInfo
    (join . deRefStablePtr) spFunction
