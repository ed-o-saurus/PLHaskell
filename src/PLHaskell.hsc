{-# LANGUAGE CApiFFI, Unsafe #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Avoid lambda using `infix`" -}

#include "plhaskell.h"

module PLHaskell (getVarSize, getDataLocation) where

import Control.Exception            (ErrorCall, catch)
import Control.Monad                (join, (>=>))
import Data.Functor                 ((<&>))
import Data.Int                     (Int16, Int32, Int64)
import Data.List                    (intercalate)
import Data.Word                    (Word8)
import Foreign.C.String             (CString, peekCString, peekCStringLen, withCString, withCStringLen)
import Foreign.C.Types              (CBool (CBool), CShort (CShort), CInt (CInt), CSize (CSize))
import Foreign.Marshal.Array        (peekArray, pokeArray)
import Foreign.Marshal.Utils        (copyBytes, fromBool, toBool)
import Foreign.Ptr                  (Ptr, plusPtr, ptrToWordPtr)
import Foreign.StablePtr            (StablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable             (Storable, sizeOf, peek, peekByteOff, poke, pokeByteOff)
import Language.Haskell.Interpreter (Extension (Safe), ImportList (ImportList), Interpreter, InterpreterError (GhcException, NotAllowed, UnknownError, WontCompile), ModuleImport (ModuleImport), ModuleQualification (QualifiedAs), OptionVal ((:=)), errMsg, installedModulesInScope, languageExtensions, liftIO, loadModules, runInterpreter, runStmt, set, setImportsF, typeChecksWithDetails)
import Prelude                      (Bool (False, True), Char, Double, Either (Left, Right), Float, Int, IO, Maybe (Just, Nothing), String, concat, concatMap, error, flip, foldl, fromIntegral, head, length, map, mapM, null, otherwise, return, show, ($), (*), (+), (++), (-), (.), (==), (>>), (>>=))
import System.Directory             (removeFile)

-- Dummy types to make pointers
type CallInfo = ()
type ValueInfo = ()

-- Replace all instances of ? with i
interpolate :: String -> Int16 -> String
interpolate "" _ = ""
interpolate ('?':ss) i = show i ++ interpolate ss i
interpolate (s:ss) i = s : interpolate ss i

-- Function to record message or raise exception
foreign import capi safe "plhaskell.h Report"
    c_Report :: CInt -> CString -> IO ()

raiseError :: String -> IO ()
raiseError str = withCString str (c_Report (#const ERROR)) 

-- Allocate memory using postgres' mechanism
foreign import capi safe "plhaskell.h palloc"
    c_palloc :: CSize -> IO (Ptr a)

palloc :: Int -> IO (Ptr a)
palloc size = c_palloc (CSize (fromIntegral size))

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
baseName (#const BYTEAOID)  = "[Data.Word.Word8]"
baseName (#const TEXTOID)   = "Prelude.String"
baseName (#const BPCHAROID) = "Prelude.Char"
baseName (#const BOOLOID)   = "Prelude.Bool"
baseName (#const INT2OID)   = "Data.Int.Int16"
baseName (#const INT4OID)   = "Data.Int.Int32"
baseName (#const INT8OID)   = "Data.Int.Int64"
baseName (#const FLOAT4OID) = "Prelude.Float"
baseName (#const FLOAT8OID) = "Prelude.Double"
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

-- Allocate memory only if the type is not ByVal and return pointer to location of data
allocDataLocation :: Ptr ValueInfo -> Int -> IO (Ptr a)
allocDataLocation pValueInfo size = do
    CBool byVal <- (#peek struct ValueInfo, ByVal) pValueInfo
    if toBool byVal
    then return ((#ptr struct ValueInfo, Value) pValueInfo)
    else do
        address <- palloc size
        (#poke struct ValueInfo, Value) pValueInfo address
        return address

-- Write a Haskell type to a ValueInfo struct
write :: (a -> Ptr ValueInfo -> IO ()) -> Maybe a -> Ptr ValueInfo -> IO ()
write _ Nothing pValueInfo = writeNull pValueInfo -- Set isNull if passed Nothing
write write' (Just result) pValueInfo = do -- Use write' if passed Just ...
    writeNotNull pValueInfo
    write' result pValueInfo

-- Set isNull to true
writeNull :: Ptr ValueInfo -> IO ()
writeNull pValueInfo = (#poke struct ValueInfo, isNull) pValueInfo (CBool (fromBool True))

-- Set isNull to false
writeNotNull :: Ptr ValueInfo -> IO ()
writeNotNull pValueInfo = (#poke struct ValueInfo, isNull) pValueInfo (CBool (fromBool False))

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
                     return ("Prelude.Maybe " ++ baseName typeOid)
                 | typ == (#const COMPOSITE_TYPE) = do
                     CShort count <- (#peek struct ValueInfo, Count) pValueInfo
                     names <- mapM (getField pValueInfo >=> getTypeName) [0 .. count-1]
                     return ("Prelude.Maybe (" ++ intercalate ", " names ++ ")")
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
    argTypeNames <- mapM (getArgValueInfo pCallInfo >=> getTypeName) [0 .. nargs-1]
    resultTypeName <- (#peek struct CallInfo, Result) pCallInfo >>= getTypeName

    CBool returnSet <- (#peek struct CallInfo, ReturnSet) pCallInfo
    if toBool returnSet
        then return (intercalate " -> " (argTypeNames ++ ["PGutils.PGm [" ++ resultTypeName ++ "]"]))
        else return (intercalate " -> " (argTypeNames ++ ["PGutils.PGm (" ++ resultTypeName ++ ")"]))

-- Used to ignore error from deleting temp module files
ignore :: ErrorCall -> IO ()
ignore _ = return ()

-- Do nothing when returning void
writeVoid :: () -> Ptr ValueInfo -> IO ()
writeVoid () _ = return ()

-- Set the size of a variable length array
foreign import capi safe "postgres.h SET_VARSIZE"
    c_SetVarSize :: Ptr () -> CInt -> IO ()

setVarSize :: Ptr () -> Int -> IO ()
setVarSize pResult len = c_SetVarSize pResult (CInt ((#const VARHDRSZ) + fromIntegral len))

-- Functions to write Haskell types to ValueInfo structs
writeBytea :: [Word8] -> Ptr ValueInfo -> IO ()
writeBytea result pValueInfo = do
    let len = length result
    pResult <- allocDataLocation pValueInfo (len + (#const VARHDRSZ))
    setVarSize pResult len
    pokeArray (plusPtr pResult (#const VARHDRSZ)) result 

writeText :: String -> Ptr ValueInfo -> IO ()
writeText result pValueInfo = withCStringLen result (\(src, len) -> do 
    pResult <- allocDataLocation pValueInfo (len + (#const VARHDRSZ))
    setVarSize pResult len
    copyBytes (plusPtr pResult (#const VARHDRSZ)) src len)

writeChar :: Char -> Ptr ValueInfo -> IO ()
writeChar result = writeText [result]

writeBool :: Bool -> Ptr ValueInfo -> IO ()
writeBool result pValueInfo = allocDataLocation pValueInfo 1 >>= (flip poke) ((CBool . fromBool) result)

writeNumber :: Storable a => a -> Ptr ValueInfo -> IO ()
writeNumber result pValueInfo = allocDataLocation pValueInfo (sizeOf result) >>= (flip poke) result

writeInt2 :: Int16 -> Ptr ValueInfo -> IO ()
writeInt2 = writeNumber

writeInt4 :: Int32 -> Ptr ValueInfo -> IO ()
writeInt4 = writeNumber

writeInt8 :: Int64 -> Ptr ValueInfo -> IO ()
writeInt8 = writeNumber

writeFloat4 :: Float -> Ptr ValueInfo -> IO ()
writeFloat4 = writeNumber

writeFloat8 :: Double -> Ptr ValueInfo -> IO ()
writeFloat8 = writeNumber

-- Return a string representing a function to take a Haskell result and write it to a ValueInfo struct
writeResultDef :: Ptr ValueInfo -> IO String
writeResultDef pValueInfo = do
    CInt typ <- (#peek struct ValueInfo, Type) pValueInfo
    let getFieldDef i = do
        writeFieldDef <- getField pValueInfo i >>= writeResultDef
        return $ interpolate ("getField pValueInfo ? Prelude.>>= (" ++ writeFieldDef ++ ") field?;") i
    let def | typ == (#const VOID_TYPE) = return "writeVoid"
            | typ == (#const BASE_TYPE) = do
                CInt typeOid <- (#peek struct ValueInfo, TypeOid) pValueInfo
                return (writeFunctionName typeOid)
            | typ == (#const COMPOSITE_TYPE) = do
                CShort count <- (#peek struct ValueInfo, Count) pValueInfo
                fieldsDef <- mapM getFieldDef [0 .. count-1]
                return ("\\result pValueInfo -> case result of Prelude.Nothing -> writeNull pValueInfo; Prelude.Just (" ++
                    intercalate ", "  (map (interpolate "field?") [0 .. count-1]) ++ ") -> do; writeNotNull pValueInfo; " ++ concat fieldsDef)
            | otherwise = error "Bad Type"
    def

readIsNull :: Ptr ValueInfo -> IO Bool
readIsNull pValueInfo = do
    CBool isNull <- (#peek struct ValueInfo, isNull) pValueInfo
    return (toBool isNull)

-- Get Location where data is stored
getDataLocation :: Ptr ValueInfo -> IO (Ptr a)
getDataLocation pValueInfo = do
    CBool byVal <- (#peek struct ValueInfo, ByVal) pValueInfo
    if toBool byVal
    then return ((#ptr struct ValueInfo, Value) pValueInfo)
    else (#peek struct ValueInfo, Value) pValueInfo

-- Read a Haskell type from a ValueInfo struct
read :: (Ptr ValueInfo -> IO a) -> Ptr ValueInfo -> IO (Maybe a)
read read' pValueInfo = do
    isNull <- readIsNull pValueInfo
    if isNull
    then return Nothing
    else read' pValueInfo <&> Just

-- Set size of variable length array
foreign import capi safe "postgres.h VARSIZE_ANY_EXHDR"
    c_GetVarSize :: Ptr () -> IO CInt

getVarSize :: Ptr () -> IO Int32
getVarSize pArg = do
    CInt size <- c_GetVarSize pArg
    return size

-- Functions to read Haskell types from ValueInfo structs
readBytea :: Ptr ValueInfo -> IO [Word8]
readBytea pValueInfo = do
    pArg <- getDataLocation pValueInfo
    len <- getVarSize pArg
    peekArray (fromIntegral len) (plusPtr pArg (#const VARHDRSZ))

readText :: Ptr ValueInfo -> IO String
readText pValueInfo = do
    pArg <- getDataLocation pValueInfo
    len <- getVarSize pArg
    peekCStringLen (plusPtr pArg (#const VARHDRSZ), fromIntegral len)

readChar :: Ptr ValueInfo -> IO Char
readChar pValueInfo = readText pValueInfo <&> head

readNumber :: Storable a => Ptr ValueInfo -> IO a
readNumber pValueInfo = getDataLocation pValueInfo >>= peek

readBool :: Ptr ValueInfo -> IO Bool
readBool pValueInfo = do
    CBool arg <- readNumber pValueInfo
    return (toBool arg)

readInt2 :: Ptr ValueInfo -> IO Int16
readInt2 = readNumber

readInt4 :: Ptr ValueInfo -> IO Int32
readInt4 = readNumber

readInt8 :: Ptr ValueInfo -> IO Int64
readInt8 = readNumber

readFloat4 :: Ptr ValueInfo -> IO Float
readFloat4 = readNumber

readFloat8 :: Ptr ValueInfo -> IO Double
readFloat8 = readNumber

-- Return a string representing a function to take and argument from a ValueInfo struct and return the Haskell value
readArgDef :: Ptr ValueInfo -> IO String
readArgDef pValueInfo = do
    CInt typ <- (#peek struct ValueInfo, Type) pValueInfo
    let getFieldDef i = do
        readFieldDef <- getField pValueInfo i >>= readArgDef
        return $ interpolate ("field? <- getField pValueInfo ? Prelude.>>= (" ++ readFieldDef ++ ");") i
    let def | typ == (#const BASE_TYPE) = do
                CInt typeOid <- (#peek struct ValueInfo, TypeOid) pValueInfo
                return (readFunctionName typeOid)
            | typ == (#const COMPOSITE_TYPE) = do
                CShort count <- (#peek struct ValueInfo, Count) pValueInfo
                fieldsDef <- mapM getFieldDef [0 .. count-1]
                return ("\\pValueInfo -> do {isNull <- readIsNull pValueInfo; if isNull; then Prelude.return Prelude.Nothing; else do {" ++
                    concat fieldsDef ++ "Prelude.return (Prelude.Just (" ++ intercalate ", " (map (interpolate "field?") [0 .. count-1]) ++ "))}}")
            | otherwise = error "Bad Type"
    def

-- Set up interpreter to evaluate a function
setUpEvalInt :: Ptr CallInfo -> Interpreter (Int16, String)
setUpEvalInt pCallInfo = do
    set [languageExtensions := [Safe], installedModulesInScope := False]
    modFileName <- getModFileName pCallInfo
    loadModules [modFileName]

    --Name of function
    funcName <- getFuncName pCallInfo
    setImportsF [ModuleImport "Prelude"           (QualifiedAs Nothing) (ImportList ["Bool", "Char", "Double", "Float", "IO", "Maybe(Just, Nothing)", "String", "(.)", "(>>=)"]),
                 ModuleImport "Foreign.Ptr"       (QualifiedAs Nothing) (ImportList ["Ptr", "wordPtrToPtr"]),
                 ModuleImport "Foreign.Storable"  (QualifiedAs Nothing) (ImportList ["peek", "poke"]),
                 ModuleImport "Foreign.StablePtr" (QualifiedAs Nothing) (ImportList ["castPtrToStablePtr", "deRefStablePtr", "freeStablePtr", "newStablePtr"]),
                 ModuleImport "PGmodule"          (QualifiedAs Nothing) (ImportList [funcName]),
                 ModuleImport "PGutils"           (QualifiedAs Nothing) (ImportList ["unPGm"]),
                 ModuleImport "Data.Int"          (QualifiedAs Nothing) (ImportList ["Int16", "Int32", "Int64"]),
                 ModuleImport "Data.Word"         (QualifiedAs Nothing) (ImportList ["Word8"])]

    liftIO (catch (removeFile modFileName) ignore) -- Delete the temp file

    -- Copy functions into interpreted environment
    #transfer getField, getField, Foreign.Ptr.Ptr () -> Data.Int.Int16 -> Prelude.IO (Foreign.Ptr.Ptr ())

    #transfer readIsNull, readIsNull, Foreign.Ptr.Ptr () -> Prelude.IO Prelude.Bool
    #transfer read readBytea,  readBytea,  Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe [Data.Word.Word8])
    #transfer read readText,   readText,   Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Prelude.String)
    #transfer read readChar,   readChar,   Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Prelude.Char)
    #transfer read readBool,   readBool,   Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Prelude.Bool)
    #transfer read readInt2,   readInt2,   Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Data.Int.Int16)
    #transfer read readInt4,   readInt4,   Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Data.Int.Int32)
    #transfer read readInt8,   readInt8,   Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Data.Int.Int64)
    #transfer read readFloat4, readFloat4, Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Prelude.Float)
    #transfer read readFloat8, readFloat8, Foreign.Ptr.Ptr () -> Prelude.IO (Prelude.Maybe Prelude.Double)

    #transfer writeNull,    writeNull,    Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer writeNotNull, writeNotNull, Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer writeVoid, writeVoid, () -> Foreign.Ptr.Ptr () -> Prelude.IO ()

    #transfer write writeBytea,  writeBytea,   Prelude.Maybe [Data.Word.Word8] -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeText,   writeText,    Prelude.Maybe Prelude.String    -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeChar,   writeChar,    Prelude.Maybe Prelude.Char      -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeBool,   writeBool,    Prelude.Maybe Prelude.Bool      -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeInt2,   writeInt2,    Prelude.Maybe Data.Int.Int16    -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeInt4,   writeInt4,    Prelude.Maybe Data.Int.Int32    -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeInt8,   writeInt8,    Prelude.Maybe Data.Int.Int64    -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeFloat4, writeFloat4,  Prelude.Maybe Prelude.Float     -> Foreign.Ptr.Ptr () -> Prelude.IO ()
    #transfer write writeFloat8, writeFloat8,  Prelude.Maybe Prelude.Double    -> Foreign.Ptr.Ptr () -> Prelude.IO ()

    -- Number of arguments
    CShort nargs <- (liftIO . (#peek struct CallInfo, nargs)) pCallInfo

    -- Fill all readArg? values with functions to read arguments from ValueInfo structs
    foldl (>>) (return ()) [liftIO (getArgValueInfo pCallInfo i) >>=
        (liftIO . readArgDef) >>= (\argDef -> runStmt (interpolate ("let readArg? = " ++ argDef) i))| i <- [0 .. nargs-1]]

    -- Fill writeResult value with function to write result to ValueInfo struct
    pResultValueInfo <- (liftIO . (#peek struct CallInfo, Result)) pCallInfo
    resultDef <- (liftIO . writeResultDef) pResultValueInfo
    runStmt ("let writeResult = " ++ resultDef)

    -- Set pArgValueInfo? and pResultValueInfo to point to ValueInfo structs
    foldl (>>) (return ()) [liftIO (getArgValueInfo pCallInfo i) >>=
        (\pArgValueInfo -> runStmt (interpolate "let pArgValueInfo? = Foreign.Ptr.wordPtrToPtr " i ++ (show . ptrToWordPtr) pArgValueInfo)) | i <- [0 .. nargs-1]]
    runStmt ("let pResultValueInfo = Foreign.Ptr.wordPtrToPtr " ++ (show . ptrToWordPtr) pResultValueInfo)

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
    set [languageExtensions := [Safe], installedModulesInScope := False]
    modFileName <- getModFileName pCallInfo
    loadModules [modFileName]

    funcName <- getFuncName pCallInfo
    setImportsF [ModuleImport "Prelude"   (QualifiedAs Nothing) (ImportList ["Bool", "Char", "Double", "Float", "Maybe", "String"]),
                 ModuleImport "PGmodule"  (QualifiedAs Nothing) (ImportList [funcName]),
                 ModuleImport "PGutils"   (QualifiedAs Nothing) (ImportList ["PGm"]),
                 ModuleImport "Data.Int"  (QualifiedAs Nothing) (ImportList ["Int16", "Int32", "Int64"]),
                 ModuleImport "Data.Word" (QualifiedAs Nothing) (ImportList ["Word8"])]

    liftIO (catch (removeFile modFileName) ignore)

    signature <- getSignature pCallInfo
    r <- typeChecksWithDetails ("PGmodule." ++ funcName ++ "::" ++ signature)
    case r of
        Left []      -> (liftIO . raiseError) "Unknown Signature Error"
        Left (err:_) -> (liftIO . raiseError) (errMsg err)
        Right _      -> return ()

-- Set the Function field of the CallInfo struct to a function that will read the arguments, call the function, and write the result
foreign export capi mkFunction :: Ptr CallInfo -> IO ()
mkFunction :: Ptr CallInfo -> IO ()
mkFunction pCallInfo = execute $ do
    (nargs, funcName) <- setUpEvalInt pCallInfo

    let prog_read_args = concatMap (interpolate "arg? <- readArg? pArgValueInfo?;") [0 .. nargs-1]
    let prog_call = "result <- PGutils.unPGm (PGmodule." ++ funcName ++ concatMap (interpolate " arg?") [0 .. nargs-1] ++ ");"
    let prog_write_result = "writeResult result pResultValueInfo"
    runStmt ("spFunction <- Foreign.StablePtr.newStablePtr (do {" ++ prog_read_args ++ prog_call ++ prog_write_result ++ "})")

    -- Poke the value of the pointer into the Function field of the CallInfo struct
    runStmt ("Foreign.Storable.poke (Foreign.Ptr.wordPtrToPtr " ++ show ((#ptr struct CallInfo, Function) pCallInfo) ++ ") spFunction")

-- Set the List field of the CallInfo struct to the list returns by the function
-- Set the Iterator field of the CallInfo struct to a function that will iterate through the list
-- Each call of the iterator :
--     Writes the head of the list to the Result ValueInfo struct
--     Advances the List pointer
foreign export capi mkIterator :: Ptr CallInfo -> IO ()
mkIterator :: Ptr CallInfo -> IO ()
mkIterator pCallInfo = execute $ do
    (nargs, funcName) <- setUpEvalInt pCallInfo

    -- Get the arguments
    foldl (>>) (return ()) [runStmt (interpolate "arg? <- readArg? pArgValueInfo?" i) | i <- [0 .. nargs-1]]

    -- Get a stable pointer to the list
    runStmt ("spList <- PGutils.unPGm (PGmodule." ++ funcName ++ concatMap (interpolate " arg?") [0 .. nargs-1] ++ ") Prelude.>>= Foreign.StablePtr.newStablePtr")

    -- poke the stable pointer value into the List field of the CallInfo struct
    runStmt ("let pList = Foreign.Ptr.wordPtrToPtr " ++ show ((#ptr struct CallInfo, List) pCallInfo))
    runStmt "Foreign.Storable.poke pList spList"

    -- poke the iterator into the Iterator field of the CallInfo struct
    runStmt "spIterator <- Foreign.StablePtr.newStablePtr (do {spList <- Foreign.Storable.peek pList; head:tail <- Foreign.StablePtr.deRefStablePtr spList; Foreign.StablePtr.freeStablePtr spList; spTail <- Foreign.StablePtr.newStablePtr tail; Foreign.Storable.poke pList spTail; writeResult head pResultValueInfo})"
    runStmt ("Foreign.Storable.poke (Foreign.Ptr.wordPtrToPtr " ++ show ((#ptr struct CallInfo, Iterator) pCallInfo) ++ ") spIterator")

-- Is the list that has the remaining data from a set returning function empty?
foreign export capi listEmpty :: Ptr CallInfo -> IO CBool
listEmpty :: Ptr CallInfo -> IO CBool
listEmpty pCallInfo = do
    list <- (#peek struct CallInfo, List) pCallInfo >>= deRefStablePtr :: IO ([a])
    return $ CBool $ fromBool (null list)

-- Run the function pointed to by the stable pointer
foreign export capi runFunction :: StablePtr (IO ()) -> IO ()
runFunction :: StablePtr (IO ()) -> IO ()
runFunction = join . deRefStablePtr
