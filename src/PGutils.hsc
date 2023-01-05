{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

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

-- This module is designed to be imported by each PostgreSQL function

#include "plhaskell.h"

module PGutils (PGm, ErrorLevel, debug5, debug4, debug3, debug2, debug1, log, info, notice, warning, exception, report, raiseError, unPGm) where

import Data.ByteString       (useAsCStringLen)
import Data.Int              (Int32)
import Data.Text             (Text)
import Data.Text.Encoding    (encodeUtf8)
import Foreign.C.String      (CString)
import Foreign.C.Types       (CInt(CInt), CSize (CSize))
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr           (Ptr, castPtr)
import Prelude               (Applicative, Functor, Int, IO, Monad, fromIntegral, return, undefined, ($), (+))
import System.IO.Unsafe      (unsafePerformIO)

newtype PGm a = PGm {unPGm :: IO a} deriving newtype (Functor, Applicative, Monad)

newtype ErrorLevel = ErrorLevel { unErrorLevel :: CInt }
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

foreign import capi safe "plhaskell.h PLHaskell_Report"
    c_Report :: CInt -> CString -> IO ()

raise :: Int32 -> Text -> IO ()
raise level str = do
    ptr <- pallocString str
    c_Report (CInt level) ptr
    pfree ptr

-- Allocate memory using postgres' mechanism and zero the contents
foreign import capi safe "plhaskell.h palloc0"
    c_palloc0 :: CSize -> IO (Ptr a)

palloc0 :: Int -> IO (Ptr a)
palloc0 size = c_palloc0 (CSize (fromIntegral size))

-- Palloc CString
-- Copy a String's contents to palloc'd memory
pallocString :: Text -> IO (Ptr a)
pallocString str = useAsCStringLen (encodeUtf8 str) (\(ptr, len) -> do
    pallocPtr <- palloc0 (len+1) -- Add one to ensure \0 termination
    copyBytes pallocPtr ptr len
    return (castPtr pallocPtr))

-- Free memory using postgres' mechanism
foreign import capi safe "plhaskell.h pfree"
    pfree :: Ptr a -> IO ()

report :: ErrorLevel -> Text -> PGm ()
report elevel msg = do
    let CInt level = unErrorLevel elevel
    PGm (raise level msg)

raiseError :: Text -> a
raiseError msg = unsafePerformIO $ do
    unPGm (report exception msg)
    undefined
