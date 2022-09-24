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

import Foreign.C.String (CString, withCString)
import Foreign.C.Types  (CInt(CInt))
import Prelude          (Applicative, Functor, IO, Monad, String, undefined, ($))
import System.IO.Unsafe (unsafePerformIO)

newtype PGm a = PGm (IO a) deriving newtype (Functor, Applicative, Monad)

unPGm :: PGm a -> IO a
unPGm (PGm x) = x

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

foreign import capi safe "plhaskell.h Report"
    c_Report :: CInt -> CString -> IO ()

report :: ErrorLevel -> String -> PGm ()
report elevel msg = PGm (withCString msg (c_Report (unErrorLevel elevel)))

raiseError :: String -> a
raiseError msg = unsafePerformIO $ do
    unPGm (report exception msg)
    undefined
