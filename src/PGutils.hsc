{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

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
