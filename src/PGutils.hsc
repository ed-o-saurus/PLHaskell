{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

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

-- This module is designed to be imported by each PostgreSQL function

#include "plhaskell.h"

module PGutils (PGm, ErrorLevel, debug5, debug4, debug3, debug2, debug1, log, info, notice, warning, exception, report, raiseError, unPGm) where

import Control.Monad.Fail    (MonadFail (fail))
import Data.ByteString       (useAsCString)
import Data.Text             (Text, pack)
import Data.Text.Encoding    (encodeUtf8)
import Foreign.C.String      (CString)
import Foreign.C.Types       (CInt (CInt))
import Prelude               (Applicative, Functor, IO, Monad, undefined, ($), (.))
import System.IO.Unsafe      (unsafePerformIO)

newtype PGm a = PGm {unPGm :: IO a} deriving newtype (Functor, Applicative, Monad)

newtype ErrorLevel = ErrorLevel {unErrorLevel :: CInt}
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

foreign import capi safe "plhaskell.h plhaskell_report"
    plhaskellReport :: CInt -> CString -> IO ()

-- useAsCString leaks memory and should be replaced
report :: ErrorLevel -> Text -> PGm ()
report elevel msg = PGm $ useAsCString (encodeUtf8 msg) (plhaskellReport (unErrorLevel elevel))

raiseError :: Text -> a
raiseError msg = unsafePerformIO $ do
    unPGm $ report exception msg
    undefined

instance MonadFail PGm where
    fail = raiseError . pack