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

#{include "postgres.h"}
#{include "storage/lock.h"}

module PGutils.Lock
  ( LockMode (..),
    LockLevel (..),
    Lockable,
    lock',
    tryLock',
    unlock',
    unlockAll',
  )
where

import Data.Functor
  ( void,
    (<$>),
  )
import Data.Int
  ( Int32,
    Int64,
  )
import Data.Word
  ( Word16,
  )
import Foreign.C.Types
  ( CBool
      ( CBool
      ),
    CInt
      ( CInt
      ),
  )
import Foreign.Marshal.Utils
  ( fromBool,
    toBool,
  )
import Foreign.Ptr
  ( Ptr,
  )
import Foreign.Storable
  ( Storable,
    alignment,
    peek,
    poke,
    sizeOf,
  )
import PGutils.Common
  ( pWith,
  )
import Prelude
  ( Bool (..),
    Eq,
    IO,
    Num,
    return,
    undefined,
    ($),
    (/=),
  )

newtype LockModeNum = LockModeNum CInt deriving (Eq, Num, Storable)

newtype LockAcquireResult = LockAcquireResult CInt deriving (Eq, Num, Storable)

newtype LockMethodId = LockMethodId Word16 deriving (Eq, Num, Storable)

data LockMode = Shared | Exclusive

lockModeCode :: LockMode -> LockModeNum
lockModeCode Shared = #{const ShareLock}
lockModeCode Exclusive = #{const ExclusiveLock}

data LockLevel = Transaction | Session

isSession :: LockLevel -> Bool
isSession Transaction = False
isSession Session = True

foreign import capi safe "../lock_plh.h set_tag_1key"
  cSetTag1Key :: (Ptr LockTag) -> Int64 -> IO ()

foreign import capi safe "../lock_plh.h set_tag_2key"
  cSetTag2Key :: (Ptr LockTag) -> Int32 -> Int32 -> IO ()

data LockTag = LockTag1 Int64 | LockTag2 (Int32, Int32)

instance Storable LockTag where
  sizeOf _ = #{size LOCKTAG}
  alignment _ = #{alignment LOCKTAG}

  peek = undefined -- Never used

  poke pTag (LockTag1 key) = cSetTag1Key pTag key
  poke pTag (LockTag2 (key1, key2)) = cSetTag2Key pTag key1 key2

class Lockable a where
  makeTag :: a -> LockTag

instance Lockable Int64 where
  makeTag = LockTag1

instance Lockable (Int32, Int32) where
  makeTag = LockTag2

foreign import ccall safe "storage/lock.h LockAcquire"
  cLockAcquire :: Ptr LockTag -> LockModeNum -> CBool -> CBool -> IO LockAcquireResult

lockAcquire :: LockMode -> LockLevel -> Bool -> LockTag -> IO LockAcquireResult
lockAcquire lockMode lockLevel dontWait lockTag = pWith lockTag $ \pTag -> cLockAcquire pTag (lockModeCode lockMode) (fromBool (isSession lockLevel)) (fromBool dontWait)

foreign import ccall safe "storage/lock.h LockRelease"
  cLockRelease :: Ptr LockTag -> LockModeNum -> CBool -> IO CBool

lockRelease :: LockMode -> LockLevel -> LockTag -> IO Bool
lockRelease lockMode lockLevel lockTag = toBool <$> (pWith lockTag $ \pTag -> cLockRelease pTag (lockModeCode lockMode) (fromBool (isSession lockLevel)))

lock' :: (Lockable a) => LockMode -> LockLevel -> a -> IO ()
lock' lockMode lockLevel key = void $ lockAcquire lockMode lockLevel False $ makeTag key

tryLock' :: (Lockable a) => LockMode -> LockLevel -> a -> IO Bool
tryLock' lockMode lockLevel key = do
  lockAcquireResult <- lockAcquire lockMode lockLevel True $ makeTag key
  return $ lockAcquireResult /= #{const LOCKACQUIRE_NOT_AVAIL}

unlock' :: (Lockable a) => LockMode -> a -> IO Bool
unlock' lockMode key = lockRelease lockMode Session $ makeTag key

foreign import ccall safe "storage/lock.h LockReleaseSession"
  cLockReleaseSession :: LockMethodId -> IO ()

unlockAll' :: IO ()
unlockAll' = cLockReleaseSession #{const USER_LOCKMETHOD}
