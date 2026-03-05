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

CREATE FUNCTION lock_test(char
                        , char
                        , integer
                        , integer)
RETURNS void
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( LockLevel (..),
      LockMode (..),
      PGm,
      lock,
    )

  getMode :: Maybe Char -> LockMode
  getMode (Just 'S') = Shared
  getMode (Just 'X') = Exclusive

  getLevel :: Maybe Char -> LockLevel
  getLevel (Just 'T') = Transaction
  getLevel (Just 'S') = Session

  lock_test :: Maybe Char -> Maybe Char -> Maybe Int32 -> Maybe Int32 -> PGm ()
  lock_test mode level (Just key1) (Just key2) = lock (getMode mode) (getLevel level) (key1, key2)
$$
LANGUAGE plhaskell;
