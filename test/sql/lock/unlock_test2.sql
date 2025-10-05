-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2025 Edward F. Behn, Jr.
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

CREATE FUNCTION unlock_test(char
                          , integer
                          , integer)
RETURNS boolean
AS $$
  import Data.Functor
    ( (<$>),
    )
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( LockMode (..),
      PGm,
      unlock,
    )

  getMode :: Maybe Char -> LockMode
  getMode (Just 'S') = Shared
  getMode (Just 'X') = Exclusive

  unlock_test :: Maybe Char -> Maybe Int32 -> Maybe Int32 -> PGm (Maybe Bool)
  unlock_test mode (Just key1) (Just key2) = Just <$> unlock (getMode mode) (key1, key2)
$$
LANGUAGE plhaskell;
