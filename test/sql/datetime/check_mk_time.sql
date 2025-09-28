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

CREATE FUNCTION check_mk_time(integer
                            , integer
                            , integer
                            , integer)
RETURNS boolean
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( PGm,
      Timestamp,
      hour,
      microsecond,
      minute,
      mkTime,
      second,
    )

  check_mk_time :: Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> PGm (Maybe Bool)
  check_mk_time (Just hour') (Just minute') (Just second') (Just microsecond') =
    return $ Just $ hour_match && minute_match && second_match && microsecond_match
    where
      t = mkTime hour' minute' second' microsecond'
      hour_match = hour t == hour'
      minute_match = minute t == minute'
      second_match = second t == second'
      microsecond_match = microsecond t == microsecond'
$$
LANGUAGE plhaskell;
