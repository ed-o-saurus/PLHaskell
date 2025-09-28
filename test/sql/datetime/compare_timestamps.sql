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

CREATE FUNCTION compare_timestamps(timestamp
                                 , timestamp)
RETURNS boolean
AS $$
  import PGutils
    ( PGm,
      Timestamp,
    )
  
  check_compare :: Ordering -> Timestamp -> Timestamp -> Bool
  check_compare LT timestamp1 timestamp2 = (timestamp1 < timestamp2)
  check_compare EQ timestamp1 timestamp2 = (timestamp1 == timestamp2)
  check_compare GT timestamp1 timestamp2 = (timestamp1 > timestamp2)
  
  compare_timestamps :: Maybe Timestamp -> Maybe Timestamp -> PGm (Maybe Bool)
  compare_timestamps (Just timestamp1) (Just timestamp2) = return $ Just $ check_compare (compare timestamp1 timestamp2) timestamp1 timestamp2
$$
LANGUAGE plhaskell;
