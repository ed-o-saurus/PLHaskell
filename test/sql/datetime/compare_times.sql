-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2024 Edward F. Behn, Jr.
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

CREATE FUNCTION compare_times(time
                            , time)
RETURNS boolean
AS $$
  import PGutils
    ( PGm,
      Time,
    )
  
  check_compare :: Ordering -> Time -> Time -> Bool
  check_compare LT time1 time2 = (time1 < time2)
  check_compare EQ time1 time2 = (time1 == time2)
  check_compare GT time1 time2 = (time1 > time2)
  
  compare_times :: Maybe Time -> Maybe Time -> PGm (Maybe Bool)
  compare_times (Just time1) (Just time2) = return $ Just $ check_compare (compare time1 time2) time1 time2
$$
LANGUAGE plhaskell;
