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

CREATE FUNCTION compare_intervals(interval
                                , interval)
RETURNS boolean
AS $$
  import PGutils
    ( Interval,
      PGm,
    )

  check_compare :: Ordering -> Interval -> Interval -> Bool
  check_compare LT interval1 interval2 = (interval1 < interval2)
  check_compare EQ interval1 interval2 = (interval1 == interval2)
  check_compare GT interval1 interval2 = (interval1 > interval2)

  compare_intervals :: Maybe Interval -> Maybe Interval -> PGm (Maybe Bool)
  compare_intervals (Just interval1) (Just interval2) = return $ Just $ check_compare (compare interval1 interval2) interval1 interval2
$$
LANGUAGE plhaskell;
