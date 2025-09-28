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

CREATE FUNCTION check_arith_date(date
                               , date
                               , integer)
RETURNS boolean
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( Date,
      Interval,
      PGm,
      dateMinusDate,
      dateMinusInt,
      datePlusInt,
    )
  
  check_arith_date :: Maybe Date -> Maybe Date -> Maybe Int32 -> PGm (Maybe Bool)
  check_arith_date (Just a) (Just b) (Just i) =
    return $ Just $ add_match && sub_match && diff_match
    where
      add_match = a == datePlusInt b i
      sub_match = b == dateMinusInt a i
      diff_match = i == dateMinusDate a b
$$
LANGUAGE plhaskell;
