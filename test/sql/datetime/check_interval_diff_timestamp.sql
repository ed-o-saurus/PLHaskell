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

CREATE FUNCTION check_interval_diff_timestamp(timestamp
                                            , timestamp
                                            , interval)
RETURNS boolean
AS $$
  import PGutils
    ( Interval,
      PGm,
      Timestamp,
      addInterval,
      diff,
      subInterval,
    )

  check_interval_diff_timestamp :: Maybe Timestamp -> Maybe Timestamp -> Maybe Interval -> PGm (Maybe Bool)
  check_interval_diff_timestamp (Just a) (Just b) (Just i) =
    return $ Just $ add_match && sub_match && diff_match
    where
      add_match = a == addInterval b i
      sub_match = b == subInterval a i
      diff_match = i == diff a b
$$
LANGUAGE plhaskell;
