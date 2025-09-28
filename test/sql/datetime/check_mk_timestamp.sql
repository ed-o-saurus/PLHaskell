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

CREATE FUNCTION check_mk_timestamp(integer
                                 , integer
                                 , integer
                                 , integer
                                 , integer
                                 , integer
                                 , integer
                                 , integer
                                 , integer
                                 , integer
                                 , integer)
RETURNS boolean
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( Month,
      PGm,
      Timestamp,
      day,
      dayOfWeek,
      dayOfYear,
      hour,
      iso,
      microsecond,
      minute,
      mkTimestamp,
      month,
      second,
      year,
    )
  
  check_mk_timestamp :: Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> PGm (Maybe Bool)
  check_mk_timestamp (Just year') (Just month') (Just day') (Just hour') (Just minute') (Just second') (Just microsecond') (Just expect_dow) (Just expect_doy) (Just expect_isoyear) (Just expect_isoweek) =
    return $ Just $ year_match && month_match && day_match && hour_match && minute_match && second_match --   && microsecond_match --  && dow_match -- && doy_match -- && iso_match
    where
      ts = mkTimestamp year' month'' day' hour' minute' second' microsecond'
      month'' = toEnum $ fromIntegral month'
      year_match = year ts == year'
      month_match = month ts == month''
      day_match = day ts == day'
      hour_match = hour ts == hour'
      minute_match = minute ts == minute'
      second_match = second ts == second'
      microsecond_match = microsecond ts == microsecond'
      expect_dow' = toEnum $ fromIntegral expect_dow
      dow_match = dayOfWeek ts == expect_dow'
      doy_match = dayOfYear ts == expect_doy
      iso_match = iso ts == (expect_isoyear, expect_isoweek)
$$
LANGUAGE plhaskell;
