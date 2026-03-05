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

CREATE FUNCTION check_mk_date(integer
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
    ( Date,
      Month,
      PGm,
      day,
      dayOfWeek,
      dayOfYear,
      iso,
      mkDate,
      month,
      year,
    )

  check_mk_date :: Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> PGm (Maybe Bool)
  check_mk_date (Just year') (Just month') (Just day') (Just expect_dow) (Just expect_doy) (Just expect_isoyear) (Just expect_isoweek) =
    return $ Just $ year_match && month_match && day_match && dow_match && doy_match && iso_match
    where
      d = mkDate year' month'' day'
      month'' = toEnum $ fromIntegral month'
      year_match = year d == year'
      month_match = month d == month''
      day_match = day d == day'
      expect_dow' = toEnum $ fromIntegral expect_dow
      dow_match = dayOfWeek d == expect_dow'
      doy_match = dayOfYear d == expect_doy
      iso_match = iso d == (expect_isoyear, expect_isoweek)
$$
LANGUAGE plhaskell;
