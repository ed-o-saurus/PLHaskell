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

CREATE FUNCTION check_mk_interval(integer
                                , integer
                                , integer
                                , integer
                                , integer
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
    ( Interval,
      PGm,
      days,
      hours,
      microseconds,
      minutes,
      mkInterval,
      months,
      seconds,
      years,
    )
  
  check_mk_interval :: Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> PGm (Maybe Bool)
  check_mk_interval (Just years') (Just months') (Just weeks') (Just days') (Just hours') (Just minutes') (Just seconds') (Just microseconds') (Just expect_years') (Just expect_months') (Just expect_days') (Just expect_hours') (Just expect_minutes') (Just expect_seconds') (Just expect_microseconds') =
    return $ Just $ years_match && months_match && days_match && hours_match && minutes_match && seconds_match && microseconds_match
    where
      i = mkInterval years' months' weeks' days' hours' minutes' seconds' microseconds'
      years_match = years i == expect_years'
      months_match = months i == expect_months'
      days_match = days i == expect_days'
      hours_match = hours i == expect_hours'
      minutes_match = minutes i == expect_minutes'
      seconds_match = seconds i == expect_seconds'
      microseconds_match = microseconds i == expect_microseconds'
$$
LANGUAGE plhaskell;
