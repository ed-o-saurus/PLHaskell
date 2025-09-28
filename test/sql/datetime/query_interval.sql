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

CREATE FUNCTION query_interval(interval)
RETURNS interval
AS $$
  import PGutils
    ( Interval,
      PGm,
      QueryParam
        ( QueryParamInterval
        ),
      QueryResultValue
        ( QueryResultValueInterval
        ),
      QueryResults
        ( SelectResults
        ),
      query,
    )

  query_interval :: Maybe Interval -> PGm (Maybe Interval)
  query_interval mInterval = do
    SelectResults _processed [_header] [[QueryResultValueInterval retVal]] <- query "SELECT $1" [QueryParamInterval mInterval]
    return retVal
$$
LANGUAGE plhaskell;
