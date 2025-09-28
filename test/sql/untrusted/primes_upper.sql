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

CREATE FUNCTION primes_upper(integer)
RETURNS SETOF n_p IMMUTABLE
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( QueryParam
        ( QueryParamInt4
        ),
      QueryResultValue
        ( QueryResultValueInt4
        ),
      QueryResults
        ( SelectResults
        ),
      query,
      unPGm,
    )

  primes_upper :: Maybe Int32 -> IO [Maybe (Maybe Int32, Maybe Int32)]
  primes_upper minimum = do
    SelectResults _processed [_header_n, _header_p] rows <- unPGm $ query "SELECT n, p FROM primes WHERE p >= $1" [QueryParamInt4 minimum]
    return $ map get_data rows
    where
      get_data [QueryResultValueInt4 n, QueryResultValueInt4 p] = Just (n, p)
$$
LANGUAGE plhaskellu;
