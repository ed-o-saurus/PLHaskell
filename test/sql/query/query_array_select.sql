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

CREATE FUNCTION query_array_select()
RETURNS integer[] IMMUTABLE
AS $$
  import Data.Int
    ( Int32,
    )
  import Data.Text
    ( Text,
    )
  import PGutils
    ( Array
        ( Array6D
        ),
      PGm,
      QueryResultValue
        ( QueryResultValueArray,
          QueryResultValueInt4
        ),
      QueryResults
        ( SelectResults
        ),
      query,
      raiseError,
    )

  assert :: Bool -> Text -> PGm ()
  assert True _msg = return ()
  assert False msg = raiseError msg

  query_array_select :: PGm (Maybe (Array (Maybe Int32)))
  query_array_select = do
    SelectResults _processed [_header] [[QueryResultValueArray (_schema, name) a]] <- query "SELECT a FROM query_arrays" []
    assert (name == "int4") "Bad type name"
    return (fmap (fmap (\(QueryResultValueInt4 i) -> i)) a)
$$
LANGUAGE plhaskell;
