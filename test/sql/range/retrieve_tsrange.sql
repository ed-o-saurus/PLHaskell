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

CREATE FUNCTION retrieve_tsrange(integer)
RETURNS tsrange IMMUTABLE
AS $$
  import Data.Int
    ( Int32,
    )
  import Data.Text
    ( Text,
    )
  import PGutils
    ( PGm,
      QueryParam
        ( QueryParamInt4
        ),
      QueryResultValue
        ( QueryResultValueRange,
          QueryResultValueTimestamp
        ),
      QueryResults
        ( SelectResults
        ),
      Range,
      Timestamp,
      query,
      raiseError,
    )

  assert :: Bool -> Text -> PGm ()
  assert True _msg = return ()
  assert False msg = raiseError msg

  convert :: QueryResultValue -> Timestamp
  convert (QueryResultValueTimestamp (Just ts)) = ts

  retrieve_tsrange :: Maybe Int32 -> PGm (Maybe (Range Timestamp))
  retrieve_tsrange i = do
    SelectResults _processed [_header] [[QueryResultValueRange (_schema, name) retVal]] <- query "SELECT r FROM t WHERE i=$1" [QueryParamInt4 i]
    assert (name == "tsrange") "Bad type name"
    return $ fmap (fmap convert) retVal
$$
LANGUAGE plhaskell;
