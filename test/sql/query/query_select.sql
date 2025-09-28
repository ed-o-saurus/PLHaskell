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

CREATE FUNCTION query_select()
RETURNS void IMMUTABLE
AS $$
  import Data.Text
    ( Text,
    )
  import PGutils
    ( PGm,
      QueryResultValue
        ( QueryResultValueInt4,
          QueryResultValueText
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

  query_select :: PGm ()
  query_select = do
    SelectResults
      processed
      [header1, header2]
      [ [QueryResultValueInt4 i0, QueryResultValueText l0],
        [QueryResultValueInt4 i1, QueryResultValueText l1],
        [QueryResultValueInt4 i2, QueryResultValueText l2],
        [QueryResultValueInt4 i3, QueryResultValueText l3],
        [QueryResultValueInt4 i4, QueryResultValueText l4],
        [QueryResultValueInt4 i5, QueryResultValueText l5],
        [QueryResultValueInt4 i6, QueryResultValueText l6]
        ] <-
      query
        "SELECT i, l \
        \ FROM t \
        \ ORDER BY i"
        []

    assert (processed == 7) "Bad processed"
    assert (header1 == "i") "Bad header1"
    assert (header2 == "l") "Bad header2"
    assert (i0 == Just 1) "Bad i0"
    assert (l0 == Just "A") "Bad l0"
    assert (i1 == Just 2) "Bad i1"
    assert (l1 == Just "B") "Bad l1"
    assert (i2 == Just 3) "Bad i2"
    assert (l2 == Just "C") "Bad l2"
    assert (i3 == Just 4) "Bad i3"
    assert (l3 == Just "A") "Bad l3"
    assert (i4 == Just 5) "Bad i4"
    assert (l4 == Just "B") "Bad l4"
    assert (i5 == Just 6) "Bad i5"
    assert (l5 == Just "C") "Bad l5"
    assert (i6 == Nothing) "Bad i6"
    assert (l6 == Nothing) "Bad l6"

    return ()
$$
LANGUAGE plhaskell;
