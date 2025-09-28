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

CREATE FUNCTION query_composite()
RETURNS void IMMUTABLE
AS $$
  import Data.Text
    ( Text,
    )
  import PGutils
    ( PGm,
      QueryResultValue
        ( QueryResultValueComposite,
          QueryResultValueFloat8,
          QueryResultValueInt4,
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
  
  query_composite :: PGm ()
  query_composite = do
    SelectResults processed [header] [row0, row1, row2, row3] <- query "SELECT d FROM deltas ORDER BY i" []
  
    assert (processed == 4) "Bad processed"
    assert (header == "d") "Bad header"
  
    let [QueryResultValueComposite (delta_schema0, delta_type0) (Just [QueryResultValueComposite (bravo_schema0, bravo_type0) (Just [QueryResultValueComposite (alpha_schema0, alpha_type0) (Just [QueryResultValueText (Just x0), QueryResultValueInt4 (Just x1), QueryResultValueFloat8 (Just x2)]), QueryResultValueInt4 (Just x3)]), QueryResultValueComposite (charlie_schema0, charlie_type0) (Just [])])] = row0
  
    assert (delta_schema0 == "plhaskell_test") "Bad delta_schema0"
    assert (delta_type0 == "delta") "Bad delta_type0"
    assert (bravo_schema0 == "plhaskell_test") "Bad bravo_schema0"
    assert (bravo_type0 == "bravo") "Bad bravo_type0"
    assert (alpha_schema0 == "plhaskell_test") "Bad alpha_schema0"
    assert (alpha_type0 == "alpha") "Bad alpha_type0"
    assert (charlie_schema0 == "plhaskell_test") "Bad charlie_schema0"
    assert (charlie_type0 == "charlie") "Bad charlie_type0"
    assert (x0 == "Hello") "Bad x0"
    assert (x1 == 12) "Bad x1"
    assert (x2 == 3.4) "Bad x2"
    assert (x3 == 76) "Bad x3"
  
    let [QueryResultValueComposite (delta_schema1, delta_type1) (Just [QueryResultValueComposite (bravo_schema1, bravo_type1) (Just [QueryResultValueComposite (alpha_schema1, alpha_type1) (Just [QueryResultValueText (Just x4), QueryResultValueInt4 (Just x5), QueryResultValueFloat8 (Just x6)]), QueryResultValueInt4 (Just x7)]), QueryResultValueComposite (charlie_schema1, charlie_type1) Nothing])] = row1
  
    assert (delta_schema1 == "plhaskell_test") "Bad delta_schema1"
    assert (delta_type1 == "delta") "Bad delta_type1"
    assert (bravo_schema1 == "plhaskell_test") "Bad bravo_schema1"
    assert (bravo_type1 == "bravo") "Bad bravo_type1"
    assert (alpha_schema1 == "plhaskell_test") "Bad alpha_schema1"
    assert (alpha_type1 == "alpha") "Bad alpha_type1"
    assert (charlie_schema1 == "plhaskell_test") "Bad charlie_schema1"
    assert (charlie_type1 == "charlie") "Bad charlie_type1"
    assert (x4 == "world") "Bad x4"
    assert (x5 == 42) "Bad x5"
    assert (x6 == 1.0) "Bad x6"
    assert (x7 == -12) "Bad x7"
  
    let [QueryResultValueComposite (delta_schema2, delta_type2) (Just [QueryResultValueComposite (bravo_schema2, bravo_type2) Nothing, QueryResultValueComposite (charlie_schema2, charlie_type2) (Just [])])] = row2
  
    assert (delta_schema2 == "plhaskell_test") "Bad delta_schema2"
    assert (delta_type2 == "delta") "Bad delta_type2"
    assert (bravo_schema2 == "plhaskell_test") "Bad bravo_schema2"
    assert (bravo_type2 == "bravo") "Bad bravo_type2"
    assert (charlie_schema2 == "plhaskell_test") "Bad charlie_schema2"
    assert (charlie_type2 == "charlie") "Bad charlie_type2"
  
    let [QueryResultValueComposite (delta_schema3, delta_type3) Nothing] = row3
  
    assert (delta_schema3 == "plhaskell_test") "Bad delta_schema3"
    assert (delta_type3 == "delta") "Bad delta_type3"
  
    return ()
$$
LANGUAGE plhaskell;
