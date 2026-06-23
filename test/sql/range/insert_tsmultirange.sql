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

CREATE FUNCTION insert_tsmultirange(integer
                                  , tsmultirange)
RETURNS void VOLATILE
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( MultiRange,
      PGm,
      QueryParam
        ( QueryParamInt4,
          QueryParamMultiRange,
          QueryParamTimestamp
        ),
      QueryResults
        ( InsertResults
        ),
      Timestamp,
      query,
    )

  convert :: Timestamp -> QueryParam
  convert ts = QueryParamTimestamp $ Just ts

  mkMultiRange :: Maybe (MultiRange Timestamp) -> QueryParam
  mkMultiRange Nothing = QueryParamMultiRange (Nothing, "tsmultirange") Nothing
  mkMultiRange (Just r) = QueryParamMultiRange (Nothing, "tsmultirange") $ Just $ fmap convert r

  insert_tsmultirange :: Maybe Int32 -> Maybe (MultiRange Timestamp) -> PGm ()
  insert_tsmultirange i mr = do
    InsertResults _processed <- query "INSERT INTO t2(i, mr) VALUES ($1, $2)" [QueryParamInt4 i, mkMultiRange mr]
    return ()
$$
LANGUAGE plhaskell;
