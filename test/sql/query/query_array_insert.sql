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

CREATE FUNCTION query_array_insert()
RETURNS void VOLATILE
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( Array
        ( Array6D
        ),
      PGm,
      QueryParam
        ( QueryParamArray,
          QueryParamInt4
        ),
      QueryResults
        ( InsertResults
        ),
      query,
    )

  chunksOf :: Int -> [a] -> [[a]]
  chunksOf _ [] = []
  chunksOf n xs = fxs : chunksOf n sxs
    where
      (fxs, sxs) = splitAt n xs

  mkList :: Int32 -> [Maybe Int32]
  mkList upper = [if x `mod` 3 == 0 then Nothing else Just x | x <- [0 .. upper - 1]]

  mk_array :: Maybe (Array QueryParam)
  mk_array = Just $ Array6D (20, 21, 22, 23, 24, 25) ((chunksOf 6) $ (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (map QueryParamInt4 (mkList 5040)))

  query_array_insert :: PGm ()
  query_array_insert = do
    InsertResults _processed <- query "INSERT INTO query_arrays(a) VALUES ($1)" [QueryParamArray (Nothing, "int4") mk_array]
    return ()
$$
LANGUAGE plhaskell;
