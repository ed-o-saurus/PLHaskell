-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2024 Edward F. Behn, Jr.
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

CREATE FUNCTION mk_array(integer)
RETURNS integer[] IMMUTABLE
AS $$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( Array (..),
      PGm,
    )
  
  chunksOf :: Int -> [a] -> [[a]]
  chunksOf _ [] = []
  chunksOf n xs = fxs : chunksOf n sxs
    where
      (fxs, sxs) = splitAt n xs
  
  mkList :: Int32 -> [Maybe Int32]
  mkList upper = [if x `mod` 3 == 0 then Nothing else Just x | x <- [0 .. upper - 1]]
  
  mk_array :: Maybe Int32 -> PGm (Maybe (Array (Maybe Int32)))
  mk_array m = case m of
    Nothing -> return Nothing
    Just 0 -> return (Just ArrayEmpty)
    Just 1 -> return (Just (Array1D 20 (mkList 2)))
    Just 2 -> return (Just (Array2D (20, 21) ((chunksOf 2) (mkList 6))))
    Just 3 -> return (Just (Array3D (20, 21, 22) ((chunksOf 3) $ (chunksOf 2) (mkList 24))))
    Just 4 -> return (Just (Array4D (20, 21, 22, 23) ((chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList 120))))
    Just 5 -> return (Just (Array5D (20, 21, 22, 23, 24) ((chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList 720))))
    Just 6 -> return (Just (Array6D (20, 21, 22, 23, 24, 25) ((chunksOf 6) $ (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList 5040))))
$$
LANGUAGE plhaskell;
