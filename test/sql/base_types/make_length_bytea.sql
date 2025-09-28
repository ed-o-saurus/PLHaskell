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

CREATE FUNCTION make_length_bytea(integer)
RETURNS bytea IMMUTABLE
AS $$
  import Data.ByteString
    ( ByteString,
      pack,
    )
  import Data.Int
    ( Int32,
    )
  import Data.Word
    ( Word8,
    )
  import PGutils
    ( PGm,
    )
  
  make_length :: Int32 -> [Word8]
  make_length n = take (fromIntegral n) (repeat 0)
  
  make_length_bytea :: Maybe Int32 -> PGm (Maybe ByteString)
  make_length_bytea Nothing = return Nothing
  make_length_bytea (Just n) = return $ Just (pack (make_length n))
$$
LANGUAGE plhaskell;
