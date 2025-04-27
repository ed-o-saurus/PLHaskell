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

CREATE FUNCTION delta_func() RETURNS delta IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (PGm)

    delta_func :: PGm (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    delta_func = return (Just (Just (Just (Just "abc", Just 42, Just (42.3)), Just 0), Just ()))
$$
LANGUAGE plhaskell