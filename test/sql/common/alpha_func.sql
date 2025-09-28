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

CREATE FUNCTION alpha_func(integer)
RETURNS alpha IMMUTABLE
AS $$
  import Data.Int
    ( Int32,
    )
  import Data.Text
    ( Text,
    )
  import PGutils
    ( PGm,
      raiseError,
    )

  p :: Double
  p = 12.3

  r :: Double
  r = 32.1

  alpha_func :: Maybe Int32 -> PGm (Maybe (Maybe Text, Maybe Int32, Maybe Double))
  alpha_func (Just 1) = return (Just (Just "abc", Just 42, Nothing))
  alpha_func (Just 2) = return (Just (Just "cde", Nothing, Just p))
  alpha_func (Just 3) = return (Just (Nothing, Just 42, Just r))
  alpha_func (Just 4) = return Nothing
  alpha_func _ = raiseError "Invalid"
$$
LANGUAGE plhaskell;
