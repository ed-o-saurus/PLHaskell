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

CREATE FUNCTION haskell_upper(tsrange)
RETURNS timestamp IMMUTABLE
AS $$
  import PGutils
    ( Bound
        ( ClosedBound,
          OpenBound
        ),
      PGm,
      Range
        ( BoundRange
        ),
    )

  haskell_upper :: Maybe (Range a) -> PGm (Maybe a)
  haskell_upper Nothing = return Nothing
  haskell_upper (Just (BoundRange _ (OpenBound b))) = return $ Just b
  haskell_upper (Just (BoundRange _ (ClosedBound b))) = return $ Just b
  haskell_upper (Just _) = return Nothing
$$
LANGUAGE plhaskell;
