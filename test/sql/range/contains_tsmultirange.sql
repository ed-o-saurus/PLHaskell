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

CREATE FUNCTION contains_tsmultirange(tsmultirange
                                    , timestamp)
RETURNS boolean IMMUTABLE
AS $$
  import PGutils
    ( MultiRange,
      PGm,
      containsMulti,
    )

  contains_tsmultirange :: (Ord a) => Maybe (MultiRange a) -> Maybe a -> PGm (Maybe Bool)
  contains_tsmultirange (Just mr) (Just ts) = return $ Just $ mr `containsMulti` ts
  contains_tsmultirange _ _ = return $ Just False
$$
LANGUAGE plhaskell;
