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

CREATE FUNCTION check_show_read_time(time
                                   , text)
RETURNS boolean
AS $$
  import Data.Text
    ( Text,
      pack,
      unpack,
    )
  import PGutils
    ( PGm,
      Time,
    )
  
  check_show_read_time :: Maybe Time -> Maybe Text -> PGm (Maybe Bool)
  check_show_read_time (Just d) (Just s) =
    return $ Just $ d_match && s_match
    where
      d_match = d == (read $ unpack s)
      s_match = s == (pack $ show d)
$$
LANGUAGE plhaskell;
