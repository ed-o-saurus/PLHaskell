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

DO $$
BEGIN
    IF alpha_func(1) <> ('abc', 42, NULL)::alpha THEN
        raise EXCEPTION 'alpha_func(1) failed';
    END IF;

    IF alpha_func(2) <> ('cde', NULL, 12.3)::alpha THEN
        raise EXCEPTION 'alpha_func(2) failed';
    END IF;

    IF alpha_func(3) <> (NULL, 42, 32.1)::alpha THEN
        raise EXCEPTION 'alpha_func(3) failed';
    END IF;

    IF alpha_func(4) is not NULL THEN
        raise EXCEPTION 'alpha_func(4) failed';
    END IF;
END
$$;
