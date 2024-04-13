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

-- Complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION plhaskell" to load this file. \quit

do $$
DECLARE
  r int;
BEGIN
  FOR r in
    SELECT 1
    FROM pg_database
    WHERE datname = current_database() AND pg_encoding_to_char(encoding) != 'UTF8'
  LOOP
    raise EXCEPTION 'Only UTF-8 encoding supported by PL/Haskell.';
  END LOOP;

  RETURN;
END $$;

CREATE FUNCTION plhaskell_call_handler()
  RETURNS language_handler
  LANGUAGE c AS 'MODULE_PATHNAME';

CREATE FUNCTION plhaskell_validator(oid)
  RETURNS void STRICT
  LANGUAGE c AS 'MODULE_PATHNAME';

CREATE FUNCTION plhaskell_inline_handler(internal)
  RETURNS void
  LANGUAGE C AS 'MODULE_PATHNAME';

CREATE TRUSTED LANGUAGE plhaskell
  HANDLER   plhaskell_call_handler
  INLINE    plhaskell_inline_handler
  VALIDATOR plhaskell_validator;

COMMENT ON LANGUAGE plhaskell IS 'PL/Haskell procedural language';

CREATE LANGUAGE plhaskellu
  HANDLER   plhaskell_call_handler
  INLINE    plhaskell_inline_handler
  VALIDATOR plhaskell_validator;

COMMENT ON LANGUAGE plhaskellu IS 'PL/Haskell procedural language (untrusted)';

CREATE FUNCTION ghc_version()
  RETURNS integer
  IMMUTABLE
  PARALLEL SAFE
  LANGUAGE c AS 'MODULE_PATHNAME';

COMMENT ON FUNCTION ghc_version() IS 'GHC API version used by PL/Haskell procedural language';
