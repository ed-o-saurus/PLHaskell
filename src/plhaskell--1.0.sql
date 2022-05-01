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

CREATE TRUSTED LANGUAGE plhaskell
  HANDLER   plhaskell_call_handler
  VALIDATOR plhaskell_validator;

COMMENT ON LANGUAGE plhaskell IS 'PL/Haskell procedural language';
