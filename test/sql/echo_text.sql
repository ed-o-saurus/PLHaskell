CREATE FUNCTION echo(text) RETURNS text IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;
