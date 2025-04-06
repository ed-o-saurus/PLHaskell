CREATE FUNCTION echo(float) RETURNS float IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;
