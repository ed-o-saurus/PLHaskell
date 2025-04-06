CREATE FUNCTION echo(char) RETURNS char IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;
