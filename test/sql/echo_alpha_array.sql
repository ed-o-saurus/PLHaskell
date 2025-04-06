CREATE FUNCTION echo(alpha[]) RETURNS alpha[] IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;
