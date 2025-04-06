CREATE FUNCTION nan() RETURNS float IMMUTABLE AS
$$
    import PGutils (PGm)

    nan :: PGm (Maybe Double)
    nan = return (Just (0.0 / 0.0))
$$
LANGUAGE plhaskell;
