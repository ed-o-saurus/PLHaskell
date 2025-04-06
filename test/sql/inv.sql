CREATE FUNCTION inv(bool) RETURNS bool IMMUTABLE AS
$$
    import PGutils (PGm)

    inv :: Maybe Bool -> PGm (Maybe Bool)
    inv Nothing = return Nothing
    inv (Just True) = return (Just False)
    inv (Just False) = return (Just True)
$$
LANGUAGE plhaskell;
