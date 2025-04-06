CREATE FUNCTION poop() RETURNS char IMMUTABLE AS
$$
    import PGutils (PGm)

    poop :: PGm (Maybe Char)
    poop = return (Just '\x0001F4A9')
$$
LANGUAGE plhaskell;
