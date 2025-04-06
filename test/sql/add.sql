CREATE FUNCTION add(int, int) RETURNS int IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)

    add :: Maybe Int32 -> Maybe Int32 -> PGm (Maybe Int32)
    add (Just a) (Just b) = return (Just (a+b))
    add a Nothing = return a
    add Nothing _ = return Nothing
$$
LANGUAGE plhaskell;
