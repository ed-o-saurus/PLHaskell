CREATE FUNCTION delta_func() RETURNS delta IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (PGm)

    delta_func :: PGm (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    delta_func = return (Just (Just (Just (Just "abc", Just 42, Just (42.3)), Just 0), Just ()))
$$
LANGUAGE plhaskell