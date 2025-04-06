CREATE FUNCTION length_bytea(bytea) RETURNS int IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.ByteString (length, ByteString)
    import Data.Int (Int32)

    length_bytea :: Maybe ByteString -> PGm (Maybe Int32)
    length_bytea Nothing = return Nothing
    length_bytea (Just s) = return $ Just $ fromIntegral $ Data.ByteString.length s
$$
LANGUAGE plhaskell;
