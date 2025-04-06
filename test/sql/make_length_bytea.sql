CREATE FUNCTION make_length_bytea(int) RETURNS bytea IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.ByteString (pack, ByteString)
    import Data.Int (Int32)
    import Data.Word (Word8)

    make_length :: Int32 -> [Word8]
    make_length n = take (fromIntegral n) (repeat 0)

    make_length_bytea :: Maybe Int32 -> PGm (Maybe ByteString)
    make_length_bytea Nothing = return Nothing
    make_length_bytea (Just n) = return $ Just (pack (make_length n))
$$
LANGUAGE plhaskell;
