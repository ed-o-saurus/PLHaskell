CREATE FUNCTION make_length_text(int) RETURNS text IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    import Data.Text (pack, Text)

    make_length_text :: Maybe Int32 -> PGm (Maybe Text)
    make_length_text Nothing = return Nothing
    make_length_text (Just n) = return $ Just $ pack (take (fromIntegral n) (repeat '_'))
$$
LANGUAGE plhaskell;
