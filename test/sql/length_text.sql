CREATE FUNCTION length_text(text) RETURNS int IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Text (length, Text)
    import Data.Int (Int32)

    length_text :: Maybe Text -> PGm (Maybe Int32)
    length_text Nothing = return Nothing
    length_text (Just s) = return $ Just $ fromIntegral $ Data.Text.length s
$$
LANGUAGE plhaskell;
