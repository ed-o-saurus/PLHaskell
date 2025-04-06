CREATE FUNCTION echo(delta) RETURNS delta IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (PGm)

    echo :: Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()) ->
        PGm (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    echo = return
$$
LANGUAGE plhaskell;
