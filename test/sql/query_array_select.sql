
CREATE FUNCTION query_array_select() RETURNS int[] IMMUTABLE AS
$$
    import PGutils (PGm, raiseError, query, arrayMap, Array(Array6D), QueryResults(SelectResults), QueryResultValue(QueryResultValueArray, QueryResultValueInt4))
    import Data.Int (Int32)
    import Data.Text (Text)

    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_array_select :: PGm (Maybe (Array (Maybe Int32)))
    query_array_select = do
        SelectResults _processed [_header] [[QueryResultValueArray (_schema, name) a]] <- query "SELECT a FROM query_arrays" [];
        assert (name == "int4") "Bad type name"
        return (fmap (arrayMap (\ (QueryResultValueInt4 i) -> i)) a)
$$
LANGUAGE plhaskell;
