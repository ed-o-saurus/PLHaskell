CREATE FUNCTION query_insert(int, text) RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (InsertResults), QueryParam (QueryParamInt4, QueryParamText))
    import Data.Int (Int32)
    import Data.Text (Text)

    query_insert :: Maybe Int32 -> Maybe Text -> PGm ()
    query_insert i l = do
        InsertResults _processed <- query "INSERT INTO t(i, l) VALUES ($1, $2);" [QueryParamInt4 i, QueryParamText l]
        return ()
$$
LANGUAGE plhaskell;
