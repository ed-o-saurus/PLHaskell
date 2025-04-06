CREATE FUNCTION forty_two() RETURNS int IMMUTABLE AS
$$
    import PGutils (unPGm, query, QueryParam (QueryParamInt4), QueryResults (SelectResults), QueryResultValue (QueryResultValueInt4))
    import Data.Int (Int32)

    forty_two :: IO (Maybe Int32)
    forty_two = do
        SelectResults _processed [_header] [[QueryResultValueInt4 val]] <- unPGm $ query "SELECT $1" [QueryParamInt4 (Just 42)]
        return val
$$
LANGUAGE plhaskellu;
