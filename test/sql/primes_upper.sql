CREATE FUNCTION primes_upper(minimum int) RETURNS SETOF n_p IMMUTABLE AS
$$
    import PGutils (unPGm, query, QueryParam (QueryParamInt4), QueryResults (SelectResults), QueryResultValue (QueryResultValueInt4))
    import Data.Int (Int32)

    primes_upper :: Maybe Int32 -> IO [Maybe (Maybe Int32, Maybe Int32)]
    primes_upper minimum = do
        SelectResults _processed [_header_n, _header_p] rows <- unPGm $ query "SELECT n, p FROM primes WHERE p >= $1" [QueryParamInt4 minimum]
        return $ map get_data rows
        where get_data [QueryResultValueInt4 n, QueryResultValueInt4 p] = Just (n, p)
$$
LANGUAGE plhaskellu;
