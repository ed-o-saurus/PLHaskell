DO
$$
    import PGutils (PGm, query, QueryResults(InsertResults), QueryParam(QueryParamInt4))

    _' :: PGm ()
    _' = do
        InsertResults _processed <- query "INSERT INTO inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 0), QueryParamInt4 (Just  0)]
        InsertResults _processed <- query "INSERT INTO inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 1), QueryParamInt4 (Just  1)]
        InsertResults _processed <- query "INSERT INTO inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 2), QueryParamInt4 (Just  4)]
        InsertResults _processed <- query "INSERT INTO inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 3), QueryParamInt4 (Just  9)]
        InsertResults _processed <- query "INSERT INTO inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 4), QueryParamInt4 (Just 16)]
        InsertResults _processed <- query "INSERT INTO inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 5), QueryParamInt4 (Just 25)]
        return ()
$$
LANGUAGE plhaskell;
