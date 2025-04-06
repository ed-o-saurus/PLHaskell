CREATE FUNCTION query_drop() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (UtilityResults))

    query_drop :: PGm ()
    query_drop = do
        UtilityResults _processed <- query "DROP TABLE t" []
        return ()
$$
LANGUAGE plhaskell;
