CREATE FUNCTION query_create() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (UtilityResults))

    query_create :: PGm ()
    query_create = do
        UtilityResults _processed <- query "CREATE TABLE t(i int, l text)" []
        return ()
$$
LANGUAGE plhaskell;
