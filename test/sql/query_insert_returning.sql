CREATE FUNCTION query_insert_returning() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (InsertReturningResults), QueryResultValue (QueryResultValueInt4, QueryResultValueText), raiseError)
    import Data.Text (Text)

    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_insert_returning :: PGm ()
    query_insert_returning = do
        InsertReturningResults processed [header1, header2] [[QueryResultValueInt4 i0, QueryResultValueText l0],
                                                             [QueryResultValueInt4 i1, QueryResultValueText l1],
                                                             [QueryResultValueInt4 i2, QueryResultValueText l2]] <- query "INSERT INTO t(i, l) \
                                                                                                                         \ SELECT i+3, l \
                                                                                                                         \ FROM t \
                                                                                                                         \ WHERE i is not NULL \
                                                                                                                         \ ORDER BY i RETURNING i, l" []

        assert (processed == 3) "Bad processed"
        assert (header1 == "i") "Bad header1"
        assert (header2 == "l") "Bad header2"
        assert (i0 == Just 4) "Bad i0"
        assert (l0 == Just "A") "Bad l0"
        assert (i1 == Just 5) "Bad i1"
        assert (l1 == Just "B") "Bad l1"
        assert (i2 == Just 6) "Bad i2"
        assert (l2 == Just "C") "Bad l2"

        return ()
$$
LANGUAGE plhaskell;
