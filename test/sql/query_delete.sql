CREATE FUNCTION query_delete() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (DeleteResults), raiseError)
    import Data.Text (Text)

    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_delete :: PGm ()
    query_delete = do
        DeleteResults processed1 <- query "DELETE FROM t" []
        DeleteResults processed2 <- query "DELETE FROM t" []

        assert (processed1 == 7) "Bad processed1"
        assert (processed2 == 0) "Bad processed2"

        return ()
$$
LANGUAGE plhaskell;
