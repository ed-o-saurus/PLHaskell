CREATE FUNCTION query_pass_composite() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, raiseError, query, QueryResults (SelectResults), QueryParam (QueryParamComposite, QueryParamText, QueryParamInt4, QueryParamFloat8), QueryResultValue (QueryResultValueComposite, QueryResultValueText, QueryResultValueInt4, QueryResultValueFloat8))
    import Data.Text (Text, pack)

    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_pass_composite ::  PGm ()
    query_pass_composite = do
        SelectResults _processed _header [row0] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") (Just [QueryParamComposite (Just "plhaskell_test", "bravo") (Just [QueryParamComposite (Just "plhaskell_test", "alpha") (Just [QueryParamText (Just "Hello"), QueryParamInt4 (Just 12), QueryParamFloat8 (Just 3.4)]), QueryParamInt4 (Just 76)]), QueryParamComposite (Just "plhaskell_test", "charlie") (Just [])])]
        let [QueryResultValueComposite (delta_schema0, delta_type0) (Just [QueryResultValueComposite (bravo_schema0, bravo_type0) (Just [QueryResultValueComposite (alpha_schema0, alpha_type0) (Just [QueryResultValueText (Just x0), QueryResultValueInt4 (Just x1), QueryResultValueFloat8 (Just x2)]), QueryResultValueInt4 (Just x3)]), QueryResultValueComposite (charlie_schema0, charlie_type0) (Just [])])] = row0

        assert (delta_schema0 == "plhaskell_test") "Bad delta_schema0"
        assert (delta_type0 == "delta") "Bad delta_type0"
        assert (bravo_schema0 == "plhaskell_test") "Bad bravo_schema0"
        assert (bravo_type0 == "bravo") "Bad bravo_type0"
        assert (alpha_schema0 == "plhaskell_test") "Bad alpha_schema0"
        assert (alpha_type0 == "alpha") "Bad alpha_type0"
        assert (charlie_schema0 == "plhaskell_test") "Bad charlie_schema0"
        assert (charlie_type0 == "charlie") "Bad charlie_type0"
        assert (x0 == "Hello") "Bad x0"
        assert (x1 == 12) "Bad x1"
        assert (x2 == 3.4) "Bad x2"
        assert (x3 == 76) "Bad x3"

        SelectResults _processed _header [row1] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") (Just [QueryParamComposite (Just "plhaskell_test", "bravo") (Just [QueryParamComposite (Just "plhaskell_test", "alpha") (Just [QueryParamText (Just "world"), QueryParamInt4 (Just 42), QueryParamFloat8 (Just 1.0)]), QueryParamInt4 (Just (-12))]), QueryParamComposite (Just "plhaskell_test", "charlie") Nothing])]
        let [QueryResultValueComposite (delta_schema1, delta_type1) (Just [QueryResultValueComposite (bravo_schema1, bravo_type1) (Just [QueryResultValueComposite (alpha_schema1, alpha_type1) (Just [QueryResultValueText (Just x4), QueryResultValueInt4 (Just x5), QueryResultValueFloat8 (Just x6)]), QueryResultValueInt4 (Just x7)]), QueryResultValueComposite (charlie_schema1, charlie_type1) Nothing])] = row1

        assert (delta_schema1 == "plhaskell_test") "Bad delta_schema1"
        assert (delta_type1 == "delta") "Bad delta_type1"
        assert (bravo_schema1 == "plhaskell_test") "Bad bravo_schema1"
        assert (bravo_type1 == "bravo") "Bad bravo_type1"
        assert (alpha_schema1 == "plhaskell_test") "Bad alpha_schema1"
        assert (alpha_type1 == "alpha") "Bad alpha_type1"
        assert (charlie_schema1 == "plhaskell_test") "Bad charlie_schema1"
        assert (charlie_type1 == "charlie") "Bad charlie_type1"
        assert (x4 == "world") "Bad x4"
        assert (x5 == 42) "Bad x5"
        assert (x6 == 1.0) "Bad x6"
        assert (x7 == -12) "Bad x7"

        SelectResults _processed _header [row2] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") (Just [QueryParamComposite (Just "plhaskell_test", "bravo") Nothing, QueryParamComposite (Just "plhaskell_test", "charlie") (Just [])])]
        let [QueryResultValueComposite (delta_schema2, delta_type2) (Just [QueryResultValueComposite (bravo_schema2, bravo_type2) Nothing, QueryResultValueComposite (charlie_schema2, charlie_type2) (Just [])])] = row2

        assert (delta_schema2 == "plhaskell_test") "Bad delta_schema2"
        assert (delta_type2 == "delta") "Bad delta_type2"
        assert (bravo_schema2 == "plhaskell_test") "Bad bravo_schema2"
        assert (bravo_type2 == "bravo") "Bad bravo_type2"
        assert (charlie_schema2 == "plhaskell_test") "Bad charlie_schema2"
        assert (charlie_type2 == "charlie") "Bad charlie_type2"

        SelectResults _processed _header [row3] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") Nothing]
        let [QueryResultValueComposite (delta_schema3, delta_type3) Nothing] = row3

        assert (delta_schema3 == "plhaskell_test") "Bad delta_schema3"
        assert (delta_type3 == "delta") "Bad delta_type3"

        return ()
$$
LANGUAGE plhaskell;
