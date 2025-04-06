CREATE FUNCTION msg_debug5() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, debug1)

    msg_debug5 :: PGm ()
    msg_debug5 = report debug1 "Test"
$$
LANGUAGE plhaskell;
