CREATE FUNCTION msg_debug1() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, debug1)

    msg_debug1 :: PGm ()
    msg_debug1 = report debug1 "Test"
$$
LANGUAGE plhaskell;
