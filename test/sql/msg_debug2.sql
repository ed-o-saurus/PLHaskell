CREATE FUNCTION msg_debug2() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, debug2)

    msg_debug2 :: PGm ()
    msg_debug2 = report debug2 "Test"
$$
LANGUAGE plhaskell;
