CREATE FUNCTION msg_debug4() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, debug4)

    msg_debug4 :: PGm ()
    msg_debug4 = report debug4 "Test"
$$
LANGUAGE plhaskell;
