CREATE FUNCTION msg_debug3() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, debug3)

    msg_debug3 :: PGm ()
    msg_debug3 = report debug3 "Test"
$$
LANGUAGE plhaskell;
