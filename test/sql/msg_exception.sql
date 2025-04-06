CREATE FUNCTION msg_exception() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, exception)

    msg_exception :: PGm ()
    msg_exception = report exception "Test"
$$
LANGUAGE plhaskell;
