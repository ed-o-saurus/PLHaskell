CREATE FUNCTION msg_fatal() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, fatal)

    msg_fatal :: PGm ()
    msg_fatal = report fatal "Test"
$$
LANGUAGE plhaskell;
