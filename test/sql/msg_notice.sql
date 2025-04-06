CREATE FUNCTION msg_notice() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, notice)

    msg_notice :: PGm ()
    msg_notice = report notice "Test"
$$
LANGUAGE plhaskell;
