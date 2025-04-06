CREATE FUNCTION msg_log() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, log)

    msg_log :: PGm ()
    msg_log = report log "Test"
$$
LANGUAGE plhaskell;
