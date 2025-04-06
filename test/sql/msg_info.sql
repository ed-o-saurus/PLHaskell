CREATE FUNCTION msg_info() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, info)

    msg_info :: PGm ()
    msg_info = report info "Test"
$$
LANGUAGE plhaskell;
