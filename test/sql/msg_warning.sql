CREATE FUNCTION msg_warning() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, report, warning)

    msg_warning :: PGm ()
    msg_warning = report warning "Test"
$$
LANGUAGE plhaskell;
