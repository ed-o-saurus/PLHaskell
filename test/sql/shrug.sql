CREATE FUNCTION shrug() RETURNS text IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Text (Text)

    shrug :: PGm (Maybe Text)
    shrug = return (Just "¯\\_(ツ)_/¯")
$$
LANGUAGE plhaskell;
