CREATE FUNCTION alpha_func(int) RETURNS alpha IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (PGm, raiseError)

    p :: Double
    p = 12.3

    r :: Double
    r = 32.1

    alpha_func :: Maybe Int32 -> PGm (Maybe (Maybe Text, Maybe Int32, Maybe Double))
    alpha_func (Just 1) = return (Just (Just "abc", Just 42, Nothing))
    alpha_func (Just 2) = return (Just (Just "cde", Nothing, Just p))
    alpha_func (Just 3) = return (Just (Nothing, Just 42, Just r))
    alpha_func (Just 4) = return Nothing

    alpha_func _ = raiseError "Invalid"
$$
LANGUAGE plhaskell;
