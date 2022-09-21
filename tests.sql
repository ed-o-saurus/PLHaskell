\set ON_ERROR_STOP=ON

DROP SCHEMA IF EXISTS plhaskell_test CASCADE;

CREATE SCHEMA plhaskell_test;

CREATE FUNCTION plhaskell_test.echo(bytea) RETURNS bytea AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(text) RETURNS text AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(char) RETURNS char AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(bool) RETURNS bool AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(smallint) RETURNS smallint AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(int) RETURNS int AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(bigint) RETURNS bigint AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(real) RETURNS real AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(float) RETURNS float AS
$$
    import PGutils (PGm)
    
    echo :: a -> PGm a
    echo a = return a
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.nan() RETURNS float AS
$$
    import PGutils (PGm)
    
    nan :: PGm (Maybe Double)
    nan = return (Just (0.0 / 0.0))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.poop() RETURNS char AS
$$
    import PGutils (PGm)
    
    poop :: PGm (Maybe Char)
    poop = return (Just '\x0001F4A9')
$$
LANGUAGE plhaskell;


CREATE FUNCTION plhaskell_test.shrug() RETURNS text AS
$$
    import PGutils (PGm)
    
    shrug :: PGm (Maybe String)
    shrug = return (Just "¯\\_(ツ)_/¯")
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.len(bytea) RETURNS int AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    
    len :: Maybe [a] -> PGm (Maybe Int32)
    len Nothing = return Nothing
    len (Just s) = return $ Just $ fromIntegral $ length s
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.len(text) RETURNS int AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    
    len :: Maybe [a] -> PGm (Maybe Int32)
    len Nothing = return Nothing
    len (Just s) = return $ Just $ fromIntegral $ length s
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.make_length_bytea(int) RETURNS bytea AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    import Data.Word (Word8)
    
    make_length_bytea :: Maybe Int32 -> PGm (Maybe [Word8])
    make_length_bytea Nothing = return Nothing
    make_length_bytea (Just n) = return $ Just (take (fromIntegral n) (repeat 0))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.make_length_text(int) RETURNS text AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    
    make_length_text :: Maybe Int32 -> PGm (Maybe String)
    make_length_text Nothing = return Nothing
    make_length_text (Just n) = return $ Just (take (fromIntegral n) (repeat '_'))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.inv(bool) RETURNS bool AS
$$
    import PGutils (PGm)
    
    inv :: Maybe Bool -> PGm (Maybe Bool)
    inv Nothing = return Nothing
    inv (Just True) = return (Just False)
    inv (Just False) = return (Just True)
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.add(int, int) RETURNS int AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    
    add :: Maybe Int32 -> Maybe Int32 -> PGm (Maybe Int32)
    add (Just a) (Just b) = return (Just (a+b))
    add a Nothing = return a
    add Nothing _ = return Nothing
$$
LANGUAGE plhaskell;

CREATE TYPE plhaskell_test.alpha AS (a text, b int, c float);
CREATE TYPE plhaskell_test.bravo AS (d plhaskell_test.alpha, e int);
CREATE TYPE plhaskell_test.charlie AS ();
CREATE TYPE plhaskell_test.delta AS (f plhaskell_test.bravo, g plhaskell_test.charlie);

CREATE FUNCTION plhaskell_test.alpha_test(int) RETURNS plhaskell_test.alpha AS
$$
    import Data.Int (Int32)
    
    import PGutils (PGm, raiseError)
    
    p :: Double
    p = 12.3
    
    r :: Double
    r = 32.1
    
    alpha_test :: Maybe Int32 -> PGm (Maybe (Maybe String, Maybe Int32, Maybe Double))
    alpha_test (Just 1) = return (Just (Just "abc", Just 42, Nothing))
    alpha_test (Just 2) = return (Just (Just "cde", Nothing, Just p))
    alpha_test (Just 3) = return (Just (Nothing, Just 42, Just r))
    alpha_test (Just 4) = return Nothing
    
    alpha_test _ = raiseError "Invalid"
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.delta_test() RETURNS plhaskell_test.delta AS
$$
    import Data.Int (Int32)
    
    import PGutils (PGm)
    
    delta_test :: PGm (Maybe (Maybe (Maybe (Maybe String, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    delta_test = return (Just (Just (Just (Just "abc", Just 42, Just (42.3)), Just 0), Just ()))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(plhaskell_test.delta) RETURNS plhaskell_test.delta AS
$$
    import Data.Int (Int32)
    
    import PGutils (PGm)
    
    echo :: Maybe (Maybe (Maybe (Maybe String, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()) -> PGm (Maybe (Maybe (Maybe (Maybe String, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    echo = return
$$
LANGUAGE plhaskell;

CREATE TYPE plhaskell_test.n_p AS (n int, p int);

CREATE FUNCTION plhaskell_test.primes(int) RETURNS SETOF plhaskell_test.n_p AS
$$
    import PGutils (PGm, raiseError)
    import Data.Int (Int32)
    
    sieve :: [Int32] -> [Int32]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []
    
    primes :: Maybe Int32 -> PGm [Maybe (Maybe Int32, Maybe Int32)]
    primes Nothing = raiseError "Invalid Null"
    primes (Just n) = return (map Just (zip [Just i | i <- [1..n]] (map Just (sieve [2..]))))
$$
LANGUAGE plhaskell;

CREATE TABLE plhaskell_test.primes(n int, p int);

INSERT INTO plhaskell_test.primes(n, p)
SELECT n, p
FROM plhaskell_test.primes(10);

DO $$
DECLARE
    r RECORD;
BEGIN
    IF plhaskell_test.echo('\xabcdef'::bytea) <> '\xabcdef'::bytea THEN
        raise EXCEPTION 'echo bytea failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::bytea) is not NULL THEN
        raise EXCEPTION 'echo NULL bytea failed';
    END IF;
    
    IF plhaskell_test.echo('\xabcdef'::text) <> '\xabcdef'::text THEN
        raise EXCEPTION 'echo text failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::text) is not NULL THEN
        raise EXCEPTION 'echo NULL text failed';
    END IF;
    
    IF plhaskell_test.echo('A'::char) <> 'A'::char THEN
        raise EXCEPTION 'echo char failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::char) is not NULL THEN
        raise EXCEPTION 'echo NULL char failed';
    END IF;
    
    IF plhaskell_test.echo(true) <> true THEN
        raise EXCEPTION 'echo bool failed';
    END IF;
    
    IF plhaskell_test.echo(false) <> false THEN
        raise EXCEPTION 'echo bool failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::bool) is not NULL THEN
        raise EXCEPTION 'echo NULL bool failed';
    END IF;
    
    IF plhaskell_test.echo(20488) <> 20488 THEN
        raise EXCEPTION 'echo smallint failed';
    END IF;
    
    IF plhaskell_test.echo(-29268) <> -29268 THEN
        raise EXCEPTION 'echo smallint failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::smallint) is not NULL THEN
        raise EXCEPTION 'echo NULL smallint failed';
    END IF;
    
    IF plhaskell_test.echo(1372801355::int) <> 1372801355 THEN
        raise EXCEPTION 'echo int failed';
    END IF;
    
    IF plhaskell_test.echo(-1042672097::int) <> -1042672097 THEN
        raise EXCEPTION 'echo int failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::int) is not NULL THEN
        raise EXCEPTION 'echo NULL int failed';
    END IF;
    
    IF plhaskell_test.echo(2263727920641201613::bigint) <> 2263727920641201613 THEN
        raise EXCEPTION 'echo bigint failed';
    END IF;
    
    IF plhaskell_test.echo(-591947113936367256::bigint) <> -591947113936367256 THEN
        raise EXCEPTION 'echo bigint failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::bigint) is not NULL THEN
        raise EXCEPTION 'echo NULL bigint failed';
    END IF;
    
    IF plhaskell_test.echo(42.0) <> 42.0 THEN
        raise EXCEPTION 'echo real failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::real) is not NULL THEN
        raise EXCEPTION 'echo NULL real failed';
    END IF;
    
    IF plhaskell_test.echo(42.0::float) <> 42.0 THEN
        raise EXCEPTION 'echo float failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::float) is not NULL THEN
        raise EXCEPTION 'echo NULL float failed';
    END IF;
    
    IF plhaskell_test.echo((((('abc', 42, 42.3), 0), '()'::plhaskell_test.charlie))::plhaskell_test.delta) <> (((('abc', 42, 42.3), 0), '()'::plhaskell_test.charlie))::plhaskell_test.delta THEN
        raise EXCEPTION 'echo delta failed';
    END IF;
    
    IF plhaskell_test.echo(NULL::plhaskell_test.delta) is not NULL THEN
        raise EXCEPTION 'echo NULL delta failed';
    END IF;
    
    IF plhaskell_test.nan() != 'nan'::float THEN
        raise EXCEPTION 'NaN failed';
    END IF;
    
    IF plhaskell_test.poop() <> '💩' THEN
        raise EXCEPTION 'Poop failed';
    END IF;
    
    IF plhaskell_test.shrug() <> '¯\_(ツ)_/¯' THEN
        raise EXCEPTION 'Shrug failed';
    END IF;
    
    IF plhaskell_test.len('\xabcdef'::bytea) <> 3 THEN
        raise EXCEPTION 'len failed';
    END IF;

    IF plhaskell_test.len('          ') <> 10 THEN
        raise EXCEPTION 'len failed';
    END IF;
    
    IF plhaskell_test.make_length_bytea(10) <> '\x00000000000000000000'::bytea THEN
        raise EXCEPTION 'make_length_bytea failed';
    END IF;
    
    IF plhaskell_test.make_length_bytea(NULL) is not NULL THEN
        raise EXCEPTION 'make_length_bytea failed';
    END IF;
    
    IF plhaskell_test.make_length_text(10) <> '__________' THEN
        raise EXCEPTION 'make_length_text failed';
    END IF;
    
    IF plhaskell_test.make_length_text(NULL) is not NULL THEN
        raise EXCEPTION 'make_length_text failed';
    END IF;
    
    IF plhaskell_test.inv(true) <> false THEN
        raise EXCEPTION 'inv failed';
    END IF;
    
    IF plhaskell_test.inv(false) <> true THEN
        raise EXCEPTION 'inv failed';
    END IF;
    
    IF plhaskell_test.inv(NULL) is not NULL THEN
        raise EXCEPTION 'inv failed';
    END IF;
    
    IF plhaskell_test.add(3, 4) <> 7 THEN
        raise EXCEPTION 'add failed';
    END IF;
    
    IF plhaskell_test.add(3, NULL) <> 3 THEN
        raise EXCEPTION 'add failed';
    END IF;
    
    IF plhaskell_test.add(NULL, 4) is not NULL THEN
        raise EXCEPTION 'add failed';
    END IF;
    
    IF plhaskell_test.add(NULL, NULL) is not NULL THEN
        raise EXCEPTION 'add failed';
    END IF;
    
    IF plhaskell_test.alpha_test(1) <> ('abc', 42, NULL)::plhaskell_test.alpha THEN 
        raise EXCEPTION 'alpha_test failed';
    END IF;
    
    IF plhaskell_test.alpha_test(2) <> ('cde', NULL, 12.3)::plhaskell_test.alpha THEN 
        raise EXCEPTION 'alpha_test failed';
    END IF;
    
    IF plhaskell_test.alpha_test(3) <> (NULL, 42, 32.1)::plhaskell_test.alpha THEN 
        raise EXCEPTION 'alpha_test failed';
    END IF;
    
    IF plhaskell_test.alpha_test(4) is not NULL THEN 
        raise EXCEPTION 'alpha_test failed';
    END IF;
    
    IF plhaskell_test.delta_test() <> (((('abc', 42, 42.3), 0), '()'::plhaskell_test.charlie))::plhaskell_test.delta THEN
        raise EXCEPTION 'delta_test failed';
    END IF;
    
    FOR r IN
        SELECT count(*) FROM plhaskell_test.primes
    LOOP
        IF r.count <> 10 THEN
            raise EXCEPTION 'primes failed';
        END IF;
    END LOOP;
    
    FOR r IN
        SELECT * FROM plhaskell_test.primes
    LOOP
        IF r.n < 1 OR r.n > 10 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 1 AND r.p <> 2 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 2 AND r.p <> 3 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 3 AND r.p <> 5 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 4 AND r.p <> 7 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 5 AND r.p <> 11 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 6 AND r.p <> 13 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 7 AND r.p <> 17 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 8 AND r.p <> 19 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 9 AND r.p <> 23 THEN
            raise EXCEPTION 'primes failed';
        END IF;
        
        IF r.n = 10 AND r.p <> 29 THEN
            raise EXCEPTION 'primes failed';
        END IF;
    END LOOP;
    
    DROP SCHEMA plhaskell_test CASCADE;
    
    raise NOTICE 'All tests passed';
END
$$;

