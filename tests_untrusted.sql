-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2023 Edward F. Behn, Jr.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

\set ON_ERROR_STOP on

DROP SCHEMA IF EXISTS plhaskellu_test CASCADE;

CREATE SCHEMA plhaskellu_test;

CREATE FUNCTION plhaskellu_test.echo(bytea) RETURNS bytea IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(text) RETURNS text IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(char) RETURNS char IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(bool) RETURNS bool IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(smallint) RETURNS smallint IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(int) RETURNS int IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(bigint) RETURNS bigint IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(real) RETURNS real IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(float) RETURNS float IMMUTABLE AS
$$
    echo :: a -> IO a
    echo = return
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.nan() RETURNS float IMMUTABLE AS
$$
    nan :: IO (Maybe Double)
    nan = return (Just (0.0 / 0.0))
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.poop() RETURNS char IMMUTABLE AS
$$
    poop :: IO (Maybe Char)
    poop = return (Just '\x0001F4A9')
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.shrug() RETURNS text IMMUTABLE AS
$$
    import Data.Text (Text)

    shrug :: IO (Maybe Text)
    shrug = return (Just "¯\\_(ツ)_/¯")
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.len(bytea) RETURNS int IMMUTABLE AS
$$
    import Data.ByteString (length, ByteString)
    import Data.Int (Int32)

    len :: Maybe ByteString -> IO (Maybe Int32)
    len Nothing = return Nothing
    len (Just s) = return $ Just $ fromIntegral $ Data.ByteString.length s
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.len(text) RETURNS int IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (length, Text)

    len :: Maybe Text -> IO (Maybe Int32)
    len Nothing = return Nothing
    len (Just s) = return $ Just $ fromIntegral $ Data.Text.length s
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.make_length_bytea(int) RETURNS bytea IMMUTABLE AS
$$
    import Data.ByteString (pack, ByteString)
    import Data.Int (Int32)
    import Data.Word (Word8)

    make_length :: Int32 -> [Word8]
    make_length n = take (fromIntegral n) (repeat 0)

    make_length_bytea :: Maybe Int32 -> IO (Maybe ByteString)
    make_length_bytea Nothing = return Nothing
    make_length_bytea (Just n) = return $ Just (pack (make_length n))
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.make_length_text(int) RETURNS text IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (pack, Text)

    make_length_text :: Maybe Int32 -> IO (Maybe Text)
    make_length_text Nothing = return Nothing
    make_length_text (Just n) = return $ Just $ pack (take (fromIntegral n) (repeat '_'))
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.inv(bool) RETURNS bool IMMUTABLE AS
$$
    inv :: Maybe Bool -> IO (Maybe Bool)
    inv Nothing = return Nothing
    inv (Just True) = return (Just False)
    inv (Just False) = return (Just True)
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.add(int, int) RETURNS int IMMUTABLE AS
$$
    import Data.Int (Int32)

    add :: Maybe Int32 -> Maybe Int32 -> IO (Maybe Int32)
    add (Just a) (Just b) = return (Just (a+b))
    add a Nothing = return a
    add Nothing _ = return Nothing
$$
LANGUAGE plhaskellu;

CREATE TYPE plhaskellu_test.alpha AS (a text, b int, c float);
CREATE TYPE plhaskellu_test.bravo AS (d plhaskellu_test.alpha, e int);
CREATE TYPE plhaskellu_test.charlie AS ();
CREATE TYPE plhaskellu_test.delta AS (f plhaskellu_test.bravo, g plhaskellu_test.charlie);

CREATE FUNCTION plhaskellu_test.alpha_test(int) RETURNS plhaskellu_test.alpha IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (unPGm, raiseError)

    p :: Double
    p = 12.3

    r :: Double
    r = 32.1

    alpha_test :: Maybe Int32 -> IO (Maybe (Maybe Text, Maybe Int32, Maybe Double))
    alpha_test (Just 1) = return (Just (Just "abc", Just 42, Nothing))
    alpha_test (Just 2) = return (Just (Just "cde", Nothing, Just p))
    alpha_test (Just 3) = return (Just (Nothing, Just 42, Just r))
    alpha_test (Just 4) = return Nothing

    alpha_test _ = unPGm $ raiseError "Invalid"
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.delta_test() RETURNS plhaskellu_test.delta IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    delta_test :: IO (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    delta_test = return (Just (Just (Just (Just "abc", Just 42, Just (42.3)), Just 0), Just ()))
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.echo(plhaskellu_test.delta) RETURNS plhaskellu_test.delta IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    echo :: Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()) -> IO (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    echo = return
$$
LANGUAGE plhaskellu;

CREATE TYPE plhaskellu_test.n_p AS (n int, p int);

CREATE FUNCTION plhaskellu_test.primes(int) RETURNS SETOF plhaskellu_test.n_p IMMUTABLE AS
$$
    import PGutils (unPGm, raiseError)
    import Data.Int (Int32)

    sieve :: [Int32] -> [Int32]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

    primes :: Maybe Int32 -> IO [Maybe (Maybe Int32, Maybe Int32)]
    primes Nothing = unPGm $ raiseError "Invalid Null"
    primes (Just n) = return (map Just (zip [Just i | i <- [1..n]] (map Just (sieve [2..]))))
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.query_create() RETURNS void VOLATILE AS
$$
    import PGutils (unPGm, query, QueryResults (UtilityResults))

    query_create :: IO ()
    query_create = do
        UtilityResults _processed <- unPGm $ query "CREATE TABLE plhaskellu_test.t(i int, l text)" []
        return ()
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.query_insert(int, text) RETURNS void VOLATILE AS
$$
    import PGutils (unPGm, query, QueryResults (InsertResults), QueryParam (QueryParamInt4, QueryParamText))
    import Data.Int (Int32)
    import Data.Text (Text)

    query_insert :: Maybe Int32 -> Maybe Text -> IO ()
    query_insert i l = do
        InsertResults _processed <- unPGm $ query "INSERT INTO plhaskellu_test.t(i, l) VALUES ($1, $2);" [QueryParamInt4 i, QueryParamText l]
        return ()
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.query_insert_returning() RETURNS void VOLATILE AS
$$
    import PGutils (unPGm, query, QueryResults (InsertReturningResults), QueryResultValue (QueryResultValueInt4, QueryResultValueText), raiseError)

    query_insert_returning :: IO ()
    query_insert_returning = do
        InsertReturningResults processed [header1, header2] [[QueryResultValueInt4 i0, QueryResultValueText l0], [QueryResultValueInt4 i1, QueryResultValueText l1], [QueryResultValueInt4 i2, QueryResultValueText l2]] <- unPGm $ query "INSERT INTO plhaskellu_test.t(i, l) SELECT i+3, l FROM plhaskellu_test.t WHERE i is not NULL ORDER BY i RETURNING i, l" []

        if processed == 3
        then return ()
        else unPGm $ raiseError "Bad processed"

        if header1 == "i"
        then return ()
        else unPGm $ raiseError "Bad header1"

        if header2 == "l"
        then return ()
        else unPGm $ raiseError "Bad header2"

        if i0 == Just 4
        then return ()
        else unPGm $ raiseError "Bad i0"

        if l0 == Just "A"
        then return ()
        else unPGm $ raiseError "Bad l0"

        if i1 == Just 5
        then return ()
        else unPGm $ raiseError "Bad i1"

        if l1 == Just "B"
        then return ()
        else unPGm $ raiseError "Bad l1"

        if i2 == Just 6
        then return ()
        else unPGm $ raiseError "Bad i2"

        if l2 == Just "C"
        then return ()
        else unPGm $ raiseError "Bad l2"

        return ()
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.query_select() RETURNS void IMMUTABLE AS
$$
    import PGutils (unPGm, query, QueryResults (SelectResults), QueryResultValue (QueryResultValueInt4, QueryResultValueText), raiseError)

    query_select :: IO ()
    query_select = do
        SelectResults processed [header1, header2] [[QueryResultValueInt4 i0, QueryResultValueText l0], [QueryResultValueInt4 i1, QueryResultValueText l1], [QueryResultValueInt4 i2, QueryResultValueText l2], [QueryResultValueInt4 i3, QueryResultValueText l3], [QueryResultValueInt4 i4, QueryResultValueText l4], [QueryResultValueInt4 i5, QueryResultValueText l5], [QueryResultValueInt4 i6, QueryResultValueText l6]] <- unPGm $ query "SELECT i, l FROM plhaskellu_test.t ORDER BY i" []

        if processed == 7
        then return ()
        else unPGm $ raiseError "Bad processed"

        if header1 == "i"
        then return ()
        else unPGm $ raiseError "Bad header1"

        if header2 == "l"
        then return ()
        else unPGm $ raiseError "Bad header2"

        if i0 == Just 1
        then return ()
        else unPGm $ raiseError "Bad i0"

        if l0 == Just "A"
        then return ()
        else unPGm $ raiseError "Bad l0"

        if i1 == Just 2
        then return ()
        else unPGm $ raiseError "Bad i1"

        if l1 == Just "B"
        then return ()
        else unPGm $ raiseError "Bad l1"

        if i2 == Just 3
        then return ()
        else unPGm $ raiseError "Bad i2"

        if l2 == Just "C"
        then return ()
        else unPGm $ raiseError "Bad l2"

        if i3 == Just 4
        then return ()
        else unPGm $ raiseError "Bad i3"

        if l3 == Just "A"
        then return ()
        else unPGm $ raiseError "Bad l3"

        if i4 == Just 5
        then return ()
        else unPGm $ raiseError "Bad i4"

        if l4 == Just "B"
        then return ()
        else unPGm $ raiseError "Bad l4"

        if i5 == Just 6
        then return ()
        else unPGm $ raiseError "Bad i5"

        if l5 == Just "C"
        then return ()
        else unPGm $ raiseError "Bad l5"

        if i6 == Nothing
        then return ()
        else unPGm $ raiseError "Bad i6"

        if l6 == Nothing
        then return ()
        else unPGm $ raiseError "Bad l6"

        return ()
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.query_delete() RETURNS void VOLATILE AS
$$
    import PGutils (unPGm, query, QueryResults (DeleteResults), raiseError)

    query_delete :: IO ()
    query_delete = do
        DeleteResults processed1 <- unPGm $ query "DELETE FROM plhaskellu_test.t" []
        DeleteResults processed2 <- unPGm $ query "DELETE FROM plhaskellu_test.t" []

        if processed1 == 7
        then return ()
        else unPGm $ raiseError "Bad processed1"

        if processed2 == 0
        then return ()
        else unPGm $ raiseError "Bad processed2"

        return ()
$$
LANGUAGE plhaskellu;

CREATE FUNCTION plhaskellu_test.query_drop() RETURNS void VOLATILE AS
$$
    import PGutils (unPGm, query, QueryResults (UtilityResults))

    query_drop :: IO ()
    query_drop = do
        UtilityResults _processed <- unPGm $ query "DROP TABLE plhaskellu_test.t" []
        return ()
$$
LANGUAGE plhaskellu;

SELECT plhaskellu_test.query_create();

SELECT plhaskellu_test.query_insert(NULL::int, NULL::text);
SELECT plhaskellu_test.query_insert(1, 'A');
SELECT plhaskellu_test.query_insert(2, 'B');
SELECT plhaskellu_test.query_insert(3, 'C');

SELECT plhaskellu_test.query_insert_returning();

SELECT plhaskellu_test.query_select();

SELECT plhaskellu_test.query_delete();

SELECT plhaskellu_test.query_drop();

CREATE FUNCTION plhaskellu_test.query_composite() RETURNS void IMMUTABLE AS
$$
    import PGutils (unPGm, query, QueryResults (SelectResults), QueryResultValue (QueryResultValueComposite, QueryResultValueInt4, QueryResultValueText, QueryResultValueFloat8), raiseError)

    query_composite :: IO ()
    query_composite = do
        SelectResults processed [header] [row0, row1, row2, row3] <- unPGm $ query "SELECT d FROM plhaskellu_test.deltas ORDER BY i" []

        if processed == 4
        then return ()
        else unPGm $ raiseError "Bad processed"

        if header == "d"
        then return ()
        else unPGm $ raiseError "Bad header"

        let [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueText (Just x0), QueryResultValueInt4 (Just x1), QueryResultValueFloat8 (Just x2)]), QueryResultValueInt4 (Just x3)]), QueryResultValueComposite (Just [])])] = row0

        if x0 == "Hello"
        then return ()
        else unPGm $ raiseError "Bad x0"

        if x1 == 12
        then return ()
        else unPGm $ raiseError "Bad x1"

        if x2 == 3.4
        then return ()
        else unPGm $ raiseError "Bad x2"

        if x3 == 76
        then return ()
        else unPGm $ raiseError "Bad x3"

        let [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueText (Just x4), QueryResultValueInt4 (Just x5), QueryResultValueFloat8 (Just x6)]), QueryResultValueInt4 (Just x7)]), QueryResultValueComposite Nothing])] = row1

        if x4 == "world"
        then return ()
        else unPGm $ raiseError "Bad x4"

        if x5 == 42
        then return ()
        else unPGm $ raiseError "Bad x5"

        if x6 == 1.0
        then return ()
        else unPGm $ raiseError "Bad x6"

        if x7 == -12
        then return ()
        else unPGm $ raiseError "Bad x7"

        let [QueryResultValueComposite (Just [QueryResultValueComposite Nothing, QueryResultValueComposite (Just [])])] = row2
        let [QueryResultValueComposite Nothing] = row3

        return ()
$$
LANGUAGE plhaskellu;

CREATE TABLE plhaskellu_test.deltas(i int, d plhaskellu_test.delta);
INSERT INTO plhaskellu_test.deltas(i, d) VALUES (1, ((('Hello', 12, 3.4)::plhaskellu_test.alpha, 76)::plhaskellu_test.bravo, '()'::plhaskellu_test.charlie)::plhaskellu_test.delta);
INSERT INTO plhaskellu_test.deltas(i, d) VALUES (2, ((('world', 42, 1.0)::plhaskellu_test.alpha, -12)::plhaskellu_test.bravo, NULL::plhaskellu_test.charlie)::plhaskellu_test.delta);
INSERT INTO plhaskellu_test.deltas(i, d) VALUES (3, (NULL::plhaskellu_test.bravo, '()'::plhaskellu_test.charlie)::plhaskellu_test.delta);
INSERT INTO plhaskellu_test.deltas(i, d) VALUES (4, NULL::plhaskellu_test.delta);

SELECT plhaskellu_test.query_composite();

CREATE TABLE plhaskellu_test.primes(n int, p int);

INSERT INTO plhaskellu_test.primes(n, p)
SELECT n, p
FROM plhaskellu_test.primes(10);

CREATE TABLE plhaskellu_test.inline_test(i int, i_sq int);
DO LANGUAGE plhaskellu $$
    import PGutils (unPGm, query, QueryParam(QueryParamInt4))

    _' :: IO ()
    _' = unPGm $ do
        _ <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 0), QueryParamInt4 (Just  0)]
        _ <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 1), QueryParamInt4 (Just  1)]
        _ <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 2), QueryParamInt4 (Just  4)]
        _ <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 3), QueryParamInt4 (Just  9)]
        _ <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 4), QueryParamInt4 (Just 16)]
        _ <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 5), QueryParamInt4 (Just 25)]
        return ()
$$;

DO $$
DECLARE
    r RECORD;
BEGIN
    IF plhaskellu_test.echo('\xabcdef'::bytea) <> '\xabcdef'::bytea THEN
        raise EXCEPTION 'echo bytea failed';
    END IF;

    IF plhaskellu_test.echo(NULL::bytea) is not NULL THEN
        raise EXCEPTION 'echo NULL bytea failed';
    END IF;

    IF plhaskellu_test.echo('\xabcdef'::text) <> '\xabcdef'::text THEN
        raise EXCEPTION 'echo text failed';
    END IF;

    IF plhaskellu_test.echo(NULL::text) is not NULL THEN
        raise EXCEPTION 'echo NULL text failed';
    END IF;

    IF plhaskellu_test.echo('A'::char) <> 'A'::char THEN
        raise EXCEPTION 'echo char failed';
    END IF;

    IF plhaskellu_test.echo(NULL::char) is not NULL THEN
        raise EXCEPTION 'echo NULL char failed';
    END IF;

    IF plhaskellu_test.echo(true) <> true THEN
        raise EXCEPTION 'echo bool failed';
    END IF;

    IF plhaskellu_test.echo(false) <> false THEN
        raise EXCEPTION 'echo bool failed';
    END IF;

    IF plhaskellu_test.echo(NULL::bool) is not NULL THEN
        raise EXCEPTION 'echo NULL bool failed';
    END IF;

    IF plhaskellu_test.echo(20488) <> 20488 THEN
        raise EXCEPTION 'echo smallint failed';
    END IF;

    IF plhaskellu_test.echo(-29268) <> -29268 THEN
        raise EXCEPTION 'echo smallint failed';
    END IF;

    IF plhaskellu_test.echo(NULL::smallint) is not NULL THEN
        raise EXCEPTION 'echo NULL smallint failed';
    END IF;

    IF plhaskellu_test.echo(1372801355::int) <> 1372801355 THEN
        raise EXCEPTION 'echo int failed';
    END IF;

    IF plhaskellu_test.echo(-1042672097::int) <> -1042672097 THEN
        raise EXCEPTION 'echo int failed';
    END IF;

    IF plhaskellu_test.echo(NULL::int) is not NULL THEN
        raise EXCEPTION 'echo NULL int failed';
    END IF;

    IF plhaskellu_test.echo(2263727920641201613::bigint) <> 2263727920641201613 THEN
        raise EXCEPTION 'echo bigint failed';
    END IF;

    IF plhaskellu_test.echo(-591947113936367256::bigint) <> -591947113936367256 THEN
        raise EXCEPTION 'echo bigint failed';
    END IF;

    IF plhaskellu_test.echo(NULL::bigint) is not NULL THEN
        raise EXCEPTION 'echo NULL bigint failed';
    END IF;

    IF plhaskellu_test.echo(42.0) <> 42.0 THEN
        raise EXCEPTION 'echo real failed';
    END IF;

    IF plhaskellu_test.echo(NULL::real) is not NULL THEN
        raise EXCEPTION 'echo NULL real failed';
    END IF;

    IF plhaskellu_test.echo(42.0::float) <> 42.0 THEN
        raise EXCEPTION 'echo float failed';
    END IF;

    IF plhaskellu_test.echo(NULL::float) is not NULL THEN
        raise EXCEPTION 'echo NULL float failed';
    END IF;

    IF plhaskellu_test.echo((((('abc', 42, 42.3), 0), '()'::plhaskellu_test.charlie))::plhaskellu_test.delta) <> (((('abc', 42, 42.3), 0), '()'::plhaskellu_test.charlie))::plhaskellu_test.delta THEN
        raise EXCEPTION 'echo delta failed';
    END IF;

    IF plhaskellu_test.echo(NULL::plhaskellu_test.delta) is not NULL THEN
        raise EXCEPTION 'echo NULL delta failed';
    END IF;

    IF plhaskellu_test.nan() != 'nan'::float THEN
        raise EXCEPTION 'NaN failed';
    END IF;

    IF plhaskellu_test.poop() <> '💩' THEN
        raise EXCEPTION 'Poop failed';
    END IF;

    IF plhaskellu_test.shrug() <> '¯\_(ツ)_/¯' THEN
        raise EXCEPTION 'Shrug failed';
    END IF;

    IF plhaskellu_test.len('\xabcdef'::bytea) <> 3 THEN
        raise EXCEPTION 'len failed';
    END IF;

    IF plhaskellu_test.len('          ') <> 10 THEN
        raise EXCEPTION 'len failed';
    END IF;

    IF plhaskellu_test.make_length_bytea(10) <> '\x00000000000000000000'::bytea THEN
        raise EXCEPTION 'make_length_bytea failed';
    END IF;

    IF plhaskellu_test.make_length_bytea(NULL) is not NULL THEN
        raise EXCEPTION 'make_length_bytea failed';
    END IF;

    IF plhaskellu_test.make_length_text(10) <> '__________' THEN
        raise EXCEPTION 'make_length_text failed';
    END IF;

    IF plhaskellu_test.make_length_text(NULL) is not NULL THEN
        raise EXCEPTION 'make_length_text failed';
    END IF;

    IF plhaskellu_test.inv(true) <> false THEN
        raise EXCEPTION 'inv failed';
    END IF;

    IF plhaskellu_test.inv(false) <> true THEN
        raise EXCEPTION 'inv failed';
    END IF;

    IF plhaskellu_test.inv(NULL) is not NULL THEN
        raise EXCEPTION 'inv failed';
    END IF;

    IF plhaskellu_test.add(3, 4) <> 7 THEN
        raise EXCEPTION 'add failed';
    END IF;

    IF plhaskellu_test.add(3, NULL) <> 3 THEN
        raise EXCEPTION 'add failed';
    END IF;

    IF plhaskellu_test.add(NULL, 4) is not NULL THEN
        raise EXCEPTION 'add failed';
    END IF;

    IF plhaskellu_test.add(NULL, NULL) is not NULL THEN
        raise EXCEPTION 'add failed';
    END IF;

    IF plhaskellu_test.alpha_test(1) <> ('abc', 42, NULL)::plhaskellu_test.alpha THEN
        raise EXCEPTION 'alpha_test failed';
    END IF;

    IF plhaskellu_test.alpha_test(2) <> ('cde', NULL, 12.3)::plhaskellu_test.alpha THEN
        raise EXCEPTION 'alpha_test failed';
    END IF;

    IF plhaskellu_test.alpha_test(3) <> (NULL, 42, 32.1)::plhaskellu_test.alpha THEN
        raise EXCEPTION 'alpha_test failed';
    END IF;

    IF plhaskellu_test.alpha_test(4) is not NULL THEN
        raise EXCEPTION 'alpha_test failed';
    END IF;

    IF plhaskellu_test.delta_test() <> (((('abc', 42, 42.3), 0), '()'::plhaskellu_test.charlie))::plhaskellu_test.delta THEN
        raise EXCEPTION 'delta_test failed';
    END IF;

    FOR r IN
        SELECT count(*) FROM plhaskellu_test.primes
    LOOP
        IF r.count <> 10 THEN
            raise EXCEPTION 'primes failed';
        END IF;
    END LOOP;

    FOR r IN
        SELECT * FROM plhaskellu_test.primes
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

    FOR r IN
        SELECT * FROM plhaskell_test.inline_test
    LOOP
        IF r.i = 0 AND r.i_sq <>  0 THEN
            raise EXCEPTION 'inline failed';
        END IF;

        IF r.i = 1 AND r.i_sq <>  1 THEN
            raise EXCEPTION 'inline failed';
        END IF;

        IF r.i = 2 AND r.i_sq <>  4 THEN
            raise EXCEPTION 'inline failed';
        END IF;

        IF r.i = 3 AND r.i_sq <>  9 THEN
            raise EXCEPTION 'inline failed';
        END IF;

        IF r.i = 4 AND r.i_sq <> 16 THEN
            raise EXCEPTION 'inline failed';
        END IF;

        IF r.i = 5 AND r.i_sq <> 25 THEN
            raise EXCEPTION 'inline failed';
        END IF;
    END LOOP;

    DROP SCHEMA plhaskellu_test CASCADE;

    raise NOTICE 'All tests passed';
END
$$;
