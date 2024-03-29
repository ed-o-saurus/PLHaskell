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

DROP SCHEMA IF EXISTS plhaskell_test CASCADE;

CREATE SCHEMA plhaskell_test;

CREATE FUNCTION plhaskell_test.echo(bytea) RETURNS bytea IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(text) RETURNS text IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(char) RETURNS char IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(bool) RETURNS bool IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(smallint) RETURNS smallint IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(int) RETURNS int IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(bigint) RETURNS bigint IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(real) RETURNS real IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(float) RETURNS float IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.nan() RETURNS float IMMUTABLE AS
$$
    import PGutils (PGm)

    nan :: PGm (Maybe Double)
    nan = return (Just (0.0 / 0.0))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.poop() RETURNS char IMMUTABLE AS
$$
    import PGutils (PGm)

    poop :: PGm (Maybe Char)
    poop = return (Just '\x0001F4A9')
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.shrug() RETURNS text IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Text (Text)

    shrug :: PGm (Maybe Text)
    shrug = return (Just "¯\\_(ツ)_/¯")
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.len(bytea) RETURNS int IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.ByteString (length, ByteString)
    import Data.Int (Int32)

    len :: Maybe ByteString -> PGm (Maybe Int32)
    len Nothing = return Nothing
    len (Just s) = return $ Just $ fromIntegral $ Data.ByteString.length s
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.len(text) RETURNS int IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    import Data.Text (length, Text)

    len :: Maybe Text -> PGm (Maybe Int32)
    len Nothing = return Nothing
    len (Just s) = return $ Just $ fromIntegral $ Data.Text.length s
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.make_length_bytea(int) RETURNS bytea IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.ByteString (pack, ByteString)
    import Data.Int (Int32)
    import Data.Word (Word8)

    make_length :: Int32 -> [Word8]
    make_length n = take (fromIntegral n) (repeat 0)

    make_length_bytea :: Maybe Int32 -> PGm (Maybe ByteString)
    make_length_bytea Nothing = return Nothing
    make_length_bytea (Just n) = return $ Just (pack (make_length n))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.make_length_text(int) RETURNS text IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    import Data.Text (pack, Text)

    make_length_text :: Maybe Int32 -> PGm (Maybe Text)
    make_length_text Nothing = return Nothing
    make_length_text (Just n) = return $ Just $ pack (take (fromIntegral n) (repeat '_'))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.inv(bool) RETURNS bool IMMUTABLE AS
$$
    import PGutils (PGm)

    inv :: Maybe Bool -> PGm (Maybe Bool)
    inv Nothing = return Nothing
    inv (Just True) = return (Just False)
    inv (Just False) = return (Just True)
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.add(int, int) RETURNS int IMMUTABLE AS
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

CREATE FUNCTION plhaskell_test.alpha_test(int) RETURNS plhaskell_test.alpha IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (PGm, raiseError)

    p :: Double
    p = 12.3

    r :: Double
    r = 32.1

    alpha_test :: Maybe Int32 -> PGm (Maybe (Maybe Text, Maybe Int32, Maybe Double))
    alpha_test (Just 1) = return (Just (Just "abc", Just 42, Nothing))
    alpha_test (Just 2) = return (Just (Just "cde", Nothing, Just p))
    alpha_test (Just 3) = return (Just (Nothing, Just 42, Just r))
    alpha_test (Just 4) = return Nothing

    alpha_test _ = raiseError "Invalid"
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.delta_test() RETURNS plhaskell_test.delta IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (PGm)

    delta_test :: PGm (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    delta_test = return (Just (Just (Just (Just "abc", Just 42, Just (42.3)), Just 0), Just ()))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(plhaskell_test.delta) RETURNS plhaskell_test.delta IMMUTABLE AS
$$
    import Data.Int (Int32)
    import Data.Text (Text)

    import PGutils (PGm)

    echo :: Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()) -> PGm (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
    echo = return
$$
LANGUAGE plhaskell;

CREATE TYPE plhaskell_test.n_p AS (n int, p int);

CREATE FUNCTION plhaskell_test.primes(int) RETURNS SETOF plhaskell_test.n_p IMMUTABLE AS
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

CREATE FUNCTION plhaskell_test.query_create() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (UtilityResults))

    query_create :: PGm ()
    query_create = do
        UtilityResults _processed <- query "CREATE TABLE plhaskell_test.t(i int, l text)" []
        return ()
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.query_insert(int, text) RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (InsertResults), QueryParam (QueryParamInt4, QueryParamText))
    import Data.Int (Int32)
    import Data.Text (Text)

    query_insert :: Maybe Int32 -> Maybe Text -> PGm ()
    query_insert i l = do
        InsertResults _processed <- query "INSERT INTO plhaskell_test.t(i, l) VALUES ($1, $2);" [QueryParamInt4 i, QueryParamText l]
        return ()
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.query_insert_returning() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (InsertReturningResults), QueryResultValue (QueryResultValueInt4, QueryResultValueText), raiseError)

    query_insert_returning :: PGm ()
    query_insert_returning = do
        InsertReturningResults processed [header1, header2] [[QueryResultValueInt4 i0, QueryResultValueText l0], [QueryResultValueInt4 i1, QueryResultValueText l1], [QueryResultValueInt4 i2, QueryResultValueText l2]] <- query "INSERT INTO plhaskell_test.t(i, l) SELECT i+3, l FROM plhaskell_test.t WHERE i is not NULL ORDER BY i RETURNING i, l" []

        if processed == 3
        then return ()
        else raiseError "Bad processed"

        if header1 == "i"
        then return ()
        else raiseError "Bad header1"

        if header2 == "l"
        then return ()
        else raiseError "Bad header2"

        if i0 == Just 4
        then return ()
        else raiseError "Bad i0"

        if l0 == Just "A"
        then return ()
        else raiseError "Bad l0"

        if i1 == Just 5
        then return ()
        else raiseError "Bad i1"

        if l1 == Just "B"
        then return ()
        else raiseError "Bad l1"

        if i2 == Just 6
        then return ()
        else raiseError "Bad i2"

        if l2 == Just "C"
        then return ()
        else raiseError "Bad l2"

        return ()
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.query_select() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, query, QueryResults (SelectResults), QueryResultValue (QueryResultValueInt4, QueryResultValueText), raiseError)

    query_select :: PGm ()
    query_select = do
        SelectResults processed [header1, header2] [[QueryResultValueInt4 i0, QueryResultValueText l0], [QueryResultValueInt4 i1, QueryResultValueText l1], [QueryResultValueInt4 i2, QueryResultValueText l2], [QueryResultValueInt4 i3, QueryResultValueText l3], [QueryResultValueInt4 i4, QueryResultValueText l4], [QueryResultValueInt4 i5, QueryResultValueText l5], [QueryResultValueInt4 i6, QueryResultValueText l6]] <- query "SELECT i, l FROM plhaskell_test.t ORDER BY i" []

        if processed == 7
        then return ()
        else raiseError "Bad processed"

        if header1 == "i"
        then return ()
        else raiseError "Bad header1"

        if header2 == "l"
        then return ()
        else raiseError "Bad header2"

        if i0 == Just 1
        then return ()
        else raiseError "Bad i0"

        if l0 == Just "A"
        then return ()
        else raiseError "Bad l0"

        if i1 == Just 2
        then return ()
        else raiseError "Bad i1"

        if l1 == Just "B"
        then return ()
        else raiseError "Bad l1"

        if i2 == Just 3
        then return ()
        else raiseError "Bad i2"

        if l2 == Just "C"
        then return ()
        else raiseError "Bad l2"

        if i3 == Just 4
        then return ()
        else raiseError "Bad i3"

        if l3 == Just "A"
        then return ()
        else raiseError "Bad l3"

        if i4 == Just 5
        then return ()
        else raiseError "Bad i4"

        if l4 == Just "B"
        then return ()
        else raiseError "Bad l4"

        if i5 == Just 6
        then return ()
        else raiseError "Bad i5"

        if l5 == Just "C"
        then return ()
        else raiseError "Bad l5"

        if i6 == Nothing
        then return ()
        else raiseError "Bad i6"

        if l6 == Nothing
        then return ()
        else raiseError "Bad l6"

        return ()
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.query_delete() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (DeleteResults), raiseError)

    query_delete :: PGm ()
    query_delete = do
        DeleteResults processed1 <- query "DELETE FROM plhaskell_test.t" []
        DeleteResults processed2 <- query "DELETE FROM plhaskell_test.t" []

        if processed1 == 7
        then return ()
        else raiseError "Bad processed1"

        if processed2 == 0
        then return ()
        else raiseError "Bad processed2"

        return ()
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.query_drop() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (UtilityResults))

    query_drop :: PGm ()
    query_drop = do
        UtilityResults _processed <- query "DROP TABLE plhaskell_test.t" []
        return ()
$$
LANGUAGE plhaskell;

SELECT plhaskell_test.query_create();

SELECT plhaskell_test.query_insert(NULL::int, NULL::text);
SELECT plhaskell_test.query_insert(1, 'A');
SELECT plhaskell_test.query_insert(2, 'B');
SELECT plhaskell_test.query_insert(3, 'C');

SELECT plhaskell_test.query_insert_returning();

SELECT plhaskell_test.query_select();

SELECT plhaskell_test.query_delete();

SELECT plhaskell_test.query_drop();

CREATE FUNCTION plhaskell_test.query_composite() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, query, QueryResults (SelectResults), QueryResultValue (QueryResultValueComposite, QueryResultValueInt4, QueryResultValueText, QueryResultValueFloat8), raiseError)

    query_composite :: PGm ()
    query_composite = do
        SelectResults processed [header] [row0, row1, row2, row3] <- query "SELECT d FROM plhaskell_test.deltas ORDER BY i" []

        if processed == 4
        then return ()
        else raiseError "Bad processed"

        if header == "d"
        then return ()
        else raiseError "Bad header"

        let [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueText (Just x0), QueryResultValueInt4 (Just x1), QueryResultValueFloat8 (Just x2)]), QueryResultValueInt4 (Just x3)]), QueryResultValueComposite (Just [])])] = row0

        if x0 == "Hello"
        then return ()
        else raiseError "Bad x0"

        if x1 == 12
        then return ()
        else raiseError "Bad x1"

        if x2 == 3.4
        then return ()
        else raiseError "Bad x2"

        if x3 == 76
        then return ()
        else raiseError "Bad x3"

        let [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueComposite (Just [QueryResultValueText (Just x4), QueryResultValueInt4 (Just x5), QueryResultValueFloat8 (Just x6)]), QueryResultValueInt4 (Just x7)]), QueryResultValueComposite Nothing])] = row1

        if x4 == "world"
        then return ()
        else raiseError "Bad x4"

        if x5 == 42
        then return ()
        else raiseError "Bad x5"

        if x6 == 1.0
        then return ()
        else raiseError "Bad x6"

        if x7 == -12
        then return ()
        else raiseError "Bad x7"

        let [QueryResultValueComposite (Just [QueryResultValueComposite Nothing, QueryResultValueComposite (Just [])])] = row2
        let [QueryResultValueComposite Nothing] = row3

        return ()
$$
LANGUAGE plhaskell;

CREATE TABLE plhaskell_test.deltas(i int, d plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (1, ((('Hello', 12, 3.4)::plhaskell_test.alpha, 76)::plhaskell_test.bravo, '()'::plhaskell_test.charlie)::plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (2, ((('world', 42, 1.0)::plhaskell_test.alpha, -12)::plhaskell_test.bravo, NULL::plhaskell_test.charlie)::plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (3, (NULL::plhaskell_test.bravo, '()'::plhaskell_test.charlie)::plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (4, NULL::plhaskell_test.delta);

SELECT plhaskell_test.query_composite();

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
