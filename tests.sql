-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2024 Edward F. Behn, Jr.
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

    echo :: Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()) ->
        PGm (Maybe (Maybe (Maybe (Maybe Text, Maybe Int32, Maybe Double), Maybe Int32), Maybe ()))
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

CREATE FUNCTION plhaskell_test.mk_array(int) RETURNS int[] IMMUTABLE AS
$$
    import PGutils (PGm, Array (..))
    import Data.Int (Int32)

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = fxs : chunksOf n sxs
       where (fxs, sxs) = splitAt n xs

    mkList :: Int32 -> [Maybe Int32]
    mkList upper = [if x `mod` 3 == 0 then Nothing else Just x | x <- [0 .. upper-1]]

    mk_array :: Maybe Int32 -> PGm (Maybe (Array (Maybe Int32)))
    mk_array m = case m of
        Nothing -> return Nothing
        Just 0 -> return (Just ArrayEmpty)
        Just 1 -> return (Just (Array1D  20                                                                                                (mkList    2)))
        Just 2 -> return (Just (Array2D (20, 21)                 (                                                            (chunksOf 2) (mkList    6))))
        Just 3 -> return (Just (Array3D (20, 21, 22)             (                                             (chunksOf 3) $ (chunksOf 2) (mkList   24))))
        Just 4 -> return (Just (Array4D (20, 21, 22, 23)         (                              (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList  120))))
        Just 5 -> return (Just (Array5D (20, 21, 22, 23, 24)     (               (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList  720))))
        Just 6 -> return (Just (Array6D (20, 21, 22, 23, 24, 25) ((chunksOf 6) $ (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (mkList 5040))))
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(int[]) RETURNS int[] IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.echo(plhaskell_test.alpha[]) RETURNS plhaskell_test.alpha[] IMMUTABLE AS
$$
    import PGutils (PGm)

    echo :: a -> PGm a
    echo = return
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
    import Data.Text (Text)

    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_insert_returning :: PGm ()
    query_insert_returning = do
        InsertReturningResults processed [header1, header2] [[QueryResultValueInt4 i0, QueryResultValueText l0],
                                                             [QueryResultValueInt4 i1, QueryResultValueText l1],
                                                             [QueryResultValueInt4 i2, QueryResultValueText l2]] <- query "INSERT INTO plhaskell_test.t(i, l) \
                                                                                                                         \ SELECT i+3, l \
                                                                                                                         \ FROM plhaskell_test.t \
                                                                                                                         \ WHERE i is not NULL \
                                                                                                                         \ ORDER BY i RETURNING i, l" []

        assert (processed == 3) "Bad processed"
        assert (header1 == "i") "Bad header1"
        assert (header2 == "l") "Bad header2"
        assert (i0 == Just 4) "Bad i0"
        assert (l0 == Just "A") "Bad l0"
        assert (i1 == Just 5) "Bad i1"
        assert (l1 == Just "B") "Bad l1"
        assert (i2 == Just 6) "Bad i2"
        assert (l2 == Just "C") "Bad l2"

        return ()
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.query_select() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, query, QueryResults (SelectResults), QueryResultValue (QueryResultValueInt4, QueryResultValueText), raiseError)
    import Data.Text (Text)
    
    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_select :: PGm ()
    query_select = do
        SelectResults processed [header1, header2] [[QueryResultValueInt4 i0, QueryResultValueText l0],
                                                    [QueryResultValueInt4 i1, QueryResultValueText l1],
                                                    [QueryResultValueInt4 i2, QueryResultValueText l2],
                                                    [QueryResultValueInt4 i3, QueryResultValueText l3],
                                                    [QueryResultValueInt4 i4, QueryResultValueText l4],
                                                    [QueryResultValueInt4 i5, QueryResultValueText l5],
                                                    [QueryResultValueInt4 i6, QueryResultValueText l6]] <- query "SELECT i, l \
                                                                                                                \ FROM plhaskell_test.t \
                                                                                                                \ ORDER BY i" []

        assert (processed == 7) "Bad processed"
        assert (header1 == "i") "Bad header1"
        assert (header2 == "l") "Bad header2"
        assert (i0 == Just 1) "Bad i0"
        assert (l0 == Just "A") "Bad l0"
        assert (i1 == Just 2) "Bad i1"
        assert (l1 == Just "B") "Bad l1"
        assert (i2 == Just 3) "Bad i2"
        assert (l2 == Just "C") "Bad l2"
        assert (i3 == Just 4) "Bad i3"
        assert (l3 == Just "A") "Bad l3"
        assert (i4 == Just 5) "Bad i4"
        assert (l4 == Just "B") "Bad l4"
        assert (i5 == Just 6) "Bad i5"
        assert (l5 == Just "C") "Bad l5"
        assert (i6 == Nothing) "Bad i6"
        assert (l6 == Nothing) "Bad l6"

        return ()
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.query_delete() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (DeleteResults), raiseError)
    import Data.Text (Text)
    
    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_delete :: PGm ()
    query_delete = do
        DeleteResults processed1 <- query "DELETE FROM plhaskell_test.t" []
        DeleteResults processed2 <- query "DELETE FROM plhaskell_test.t" []

        assert (processed1 == 7) "Bad processed1"
        assert (processed2 == 0) "Bad processed2"

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
    import Data.Text (Text)
    
    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_composite :: PGm ()
    query_composite = do
        SelectResults processed [header] [row0, row1, row2, row3] <- query "SELECT d FROM plhaskell_test.deltas ORDER BY i" []

        assert (processed == 4) "Bad processed"
        assert (header == "d") "Bad header"

        let [QueryResultValueComposite (delta_schema0, delta_type0) (Just [QueryResultValueComposite (bravo_schema0, bravo_type0) (Just [QueryResultValueComposite (alpha_schema0, alpha_type0) (Just [QueryResultValueText (Just x0), QueryResultValueInt4 (Just x1), QueryResultValueFloat8 (Just x2)]), QueryResultValueInt4 (Just x3)]), QueryResultValueComposite (charlie_schema0, charlie_type0) (Just [])])] = row0

        assert (delta_schema0 == "plhaskell_test") "Bad delta_schema0"
        assert (delta_type0 == "delta") "Bad delta_type0"
        assert (bravo_schema0 == "plhaskell_test") "Bad bravo_schema0"
        assert (bravo_type0 == "bravo") "Bad bravo_type0"
        assert (alpha_schema0 == "plhaskell_test") "Bad alpha_schema0"
        assert (alpha_type0 == "alpha") "Bad alpha_type0"
        assert (charlie_schema0 == "plhaskell_test") "Bad charlie_schema0"
        assert (charlie_type0 == "charlie") "Bad charlie_type0"
        assert (x0 == "Hello") "Bad x0"
        assert (x1 == 12) "Bad x1"
        assert (x2 == 3.4) "Bad x2"
        assert (x3 == 76) "Bad x3"

        let [QueryResultValueComposite (delta_schema1, delta_type1) (Just [QueryResultValueComposite (bravo_schema1, bravo_type1) (Just [QueryResultValueComposite (alpha_schema1, alpha_type1) (Just [QueryResultValueText (Just x4), QueryResultValueInt4 (Just x5), QueryResultValueFloat8 (Just x6)]), QueryResultValueInt4 (Just x7)]), QueryResultValueComposite (charlie_schema1, charlie_type1) Nothing])] = row1

        assert (delta_schema1 == "plhaskell_test") "Bad delta_schema1"
        assert (delta_type1 == "delta") "Bad delta_type1"
        assert (bravo_schema1 == "plhaskell_test") "Bad bravo_schema1"
        assert (bravo_type1 == "bravo") "Bad bravo_type1"
        assert (alpha_schema1 == "plhaskell_test") "Bad alpha_schema1"
        assert (alpha_type1 == "alpha") "Bad alpha_type1"
        assert (charlie_schema1 == "plhaskell_test") "Bad charlie_schema1"
        assert (charlie_type1 == "charlie") "Bad charlie_type1"
        assert (x4 == "world") "Bad x4"
        assert (x5 == 42) "Bad x5"
        assert (x6 == 1.0) "Bad x6"
        assert (x7 == -12) "Bad x7"

        let [QueryResultValueComposite (delta_schema2, delta_type2) (Just [QueryResultValueComposite (bravo_schema2, bravo_type2) Nothing, QueryResultValueComposite (charlie_schema2, charlie_type2) (Just [])])] = row2

        assert (delta_schema2 == "plhaskell_test") "Bad delta_schema2"
        assert (delta_type2 == "delta") "Bad delta_type2"
        assert (bravo_schema2 == "plhaskell_test") "Bad bravo_schema2"
        assert (bravo_type2 == "bravo") "Bad bravo_type2"
        assert (charlie_schema2 == "plhaskell_test") "Bad charlie_schema2"
        assert (charlie_type2 == "charlie") "Bad charlie_type2"

        let [QueryResultValueComposite (delta_schema3, delta_type3) Nothing] = row3

        assert (delta_schema3 == "plhaskell_test") "Bad delta_schema3"
        assert (delta_type3 == "delta") "Bad delta_type3"

        return ()
$$
LANGUAGE plhaskell;

CREATE TABLE plhaskell_test.deltas(i int, d plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (1, ((('Hello', 12, 3.4)::plhaskell_test.alpha, 76)::plhaskell_test.bravo, '()'::plhaskell_test.charlie)::plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (2, ((('world', 42, 1.0)::plhaskell_test.alpha, -12)::plhaskell_test.bravo, NULL::plhaskell_test.charlie)::plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (3, (NULL::plhaskell_test.bravo, '()'::plhaskell_test.charlie)::plhaskell_test.delta);
INSERT INTO plhaskell_test.deltas(i, d) VALUES (4, NULL::plhaskell_test.delta);

SELECT plhaskell_test.query_composite();

CREATE FUNCTION plhaskell_test.query_pass_composite() RETURNS void IMMUTABLE AS
$$
    import PGutils (PGm, raiseError, query, QueryResults (SelectResults), QueryParam (QueryParamComposite, QueryParamText, QueryParamInt4, QueryParamFloat8), QueryResultValue (QueryResultValueComposite, QueryResultValueText, QueryResultValueInt4, QueryResultValueFloat8))
    import Data.Text (Text, pack)
    
    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_pass_composite ::  PGm ()
    query_pass_composite = do
        SelectResults _processed _header [row0] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") (Just [QueryParamComposite (Just "plhaskell_test", "bravo") (Just [QueryParamComposite (Just "plhaskell_test", "alpha") (Just [QueryParamText (Just "Hello"), QueryParamInt4 (Just 12), QueryParamFloat8 (Just 3.4)]), QueryParamInt4 (Just 76)]), QueryParamComposite (Just "plhaskell_test", "charlie") (Just [])])]
        let [QueryResultValueComposite (delta_schema0, delta_type0) (Just [QueryResultValueComposite (bravo_schema0, bravo_type0) (Just [QueryResultValueComposite (alpha_schema0, alpha_type0) (Just [QueryResultValueText (Just x0), QueryResultValueInt4 (Just x1), QueryResultValueFloat8 (Just x2)]), QueryResultValueInt4 (Just x3)]), QueryResultValueComposite (charlie_schema0, charlie_type0) (Just [])])] = row0

        assert (delta_schema0 == "plhaskell_test") "Bad delta_schema0"
        assert (delta_type0 == "delta") "Bad delta_type0"
        assert (bravo_schema0 == "plhaskell_test") "Bad bravo_schema0"
        assert (bravo_type0 == "bravo") "Bad bravo_type0"
        assert (alpha_schema0 == "plhaskell_test") "Bad alpha_schema0"
        assert (alpha_type0 == "alpha") "Bad alpha_type0"
        assert (charlie_schema0 == "plhaskell_test") "Bad charlie_schema0"
        assert (charlie_type0 == "charlie") "Bad charlie_type0"
        assert (x0 == "Hello") "Bad x0"
        assert (x1 == 12) "Bad x1"
        assert (x2 == 3.4) "Bad x2"
        assert (x3 == 76) "Bad x3"

        SelectResults _processed _header [row1] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") (Just [QueryParamComposite (Just "plhaskell_test", "bravo") (Just [QueryParamComposite (Just "plhaskell_test", "alpha") (Just [QueryParamText (Just "world"), QueryParamInt4 (Just 42), QueryParamFloat8 (Just 1.0)]), QueryParamInt4 (Just (-12))]), QueryParamComposite (Just "plhaskell_test", "charlie") Nothing])]
        let [QueryResultValueComposite (delta_schema1, delta_type1) (Just [QueryResultValueComposite (bravo_schema1, bravo_type1) (Just [QueryResultValueComposite (alpha_schema1, alpha_type1) (Just [QueryResultValueText (Just x4), QueryResultValueInt4 (Just x5), QueryResultValueFloat8 (Just x6)]), QueryResultValueInt4 (Just x7)]), QueryResultValueComposite (charlie_schema1, charlie_type1) Nothing])] = row1

        assert (delta_schema1 == "plhaskell_test") "Bad delta_schema1"
        assert (delta_type1 == "delta") "Bad delta_type1"
        assert (bravo_schema1 == "plhaskell_test") "Bad bravo_schema1"
        assert (bravo_type1 == "bravo") "Bad bravo_type1"
        assert (alpha_schema1 == "plhaskell_test") "Bad alpha_schema1"
        assert (alpha_type1 == "alpha") "Bad alpha_type1"
        assert (charlie_schema1 == "plhaskell_test") "Bad charlie_schema1"
        assert (charlie_type1 == "charlie") "Bad charlie_type1"
        assert (x4 == "world") "Bad x4"
        assert (x5 == 42) "Bad x5"
        assert (x6 == 1.0) "Bad x6"
        assert (x7 == -12) "Bad x7"

        SelectResults _processed _header [row2] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") (Just [QueryParamComposite (Just "plhaskell_test", "bravo") Nothing, QueryParamComposite (Just "plhaskell_test", "charlie") (Just [])])]
        let [QueryResultValueComposite (delta_schema2, delta_type2) (Just [QueryResultValueComposite (bravo_schema2, bravo_type2) Nothing, QueryResultValueComposite (charlie_schema2, charlie_type2) (Just [])])] = row2

        assert (delta_schema2 == "plhaskell_test") "Bad delta_schema2"
        assert (delta_type2 == "delta") "Bad delta_type2"
        assert (bravo_schema2 == "plhaskell_test") "Bad bravo_schema2"
        assert (bravo_type2 == "bravo") "Bad bravo_type2"
        assert (charlie_schema2 == "plhaskell_test") "Bad charlie_schema2"
        assert (charlie_type2 == "charlie") "Bad charlie_type2"

        SelectResults _processed _header [row3] <- query "SELECT $1" [QueryParamComposite (Just "plhaskell_test", "delta") Nothing]
        let [QueryResultValueComposite (delta_schema3, delta_type3) Nothing] = row3

        assert (delta_schema3 == "plhaskell_test") "Bad delta_schema3"
        assert (delta_type3 == "delta") "Bad delta_type3"

        return ()
$$
LANGUAGE plhaskell;

SELECT plhaskell_test.query_pass_composite();

CREATE FUNCTION plhaskell_test.primes_upper(minimum int) RETURNS SETOF plhaskell_test.n_p IMMUTABLE AS
$$
    import PGutils (unPGm, query, QueryParam (QueryParamInt4), QueryResults (SelectResults), QueryResultValue (QueryResultValueInt4))
    import Data.Int (Int32)

    primes_upper :: Maybe Int32 -> IO [Maybe (Maybe Int32, Maybe Int32)]
    primes_upper minimum = do
        SelectResults _processed [_header_n, _header_p] rows <- unPGm $ query "SELECT n, p FROM plhaskell_test.primes WHERE p >= $1" [QueryParamInt4 minimum]
        return $ map get_data rows
        where get_data [QueryResultValueInt4 n, QueryResultValueInt4 p] = Just (n, p)
$$
LANGUAGE plhaskellu;

CREATE TABLE plhaskell_test.primes(n int, p int);

INSERT INTO plhaskell_test.primes(n, p)
SELECT n, p
FROM plhaskell_test.primes(10);

CREATE TABLE plhaskell_test.primes_upper(n int, p int);

INSERT INTO plhaskell_test.primes_upper(n, p)
SELECT n, p
FROM plhaskell_test.primes_upper(13);

CREATE TABLE plhaskell_test.inline_test(i int, i_sq int);
DO LANGUAGE plhaskell $$
    import PGutils (PGm, query, QueryResults(InsertResults), QueryParam(QueryParamInt4))

    _' :: PGm ()
    _' = do
        InsertResults _processed <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 0), QueryParamInt4 (Just  0)]
        InsertResults _processed <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 1), QueryParamInt4 (Just  1)]
        InsertResults _processed <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 2), QueryParamInt4 (Just  4)]
        InsertResults _processed <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 3), QueryParamInt4 (Just  9)]
        InsertResults _processed <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 4), QueryParamInt4 (Just 16)]
        InsertResults _processed <- query "INSERT INTO plhaskell_test.inline_test(i, i_sq) VALUES ($1, $2)" [QueryParamInt4 (Just 5), QueryParamInt4 (Just 25)]
        return ()
$$;

CREATE TABLE plhaskell_test.query_arrays(a int[]) ;
CREATE FUNCTION plhaskell_test.query_array_insert() RETURNS void VOLATILE AS
$$
    import PGutils (PGm, query, Array(Array6D), QueryResults(InsertResults), QueryParam(QueryParamArray, QueryParamInt4))
    import Data.Int (Int32)

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = fxs : chunksOf n sxs
       where (fxs, sxs) = splitAt n xs

    mkList :: Int32 -> [Maybe Int32]
    mkList upper = [if x `mod` 3 == 0 then Nothing else Just x | x <- [0 .. upper-1]]

    mk_array :: Maybe (Array QueryParam)
    mk_array = Just (Array6D (20, 21, 22, 23, 24, 25) ((chunksOf 6) $ (chunksOf 5) $ (chunksOf 4) $ (chunksOf 3) $ (chunksOf 2) (map QueryParamInt4 (mkList 5040))))

    query_array_insert :: PGm ()
    query_array_insert = do
        InsertResults _processed <- query "INSERT INTO plhaskell_test.query_arrays(a) VALUES ($1)" [QueryParamArray (Nothing, "int4") mk_array]
        return ()
$$
LANGUAGE plhaskell;

SELECT plhaskell_test.query_array_insert();

CREATE FUNCTION plhaskell_test.query_array_select() RETURNS int[] IMMUTABLE AS
$$
    import PGutils (PGm, raiseError, query, arrayMap, Array(Array6D), QueryResults(SelectResults), QueryResultValue(QueryResultValueArray, QueryResultValueInt4))
    import Data.Int (Int32)
    import Data.Text (Text)
    
    assert :: Bool -> Text -> PGm ()
    assert True _msg = return ()
    assert False msg = raiseError msg

    query_array_select :: PGm (Maybe (Array (Maybe Int32)))
    query_array_select = do
        SelectResults _processed [_header] [[QueryResultValueArray (_schema, name) a]] <- query "SELECT a FROM plhaskell_test.query_arrays" [];

        assert (name == "int4") "Bad type name"

        return (fmap (arrayMap (\ (QueryResultValueInt4 i) -> i)) a)
$$
LANGUAGE plhaskell;

CREATE FUNCTION plhaskell_test.forty_two() RETURNS int IMMUTABLE AS
$$
    import PGutils (unPGm, query, QueryParam (QueryParamInt4), QueryResults (SelectResults), QueryResultValue (QueryResultValueInt4))
    import Data.Int (Int32)

    forty_two :: IO (Maybe Int32)
    forty_two = do
        SelectResults _processed [_header] [[QueryResultValueInt4 val]] <- unPGm $ query "SELECT $1" [QueryParamInt4 (Just 42)]
        return val
$$
LANGUAGE plhaskellu;

DO $$
DECLARE
    r RECORD;

    null_array int[];
    array_empty int[];
    array_1d int[];
    array_2d int[];
    array_3d int[];
    array_4d int[];
    array_5d int[];
    array_6d int[];
    x int;
    a int;

    alpha_array plhaskell_test.alpha[];
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

    FOR r IN
        SELECT count(*) FROM plhaskell_test.primes_upper
    LOOP
        IF r.count <> 5 THEN
            raise EXCEPTION 'primes_upper failed';
        END IF;
    END LOOP;

    FOR r IN
        SELECT * FROM plhaskell_test.primes_upper
    LOOP
        IF r.n = 6 AND r.p <> 13 THEN
            raise EXCEPTION 'primes_upper failed';
        END IF;

        IF r.n = 7 AND r.p <> 17 THEN
            raise EXCEPTION 'primes_upper failed';
        END IF;

        IF r.n = 8 AND r.p <> 19 THEN
            raise EXCEPTION 'primes_upper failed';
        END IF;

        IF r.n = 9 AND r.p <> 23 THEN
            raise EXCEPTION 'primes_upper failed';
        END IF;

        IF r.n = 10 AND r.p <> 29 THEN
            raise EXCEPTION 'primes_upper failed';
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

    null_array = plhaskell_test.mk_array(NULL);
    array_empty = plhaskell_test.mk_array(0);
    array_1d = plhaskell_test.mk_array(1);
    array_2d = plhaskell_test.mk_array(2);
    array_3d = plhaskell_test.mk_array(3);
    array_4d = plhaskell_test.mk_array(4);
    array_5d = plhaskell_test.mk_array(5);
    array_6d = plhaskell_test.mk_array(6);

    IF null_array != NULL THEN
        raise EXCEPTION 'null_array failed';
    END IF;

    IF array_empty != '{}'::int[] THEN
        raise EXCEPTION 'empty array failed';
    END IF;

    FOR idx0 IN 20 .. 21 LOOP
        x = idx0 - 20;
        a = array_1d[idx0];

        IF x%3 = 0 THEN
            IF a is not null THEN
                raise EXCEPTION '1-d array failed';
            END IF;
        ELSE
            IF a is null THEN
                raise EXCEPTION '1-d array failed';
            END IF;
            IF a != x THEN
                raise EXCEPTION '1-d array failed';
            END IF;
        END IF;
    END LOOP;

    FOR idx0 IN 20 .. 22 LOOP
        FOR idx1 IN 21 .. 22 LOOP
            x = 2*(idx0-20)+(idx1-21);
            a = array_2d[idx0][idx1];

            IF x%3 = 0 THEN
                IF a is not null THEN
                    raise EXCEPTION '2-d array failed';
                END IF;
            ELSE
                IF a is null THEN
                    raise EXCEPTION '2-d array failed';
                END IF;
                IF a != x THEN
                    raise EXCEPTION '2-d array failed';
                END IF;
            END IF;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 23 LOOP
        FOR idx1 IN 21 .. 23 LOOP
            FOR idx2 IN 22 .. 23 LOOP
                x = 2*(3*(idx0-20)+(idx1-21))+(idx2-22);
                a = array_3d[idx0][idx1][idx2];

                IF x%3 = 0 THEN
                    IF a is not null THEN
                        raise EXCEPTION '3-d array failed';
                    END IF;
                ELSE
                    IF a is null THEN
                        raise EXCEPTION '3-d array failed';
                    END IF;
                    IF a != x THEN
                        raise EXCEPTION '3-d array failed';
                    END IF;
                END IF;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 24 LOOP
        FOR idx1 IN 21 .. 24 LOOP
            FOR idx2 IN 22 .. 24 LOOP
                FOR idx3 IN 23 .. 24 LOOP
                    x = 2*(3*(4*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23);
                    a = array_4d[idx0][idx1][idx2][idx3];

                    IF x%3 = 0 THEN
                        IF a is not null THEN
                            raise EXCEPTION '4-d array failed';
                        END IF;
                    ELSE
                        IF a is null THEN
                            raise EXCEPTION '4-d array failed';
                        END IF;
                        IF a != x THEN
                            raise EXCEPTION '4-d array failed';
                        END IF;
                    END IF;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 25 LOOP
        FOR idx1 IN 21 .. 25 LOOP
            FOR idx2 IN 22 .. 25 LOOP
                FOR idx3 IN 23 .. 25 LOOP
                    FOR idx4 IN 24 .. 25 LOOP
                        x = 2*(3*(4*(5*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24);
                        a = array_5d[idx0][idx1][idx2][idx3][idx4];

                        IF x%3 = 0 THEN
                            IF a is not null THEN
                                raise EXCEPTION '5-d array failed';
                            END IF;
                        ELSE
                            IF a is null THEN
                                raise EXCEPTION '5-d array failed';
                            END IF;
                            IF a != x THEN
                                raise EXCEPTION '5-d array failed';
                            END IF;
                        END IF;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 26 LOOP
        FOR idx1 IN 21 .. 26 LOOP
            FOR idx2 IN 22 .. 26 LOOP
                FOR idx3 IN 23 .. 26 LOOP
                    FOR idx4 IN 24 .. 26 LOOP
                        FOR idx5 IN 25 .. 26 LOOP
                            x = array_6d[idx0][idx1][idx2][idx3][idx4][idx5];
                            a = 2*(3*(4*(5*(6*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24))+(idx5-25);

                            IF x%3 = 0 THEN
                                IF a is not null THEN
                                    raise EXCEPTION '6-d array failed';
                                END IF;
                            ELSE
                                IF a is null THEN
                                    raise EXCEPTION '6-d array failed';
                                END IF;
                                IF a != x THEN
                                    raise EXCEPTION '6-d array failed';
                                END IF;
                            END IF;
                        END LOOP;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    null_array = plhaskell_test.echo(null_array);
    array_empty = plhaskell_test.echo(array_empty);
    array_1d = plhaskell_test.echo(array_1d);
    array_2d = plhaskell_test.echo(array_2d);
    array_3d = plhaskell_test.echo(array_3d);
    array_4d = plhaskell_test.echo(array_4d);
    array_5d = plhaskell_test.echo(array_5d);
    array_6d = plhaskell_test.echo(array_6d);

    IF null_array != NULL THEN
        raise EXCEPTION 'null_array copy failed';
    END IF;

    IF array_empty != '{}'::int[] THEN
        raise EXCEPTION 'empty array copy failed';
    END IF;

    FOR idx0 IN 20 .. 21 LOOP
        x = idx0 - 20;
        a = array_1d[idx0];

        IF x%3 = 0 THEN
            IF a is not null THEN
                raise EXCEPTION '1-d array copy failed';
            END IF;
        ELSE
            IF a is null THEN
                raise EXCEPTION '1-d array copy failed';
            END IF;
            IF a != x THEN
                raise EXCEPTION '1-d array copy failed';
            END IF;
        END IF;
    END LOOP;

    FOR idx0 IN 20 .. 22 LOOP
        FOR idx1 IN 21 .. 22 LOOP
            x = 2*(idx0-20)+(idx1-21);
            a = array_2d[idx0][idx1];

            IF x%3 = 0 THEN
                IF a is not null THEN
                    raise EXCEPTION '2-d array copy failed';
                END IF;
            ELSE
                IF a is null THEN
                    raise EXCEPTION '2-d array copy failed';
                END IF;
                IF a != x THEN
                    raise EXCEPTION '2-d array copy failed';
                END IF;
            END IF;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 23 LOOP
        FOR idx1 IN 21 .. 23 LOOP
            FOR idx2 IN 22 .. 23 LOOP
                x = 2*(3*(idx0-20)+(idx1-21))+(idx2-22);
                a = array_3d[idx0][idx1][idx2];

                IF x%3 = 0 THEN
                    IF a is not null THEN
                        raise EXCEPTION '3-d array copy failed';
                    END IF;
                ELSE
                    IF a is null THEN
                        raise EXCEPTION '3-d array copy failed';
                    END IF;
                    IF a != x THEN
                        raise EXCEPTION '3-d array copy failed';
                    END IF;
                END IF;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 24 LOOP
        FOR idx1 IN 21 .. 24 LOOP
            FOR idx2 IN 22 .. 24 LOOP
                FOR idx3 IN 23 .. 24 LOOP
                    x = 2*(3*(4*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23);
                    a = array_4d[idx0][idx1][idx2][idx3];

                    IF x%3 = 0 THEN
                        IF a is not null THEN
                            raise EXCEPTION '4-d array copy failed';
                        END IF;
                    ELSE
                        IF a is null THEN
                            raise EXCEPTION '4-d array copy failed';
                        END IF;
                        IF a != x THEN
                            raise EXCEPTION '4-d array copy failed';
                        END IF;
                    END IF;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 25 LOOP
        FOR idx1 IN 21 .. 25 LOOP
            FOR idx2 IN 22 .. 25 LOOP
                FOR idx3 IN 23 .. 25 LOOP
                    FOR idx4 IN 24 .. 25 LOOP
                        x = 2*(3*(4*(5*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24);
                        a = array_5d[idx0][idx1][idx2][idx3][idx4];

                        IF x%3 = 0 THEN
                            IF a is not null THEN
                                raise EXCEPTION '5-d array copy failed';
                            END IF;
                        ELSE
                            IF a is null THEN
                                raise EXCEPTION '5-d array copy failed';
                            END IF;
                            IF a != x THEN
                                raise EXCEPTION '5-d array copy failed';
                            END IF;
                        END IF;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 26 LOOP
        FOR idx1 IN 21 .. 26 LOOP
            FOR idx2 IN 22 .. 26 LOOP
                FOR idx3 IN 23 .. 26 LOOP
                    FOR idx4 IN 24 .. 26 LOOP
                        FOR idx5 IN 25 .. 26 LOOP
                            x = array_6d[idx0][idx1][idx2][idx3][idx4][idx5];
                            a = 2*(3*(4*(5*(6*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24))+(idx5-25);

                            IF x%3 = 0 THEN
                                IF a is not null THEN
                                    raise EXCEPTION '6-d array copy failed';
                                END IF;
                            ELSE
                                IF a is null THEN
                                    raise EXCEPTION '6-d array copy failed';
                                END IF;
                                IF a != x THEN
                                    raise EXCEPTION '6-d array copy failed';
                                END IF;
                            END IF;
                        END LOOP;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    SELECT array_agg(plhaskell_test.alpha_test(i))
    INTO alpha_array
    FROM generate_series(1, 4) i;

    IF alpha_array != plhaskell_test.echo(alpha_array) THEN
        raise EXCEPTION 'alpha_array failed';
    END IF;

    IF plhaskell_test.query_array_select() != plhaskell_test.mk_array(6) THEN
        raise EXCEPTION 'array query failed';
    END IF;

    IF plhaskell_test.forty_two() != 42 THEN
        raise EXCEPTION 'plhaskellu forty_two failed';
    END IF;

    DROP SCHEMA plhaskell_test CASCADE;

    raise NOTICE 'All tests passed';
END
$$;
