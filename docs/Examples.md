# Examples

## Hello, world

```
DO LANGUAGE plhaskell $$
  import PGutils
    ( PGm,
      notice,
      report,
    )

  _' :: PGm ()
  _' = report notice "Hello, world!"
$$;
```

```
DO LANGUAGE plhaskellu $$
  import PGutils
    ( notice,
      report,
      unPGm,
    )

  _' :: IO ()
  _' = unPGm $ report notice "Hello, world!"
$$;
```

## Addition

```
CREATE FUNCTION add(int, int) RETURNS int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( PGm,
    )

  add :: Maybe Int32 -> Maybe Int32 -> PGm (Maybe Int32)
  add Nothing Nothing = return Nothing
  add (Just a) (Just b) = return $ Just $ a + b
  add a Nothing = return a
  add Nothing b = return b
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION add(int, int) RETURNS int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )

  add :: Maybe Int32 -> Maybe Int32 -> IO (Maybe Int32)
  add Nothing Nothing = return Nothing
  add (Just a) (Just b) = return $ Just $ a + b
  add a Nothing = return a
  add Nothing b = return b
$$
LANGUAGE plhaskellu;
```

## Fibonacci Sequence

```
CREATE FUNCTION fibonacci(int) RETURNS int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( PGm,
    )

  fibonacci' :: Int32 -> Int32
  fibonacci' 0 = 0
  fibonacci' 1 = 1
  fibonacci' n = fibonacci' (n - 2) + fibonacci' (n - 1)

  fibonacci :: Maybe Int32 -> PGm (Maybe Int32)
  fibonacci = return . fmap fibonacci'
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION fibonacci(int) RETURNS int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )

  fibonacci' :: Int32 -> Int32
  fibonacci' 0 = 0
  fibonacci' 1 = 1
  fibonacci' n = fibonacci' (n - 2) + fibonacci' (n - 1)

  fibonacci :: Maybe Int32 -> IO (Maybe Int32)
  fibonacci = return . fmap fibonacci'
$$
LANGUAGE plhaskellu;
```

## Primes

This section shows how to return a set of results. The functions listed produce lists of prime numbers using the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).

### Composite Type

The functions in this section return a set of composite results.

```
CREATE TYPE n_p AS (n int, p int);
```

```
CREATE FUNCTION primes(int) RETURNS SETOF n_p IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( PGm,
      raiseError,
    )

  sieve :: [Int32] -> [Int32]
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
  sieve [] = []

  primes :: Maybe Int32 -> PGm [Maybe (Maybe Int32, Maybe Int32)]
  primes Nothing = raiseError "Invalid Null"
  primes (Just n) = return $ zipWith (curry Just) ([Just i | i <- [1 .. n]]) (map Just $ sieve [2 .. ])
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION primes(int) RETURNS SETOF n_p IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( raiseError,
    )

  sieve :: [Int32] -> [Int32]
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
  sieve [] = []

  primes :: Maybe Int32 -> IO [Maybe (Maybe Int32, Maybe Int32)]
  primes Nothing = raiseError "Invalid Null"
  primes (Just n) = return $ zipWith (curry Just) ([Just i | i <- [1 .. n]]) (map Just $ sieve [2 .. ])
$$
LANGUAGE plhaskellu;
```

By running the query
```
SELECT *
FROM primes(10)
```

The following is produced

 n  |  p  |
--- | --- |
  1 |  2  |
  2 |  3  |
  3 |  5  |
  4 |  7  |
  5 | 11  |
  6 | 13  |
  7 | 17  |
  8 | 19  |
  9 | 23  |
 10 | 29  |

### Infinite List

The following functions return a infinite list of prime numbers

```
CREATE FUNCTION primes() RETURNS SETOF int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( PGm,
    )

  sieve :: [Int32] -> [Int32]
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

  primes :: PGm [Maybe Int32]
  primes = return $ map Just $ sieve [2 ..]
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION primes() RETURNS SETOF int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )

  sieve :: [Int32] -> [Int32]
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

  primes :: IO [Maybe Int32]
  primes = return $ map Just $ sieve [2 ..]
$$
LANGUAGE plhaskellu;
```

To generate the first twenty-five prime numbers, run:

```
SELECT primes()
LIMIT 25
```

 primes  |
-------- |
  2      |
  3      |
  5      |
  7      |
 11      |
 13      |
 17      |
 19      |
 23      |
 29      |
 31      |
 37      |
 41      |
 43      |
 47      |
 53      |
 59      |
 61      |
 67      |
 71      |
 73      |
 79      |
 83      |
 89      |
 97      |

## Array

The following add one to all non-null elements of an array.

```
CREATE FUNCTION increment_array(int[]) RETURNS int[] IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( Array,
      PGm,
      arrayMap,
    )

  increment_array :: Maybe (Array (Maybe Int32)) -> PGm (Maybe (Array (Maybe Int32)))
  increment_array  = return . (fmap $ arrayMap $ fmap succ)
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION increment_array(int[]) RETURNS int[] IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( Array,
      arrayMap,
    )

  increment_array :: Maybe (Array (Maybe Int32)) -> IO (Maybe (Array (Maybe Int32)))
  increment_array  = return . (fmap $ arrayMap $ fmap succ)
$$
LANGUAGE plhaskellu;
```

## Message

The following demonstrate how to show a notice from within a function.

```
CREATE FUNCTION forty_two() RETURNS int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( PGm,
      notice,
      report,
    )

  forty_two :: PGm (Maybe Int32)
  forty_two = do
    report notice "Don't Panic"
    return $ Just 42
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION forty_two() RETURNS int IMMUTABLE AS
$$
  import Data.Int
    ( Int32,
    )
  import PGutils
    ( notice,
      report,
      unPGm,
    )

  forty_two :: IO (Maybe Int32)
  forty_two = do
    unPGm $ report notice "Don't Panic"
    return $ Just 42
$$
LANGUAGE plhaskellu;
```

## Query

The following delete all elements of table `t` and return the number of rows removed.

```
CREATE FUNCTION remove_all() RETURNS bigint VOLATILE AS
$$
  import Data.Int
    ( Int64,
    )
  import PGutils
    ( PGm,
      QueryResults (DeleteResults),
      query,
    )

  remove_all :: PGm (Maybe Int64)
  remove_all = do
    DeleteResults processed <- query "DELETE FROM t" []
    return $ Just $ fromIntegral processed
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION remove_all() RETURNS bigint VOLATILE AS
$$
  import Data.Int
    ( Int64,
    )
  import PGutils
    ( QueryResults (DeleteResults),
      query,
      unPGm,
    )

  remove_all :: IO (Maybe Int64)
  remove_all = do
    DeleteResults processed <- unPGm $ query "DELETE FROM t" []
    return $ Just $ fromIntegral processed
$$
LANGUAGE plhaskellu;
```

The following return the last names of students with the passed first name.

```
CREATE FUNCTION last_names(text) RETURNS SETOF text IMMUTABLE AS
$$
  import Data.Text
    ( Text,
    )
  import PGutils
    ( PGm,
      QueryParam (QueryParamText),
      QueryResultValue (QueryResultValueText),
      QueryResults (SelectResults),
      query,
    )

  extract_text :: [QueryResultValue] -> Maybe Text
  extract_text [QueryResultValueText name] = name

  last_names :: Maybe Text -> PGm [Maybe Text]
  last_names first_name = do
    SelectResults _processed _header results <- query "SELECT last_name FROM students WHERE first_name = $1" [QueryParamText first_name]
    return $ map extract_text results
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION last_names(text) RETURNS SETOF text IMMUTABLE AS
$$
  import Data.Text
    ( Text,
    )
  import PGutils
    ( QueryParam (QueryParamText),
      QueryResultValue (QueryResultValueText),
      QueryResults (SelectResults),
      query,
      unPGm,
    )

  extract_text :: [QueryResultValue] -> Maybe Text
  extract_text [QueryResultValueText name] = name

  last_names :: Maybe Text -> IO [Maybe Text]
  last_names first_name = do
    SelectResults _processed _header results <- unPGm $ query "SELECT last_name FROM students WHERE first_name = $1" [QueryParamText first_name]
    return $ map extract_text results
$$
LANGUAGE plhaskellu;
```
