# PL/Haskell

This project is a "procedural language" extension of PostgreSQL allowing the execution of code in Haskell within SQL code. Despite the name, Haskell is, of course, not a procedural language as it is a functional language. However, "procedural language" is the term that PostgreSQL uses to describe languages that can be embedded in its SQL code.

The extension allows users, even unprivileged ones, to write, install, and run functions written in Haskell.

## License

This project is covered by [Version 3 of the GNU General Public License](https://www.gnu.org/licenses/gpl-3.0.en.html). (See the file LICENSE in the root directory.)

## Contact

I wrote the project mostly for my own amusement and as a challenge. If you have any questions, comments, or issues, or if you find the project interesting or useful, please don't hesitate to contact me at ed@behn.us.

## Prerequisites

This document assumes that the reader is familiar with PostgreSQL (specifically procedural languages) and with the Haskell language.

## Build and Install

This extension is intended to be build and installed in a Linux environment. It has only been tested on the x86-64 architecture. 

The following are needed to build and install PL/Haskell:
* PostgreSQL server
* PostgreSQL development files
* Glasgow Haskell Compiler (GHC)
* The GHC Hint development module
* libpq-devel

### Build

First, edit `Makefile` to have the variable `PG_INCLUDE_DIR` set to the directory that contains the Postgres include files. This is the directory that contains the file `postgres.h`.

Run, `make` to build all the files needed for the extension.

### Install

You must be `root` to install the extension. Ensure that `pg_config` is available in the path for root. As root, run `install.sh` from the root directory. 

In each database that you wish to have the extension, run the SQL command `CREATE EXTENSION plhaskell;`.

### Uninstall

Run the SQL command `DROP EXTENSION plhaskell;`.

As root, run `uninstall.sh`.

### Testing

A test battery is provided in the `test.sql` file in the root directory. It can be run with the command

```
psql -f tests.sql
```

If successful, it will terminate with the notice `All tests passed`.

### Room for Improvement

The Makefile, install and uninstall scripts should probably be replaced by a cabal or stack script. Unfortunately, I'm not familiar with either of them. If you see room for improvement or have a complete rewrite, drop me a note.

## Trust

PL/Haskell is a "trusted" language. The PostgreSQL manual explains:
<blockquote>
The optional key word TRUSTED specifies that the language does not grant access to data that the user would not otherwise have. Trusted languages are designed for ordinary database users (those without superuser privilege) and allows them to safely create functions and procedures.
</blockquote>

As such, unprivileged users are permitted to write and execute functions without the possibility that they will be able to access information or resources that are not allowed to access. This is accomplished by enforcing Haskell's strong type system.

## Usage

Functions in PL/Haskell are created the same manner as other PostgreSQL functions. The code must be valid Haskell. It must import the `PGm` monad type from the `PGutils` module. 

The code must contain a function with the same name as the PostgreSQL function. Each of its arguments must be of the type `Maybe `*`arg`* where *`arg`* is the Haskell type as determined by the PostgreSQL type as indicated by the following table. 

PostgreSQL Type |   Module    | Haskell Type
--------------- | ----------- | ------------
`bytea`         | `Data.Word` | `[Word8]`
`text`          | `Prelude`   | `String`
`char`          | `Prelude`   | `Char`
`bool`          | `Prelude`   | `Bool`
`smallint`      | `Data.Int`  | `Int16`  
`integer`       | `Data.Int`  | `Int32`
`bigint`        | `Data.Int`  | `Int64`
`real`          | `Prelude`   | `Float`
`float`         | `Prelude`   | `Double`

The function must return type `PGm (Maybe `*`result`*`)` where *`result`* is the appropriate Haskell type as determined by the return type of function. 

In addition, functions can use composite types as arguments or return values provided that the composite types consist of elements that are listed in the table above or are themselves composite types. Composite values are represented as Haskell tuples.

### Returning Sets

Functions can return sets of values by returning type `PGm [Maybe `*`result`*`]` where *`result`* is the appropriate Haskell type as determined by the return type of function. 

### Reporting Messages and Raising Error

To report a message or raise an error, use the function `report :: ErrorLevel -> String -> PGm ()`. `ErrorLevel` is any of the following:

  Level   |
--------- |
exception |
warning   |
notice    |
info      |
log       |
debug1    |
debug2    |
debug3    |
debug4    |
debug5    |

In addition, the function `raiseError :: String -> a` stops execution and raises an error.

## Examples

### Addition

```
CREATE FUNCTION add(int, int) RETURNS int AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
   
    add :: Maybe Int32 -> Maybe Int32 -> PGm (Maybe Int32)
   
    add Nothing Nothing = return Nothing
    add (Just a) (Just b) = return (Just (a+b))
    add a Nothing = return a
    add Nothing b = return b
$$
LANGUAGE plhaskell;
```

### Primes

This section shows how to return a set of results. The functions listed produce lists of prime numbers using the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).

#### Composite type

The function in this section returns a set of composite results.

```
CREATE TYPE n_p AS (n int, p int);

CREATE FUNCTION primes(int) RETURNS SETOF n_p AS
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

#### Infinite list

The following function returns a infinite list of prime numbers

```
CREATE FUNCTION primes() RETURNS SETOF int AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)
    
    sieve :: [Int32] -> [Int32]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    
    primes :: PGm [Maybe Int32]
    primes = return (map Just (sieve [2..]))
$$
LANGUAGE plhaskell;
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
 
### Message

The following demonstrates how to show a notice from within a function.

```
CREATE FUNCTION forty_two() RETURNS int AS
$$
    import PGutils (PGm, report, notice)
    import Data.Int (Int32)
    
    forty_two :: PGm (Maybe Int32)
    forty_two = do
        report notice "Don't Panic"
        return (Just 42)
$$
LANGUAGE plhaskell;
```
