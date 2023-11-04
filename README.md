# PL/Haskell

This project is a "procedural language" extension of PostgreSQL allowing the execution of code in Haskell within SQL code. Despite the name, Haskell is, of course, not a procedural language as it is a functional language. However, "procedural language" is the term that PostgreSQL uses to describe languages that can be embedded in its SQL code.

The extension allows users, even unprivileged ones, to write, install, and run functions written in Haskell.

## Copyright

Copyright (C) 2023 Edward F. Behn, Jr.

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Contact

I wrote the project mostly for my own amusement and as a challenge. If you have any questions, comments, or issues, or if you find the project interesting or useful, please don't hesitate to contact me at ed@behn.us.

## Bugs and Features

Bug reports and feature requests should be submitted in the Github issues page. 

## Prerequisites

This document assumes that the reader is familiar with PostgreSQL (specifically procedural languages) and with the Haskell language.

## Package Repositories

The easiest way to install the project is to use the RPM package repository for Fedora or the apt package repository for Ubuntu.

### Fedora

Add the repository:

**`$>`** `wget --quiet -O- https://ed-o-saurus.github.io/repos/plhaskell/fedora/plhaskell.repo | sudo tee /etc/yum.repos.d/plhaskell.repo > /dev/null`

Update the repository information:

**`$>`** `sudo dnf update`

Install the package:

**`$>`** `sudo dnf install plhaskell`

### Ubuntu

Add the signing key to the apt keys:

**`$>`** `wget --quiet -O- https://ed-o-saurus.github.io/keys/A9DD4516.asc | gpg --dearmor | sudo tee /usr/share/keyrings/plhaskell-keyring.gpg > /dev/null`

Add the repository:

**`$>`** `echo deb \[signed-by=/usr/share/keyrings/plhaskell-keyring.gpg\] https://ed-o-saurus.github.io/repos/plhaskell/ubuntu/$(lsb_release -cs)/apt-repo stable main | sudo tee /etc/apt/sources.list.d/plhaskell.list > /dev/null`

Update the repository information:

**`$>`** `sudo apt update`

Install the package:

**`$>`** `sudo apt install plhaskell`

## Build and Install

This extension is intended to be built and installed in a Linux environment. It has only been tested on the x86-64 architecture.

### Red Hat Package Management

The following should be done as a non-root user.

Install git, rpmdevtools, make, checkpolicy, policycoreutils, postgresql-server-devel, ghc-compiler, ghc-bytestring-devel, ghc-text-devel, and ghc-hint-devel:

**`$>`** `sudo dnf install git rpmdevtools make selinux-policy-devel postgresql-server-devel ghc-compiler ghc-bytestring-devel ghc-text-devel ghc-hint-devel`

Create the directories necessary to build the .rpm:

**`$>`** `rpmdev-setuptree`

Download the project code:

**`$>`** `git clone https://github.com/ed-o-saurus/PLHaskell`

Copy the `.spec` file to the `~/rpmbuild/SPECS` directory:

**`$>`** `cp PLHaskell/spec/plhaskell.spec ~/rpmbuild/SPECS`

Copy the source to the `~/rpmbuild/SOURCES` directory:

**`$>`** `tar --exclude-vcs -czf ~/rpmbuild/SOURCES/PLHaskell.tar.gz PLHaskell`

Build the `.rpm` package:

**`$>`** `rpmbuild -bb ~/rpmbuild/SPECS/plhaskell.spec`

Install the `.rpm` package written to the `~/rpmbuild/RPMS/`*`<arch>`* directory on the target machine.

### Debian Package Management

The following should be done as a non-root user.

Install git, devscripts, debhelper-compat, postgresql-server-dev-all ghc, libghc-hint-dev:

**`$>`** `sudo apt install git devscripts debhelper-compat postgresql-server-dev-all ghc libghc-hint-dev`

Download the project code:

**`$>`** `git clone https://github.com/ed-o-saurus/PLHaskell`

Build the `.deb` package:

**`$>`** `cd PLHaskell`

**`$>`** `debuild --no-tgz-check`

Install the `.deb` package written to the parent directory on the target machine.

### Other

The alternative to building an `.rpm` or `.deb` package is to build and install the project manually.

The following are needed to build and install PL/Haskell:
* PostgreSQL server
* PostgreSQL development files
* Glasgow Haskell Compiler (GHC)
* The GHC Hint development module
* libpq-devel

#### Build

Ensure that `pg_config` is available in the search path.

From the repository's root directory, build all the files needed for the extension:

**`$>`** `make`

#### Install

Install the extension:

**`$>`** `sudo make install`

#### Uninstall

To uninstall the extension:

**`$>`** `sudo make uninstall`

### Security Enhanced Linux

Systems that use Security Enhanced Linux (SELinux) may encounter problems running the extension. This manifests as the ability to create functions and the inability to call them. Appropriate policies must be implemented. Full details are beyond the scope of this document.

A policy file designed to accommodate Red Hat based systems can be built by running `make SELINUX=1`. Upon installation, this file is saved to `/usr/share/selinux/packages/plhaskell.pp`. It is the user's responsibility to install the policy.

### Testing

Test batteries are provided in the `tests_trusted.sql` and `tests_untrusted.sql` files in the root directory. They can be run with the commands

**`$>`** `psql -f tests_trusted.sql`

**`$>`** `psql -f tests_untrusted.sql`

If successful, they will terminate with the notice `All tests passed`.

## Trust

The package provides two language variants. `plhaskell` is a "trusted" language. The PostgreSQL manual explains:
<blockquote>
The optional key word TRUSTED specifies that the language does not grant access to data that the user would not otherwise have. Trusted languages are designed for ordinary database users (those without superuser privilege) and allows them to safely create functions and procedures.
</blockquote>

As such, unprivileged users are permitted to write and execute functions without the possibility that they will be able to access information or resources that are not allowed to access. This is accomplished by enforcing Haskell's strong type system.

`plhaskellu` is an "untrusted" language. It affords the user more flexibility but lacks the safeguards of the trusted variant. As such, only users with PostgreSQL superuser privilege can create functions with this variant.

## Usage

To install the language in a database, run the SQL command `CREATE EXTENSION plhaskell;`.

Functions in PL/Haskell are created the same manner as other PostgreSQL functions. The code must be valid Haskell.

The code must contain a function with the same name as the PostgreSQL function. Each of its arguments must be of the type `Maybe `*`arg`* where *`arg`* is the Haskell type as determined by the PostgreSQL type as indicated by the following table.

PostgreSQL Type |   Module          | Haskell Type
--------------- | ----------------- | ------------
`bytea`         | `Data.ByteString` | `ByteString`
`text`          | `Data.Text`       | `Text`
`char`          | `Prelude`         | `Char`
`bool`          | `Prelude`         | `Bool`
`smallint`      | `Data.Int`        | `Int16`
`integer`       | `Data.Int`        | `Int32`
`bigint`        | `Data.Int`        | `Int64`
`real`          | `Prelude`         | `Float`
`float`         | `Prelude`         | `Double`

Trusted functions must return type `PGm (Maybe `*`result`*`)` where *`result`* is the appropriate Haskell type as determined by the return type of function while untrusted functions must return type `IO (Maybe `*`result`*`)`. The `PGm`  monad type can be imported from the `PGutils` module.

In addition, functions can use composite types as arguments or return values provided that the composite types consist of elements that are listed in the table above or are themselves composite types. Composite values are represented as Haskell tuples.

### Inline Code

The extension supports anonymous code blocks with the use of the `DO` keyword. Code is written the same way as in functions. The Haskell function to be run must be named `_'` and have signature `_' :: PGm ()` or `_' :: IO ()` depending on the language variant used.

### Returning Sets

Functions can return sets of values by returning type `PGm [Maybe `*`result`*`]` or `IO [Maybe `*`result`*`]` where *`result`* is the appropriate Haskell type as determined by the return type of function.

### Reporting Messages and Raising Error

To report a message or raise an error, use the function `report :: ErrorLevel -> Text -> PGm ()`. `ErrorLevel` is any of the following:

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

In addition, the function `raiseError :: Text -> a` stops execution and raises an error.

### Queries

Queries can be performed from inside PL/Haskell with the function `query :: Text -> [QueryParam] -> PGm QueryResults`. The first argument is the query itself. The second argument is a list of values to be substituted into the query using the expressions `$`*`n`*. This should be used rather than constructing a query string directly to prevent [SQL injection attacks](https://xkcd.com/327/).

Queries in stable and immutable functions are executed in read-only mode, while volatile functions execute queries in read-write mode.

Any base type that can be passed to PL/Haskell functions can be used as a parameter by using the appropriate constructor for the type `QueryParam`. The constructors are the following:

`QueryParam`                          |
------------------------------------- |
`QueryParamByteA  (Maybe ByteString)` |
`QueryParamText   (Maybe Text)`       |
`QueryParamChar   (Maybe Char)`       |
`QueryParamBool   (Maybe Bool)`       |
`QueryParamInt2   (Maybe Int16)`      |
`QueryParamInt4   (Maybe Int32)`      |
`QueryParamInt8   (Maybe Int64)`      |
`QueryParamFloat4 (Maybe Float)`      |
`QueryParamFloat8 (Maybe Double)`     |

The constructors for `QueryResults` are the following:

`QueryResults`                                              |
----------------------------------------------------------- |
`SelectResults          Word64 [Text] [[QueryResultValue]]` |
`SelectIntoResults      Word64`                             |
`InsertResults          Word64`                             |
`DeleteResults          Word64`                             |
`UpdateResults          Word64`                             |
`InsertReturningResults Word64 [Text] [[QueryResultValue]]` |
`DeleteReturningResults Word64 [Text] [[QueryResultValue]]` |
`UpdateReturningResults Word64 [Text] [[QueryResultValue]]` |
`UtilityResults         Word64`                             |
`RewrittenResults       Word64`                             |

The constructor indicates the type of query run. The `Word64` field is the number of rows processed. The `[Text]` field is the names of the columns returned and the `[[QueryResultValue]]` is the data returned.

The constructors for `QueryResultValue` are the following:

`QueryResultValue`                                     |
------------------------------------------------------ |
`QueryResultValueByteA     (Maybe ByteString)`         |
`QueryResultValueText      (Maybe Text)`               |
`QueryResultValueChar      (Maybe Char)`               |
`QueryResultValueBool      (Maybe Bool)`               |
`QueryResultValueInt2      (Maybe Int16)`              |
`QueryResultValueInt4      (Maybe Int32)`              |
`QueryResultValueInt8      (Maybe Int64)`              |
`QueryResultValueFloat4    (Maybe Float)`              |
`QueryResultValueFloat8    (Maybe Double)`             |
`QueryResultValueComposite (Maybe [QueryResultValue])` |

### Converting `PGm` to `IO`

The function `unPGm :: PGm a -> IO a` from the `PGutils` module can be used to convert a `PGm` action to an `IO` action.

## Maximum Memory

By default, the maximum memory that can be used by the Haskell runtime system is 128 MB. This can be changed by setting the `plhaskell.max_memory` variable in the `postgresql.conf` file. Setting the value to zero disables the limit.

## GHC Version

The function `ghc_version` takes no arguments and returns the version of the underlying GHC API. Values are:

* 804 for GHC 8.4.x
* 806 for GHC 8.6.x
* etc...

## Examples

### Hello, world

```
DO LANGUAGE plhaskell $$
    import PGutils (PGm, report, notice)

    _' :: PGm ()
    _' = report notice "Hello, world!"
$$;
```

```
DO LANGUAGE plhaskellu $$
    import PGutils (unPGm, report, notice)

    _' :: IO ()
    _' = unPGm $ report notice "Hello, world!"
$$;
```

### Addition

```
CREATE FUNCTION add(int, int) RETURNS int IMMUTABLE AS
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

```
CREATE FUNCTION add(int, int) RETURNS int IMMUTABLE AS
$$
    import Data.Int (Int32)

    add :: Maybe Int32 -> Maybe Int32 -> IO (Maybe Int32)

    add Nothing Nothing = return Nothing
    add (Just a) (Just b) = return (Just (a+b))
    add a Nothing = return a
    add Nothing b = return b
$$
LANGUAGE plhaskellu;
```

### Fibonacci Sequence

```
CREATE FUNCTION fibonacci(int) RETURNS int IMMUTABLE AS
$$
    import PGutils (PGm)
    import Data.Int (Int32)

    fibonacci' :: Int32 -> Int32
    fibonacci' 0 = 0
    fibonacci' 1 = 1
    fibonacci' n = fibonacci' (n-2) + fibonacci' (n-1)

    fibonacci :: Maybe Int32 -> PGm (Maybe Int32)
    fibonacci Nothing = return Nothing
    fibonacci (Just n) = return (Just (fibonacci' n))
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION fibonacci(int) RETURNS int IMMUTABLE AS
$$
    import Data.Int (Int32)

    fibonacci' :: Int32 -> Int32
    fibonacci' 0 = 0
    fibonacci' 1 = 1
    fibonacci' n = fibonacci' (n-2) + fibonacci' (n-1)

    fibonacci :: Maybe Int32 -> IO (Maybe Int32)
    fibonacci Nothing = return Nothing
    fibonacci (Just n) = return (Just (fibonacci' n))
$$
LANGUAGE plhaskellu;
```

### Primes

This section shows how to return a set of results. The functions listed produce lists of prime numbers using the [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).

#### Composite Type

The functions in this section return a set of composite results.

```
CREATE TYPE n_p AS (n int, p int);
```

```
CREATE FUNCTION primes(int) RETURNS SETOF n_p IMMUTABLE AS
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

```
CREATE FUNCTION primes(int) RETURNS SETOF n_p IMMUTABLE AS
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

#### Infinite List

The following functions return a infinite list of prime numbers

```
CREATE FUNCTION primes() RETURNS SETOF int IMMUTABLE AS
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

```
CREATE FUNCTION primes() RETURNS SETOF int IMMUTABLE AS
$$
    import Data.Int (Int32)

    sieve :: [Int32] -> [Int32]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

    primes :: IO [Maybe Int32]
    primes = return (map Just (sieve [2..]))
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

### Message

The following demonstrate how to show a notice from within a function.

```
CREATE FUNCTION forty_two() RETURNS int IMMUTABLE AS
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

```
CREATE FUNCTION forty_two() RETURNS int IMMUTABLE AS
$$
    import PGutils (unPGm, report, notice)
    import Data.Int (Int32)

    forty_two :: IO (Maybe Int32)
    forty_two = do
        unPGm $ report notice "Don't Panic"
        return (Just 42)
$$
LANGUAGE plhaskellu;
```

### Query

The following delete all elements of table `t` and return the number of rows removed.

```
CREATE FUNCTION remove_all() RETURNS bigint VOLATILE AS
$$
    import PGutils (PGm, query, QueryResults (DeleteResults))
    import Data.Int (Int64)

    remove_all :: PGm (Maybe Int64)
    remove_all = do
        DeleteResults processed <- query "DELETE FROM t" []
        return (Just (fromIntegral processed))
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION remove_all() RETURNS bigint VOLATILE AS
$$
    import PGutils (unPGm, query, QueryResults (DeleteResults))
    import Data.Int (Int64)

    remove_all :: IO (Maybe Int64)
    remove_all = do
        DeleteResults processed <- unPGm $ query "DELETE FROM t" []
        return (Just (fromIntegral processed))
$$
LANGUAGE plhaskellu;
```

The following return the last names of students with the passed first name.

```
CREATE FUNCTION last_names(text) RETURNS SETOF text IMMUTABLE AS
$$
    import PGutils (PGm, query, QueryResults (SelectResults), QueryParam (QueryParamText), QueryResultValue (QueryResultValueText))
    import Data.Text (Text)

    extract_text :: [QueryResultValue] -> Maybe Text
    extract_text [QueryResultValueText name] = name

    last_names :: Maybe Text -> PGm [Maybe Text]
    last_names first_name = do
        SelectResults _processed _header results <- query "SELECT last_name FROM students WHERE first_name = $1" [QueryParamText first_name]
        return (map extract_text results)
$$
LANGUAGE plhaskell;
```

```
CREATE FUNCTION last_names(text) RETURNS SETOF text IMMUTABLE AS
$$
    import PGutils (unPGm, query, QueryResults (SelectResults), QueryParam (QueryParamText), QueryResultValue (QueryResultValueText))
    import Data.Text (Text)

    extract_text :: [QueryResultValue] -> Maybe Text
    extract_text [QueryResultValueText name] = name

    last_names :: Maybe Text -> IO [Maybe Text]
    last_names first_name = do
        SelectResults _processed _header results <- unPGm $ query "SELECT last_name FROM students WHERE first_name = $1" [QueryParamText first_name]
        return (map extract_text results)
$$
LANGUAGE plhaskellu;
```
