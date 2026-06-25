# Usage

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
`date`          | `PGutils`         | `Date`
`time`          | `PGutils`         | `Time`
`timestamp`     | `PGutils`         | `Timestamp`
`interval`      | `PGutils`         | `Interval`

Trusted functions must return type `PGm (Maybe `*`result`*`)` where *`result`* is the appropriate Haskell type as determined by the return type of function while untrusted functions must return type `IO (Maybe `*`result`*`)`. The `PGm`  monad type can be imported from the `PGutils` module.

If a function returns a void (no value), the Haskell function should return type `PGm ()` or `IO ()`.

In addition, functions can use composite types as arguments or return values provided that the composite types consist of elements that are listed in the table above or are themselves composite types. Composite values are represented as Haskell tuples.

## Time and Date

Types `Date`, `Time`, `Timestamp`, and `Interval` represent corresponding PostgreSQL types. They can be imported from the `PGutils` module as can all functions listed in this section.

Timezones are not supported.

### Construction

The type `Date` has two exported constructors. `DateNInfinity` and `DatePInfinity` create dates at negative and positive infinity. In addition, the function `mkDate :: Int32 -> Month -> Int32 -> Date` builds a date from the passed year, month, and day. The type `Month` has constructors with names of standard English month name abbreviations (`Jan`, `Feb`, `Mar`, `Apr`, `May`, `Jun`, `Jul`, `Aug`, `Sep`, `Oct`, `Nov`, `Dec`).

The type `Time` can only be created by the function `mkTime :: Int32 -> Int32 -> Int32 -> Int32 -> Time` which takes arguments of hour, minute, second, and microsecond.

The type `Timestamp` has two exported constructors. `TimestampNInfinity` and `TimestampPInfinity` create timestamps at negative and positive infinity. In addition, the function `mkTimestamp :: mkTimestamp :: Int32 -> Month -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Timestamp` builds a timestamp from the passed year, month, day, hour, minute, second, and microsecond.

The type `Interval` can only be created by the function `mkInterval :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Interval` which takes arguments of years, months, weeks, days, hours, minutes, seconds, and microseconds.

### Arithmetic

The types `Time`, `Timestamp`, and `Interval` are instances of the class `IntervalDiff`.

```
class IntervalDiff a where
  addInterval :: a -> Interval -> a
  subInterval :: a -> Interval -> a
  diff :: a -> a -> Interval
```

The following can be used to perform arithmetic on `Date` or `Interval` types.
* `dateMinusDate :: Date -> Date -> Int32`
* `dateMinusInt :: Date -> Int32 -> Date`
* `datePlusInt :: Date -> Int32 -> Date`
* `doubleTimesInterval :: Double -> Interval -> Interval`
* `dateMinusInterval :: Date -> Interval -> Timestamp`
* `datePlusInterval :: Date -> Interval -> Timestamp`

### Extracting Information

The types `Date` and `Timestamp` are instances of the class `HasDate`.

```
class HasDate a where
  year :: a -> Int32
  month :: a -> Month
  day :: a -> Int32
  dayOfWeek :: a -> Weekday
  dayOfYear :: a -> Int32
  iso :: a -> (Int32, Int32)
```
 The functions `year`, `month`, and `day` extract those fields. The function `dayOfWeek` returns the day of the week.  The type `Weekday` has constructors with names of standard English day abbreviations (`Sun`, `Mon`, `Tue`, `Wen`, `Thu`, `Fri`, `Sat`). The function `dayOfYear` returns the day of the calendar year. The function `iso` returns a tuple of the [ISO standard](https://en.wikipedia.org/wiki/ISO_week_date) year and week.

The types `Time` and `Timestamp` are instances of the class `HasTime`.

```
class HasTime a where
  hour :: a -> Int32
  minute :: a -> Int32
  second :: a -> Int32
  microsecond :: a -> Int32
```

The functions listed extract those fields.

Fields from type `Interval` can be extracted via the functions

* `years :: Interval -> Int32`
* `months :: Interval -> Int32`
* `days :: Interval -> Int32`
* `hours :: Interval -> Int32`
* `minutes :: Interval -> Int32`
* `seconds :: Interval -> Int32`
* `microseconds :: Interval -> Int32`

### Combining and Separating

The function `combineTimestamp :: Date -> Time -> Timestamp` combines a date and time into a timestamp.

The function `separateTimestamp :: Timestamp -> (Date, Time)` separates a timestamp into date and time.

### `Eq` and `Ord`

All time and date types are instances of the classes `Eq` and `Ord`.

### `Show` and `Read`

All time and date types are instances of the classes `Show` and `Read`. The `show` function returns an ISO representation of the type. The `read` function accepts the same formats as PostgreSQL's input functions.

### `Enum`

The `Date` type is an instance of the class `Enum`. The function `fromEnum` returns the number of days since Jan 1, 2000. `fromEnum` of infinite dates is not supported.

### Accessing Current Time

The PostgreSQL functions `transaction_timestamp`, `statement_timestamp`, and `clock_timestamp` can be accessed from the functions `transactionTimestampUTC :: PGm Timestamp`, `statementTimestampUTC :: PGm Timestamp`, and `clockTimestampUTC :: PGm Timestamp` respectively.

## Advisory Locking

Advisory locks can be managed via the following function:

* `lock :: LockMode -> LockLevel -> a -> PGm ()`
* `tryLock :: LockMode -> LockLevel -> a -> PGm Bool`
* `unlock :: LockMode -> a -> PGm Bool`

`a` must be of type `Int64` or `(Int32, Int32)`.

The constructors of `LockMode` are `Shared` and `Exclusive`. The constructors of `LockLevel` are `Transaction` and `Session`.

The function `lock` waits until a lock is acquired. The function `tryLock` returns immediately with `True` if a lock is obtained or `False` if not. The function `unlock` releases a session-level lock and returns `True` if the lock was previously held.

The function `unlockAll :: PGm ()` releases all session-level locks.

## Arrays

Arrays can be passed to and returned from functions. They are represented in Haskell by the `Array a` type which can be imported from the `PGutils` module. The constructors are the following:

`Array a`                                                          |
------------------------------------------------------------------ |
`ArrayEmpty`                                                       |
`Array1D  Int32                                          [a]`      |
`Array2D (Int32, Int32)                                 [[a]]`     |
`Array3D (Int32, Int32, Int32)                         [[[a]]]`    |
`Array4D (Int32, Int32, Int32, Int32)                 [[[[a]]]]`   |
`Array5D (Int32, Int32, Int32, Int32, Int32)         [[[[[a]]]]]`  |
`Array6D (Int32, Int32, Int32, Int32, Int32, Int32) [[[[[[a]]]]]]` |

The `Int32` values represent the lower bounds of the array indexes and the lists represent the contents of the array

### Array Mapping Functions

An `Array` is a `Functor` and `Traversable`. Therefore, `fmap` and `mapM` can be used to transform them.

The functions `arrayMap :: (a -> b) -> Array a -> Array b` and `arrayMapM :: Monad m => (a -> m b) -> Array a -> m (Array b)` are deprecated and will be removed in a future release.

## (Multi)Ranges

Ranges and multiranges can be passed to and returned from functions.

### Ranges

Ranges are represented in Haskell by the `Range a` type which can be imported from the `PGutils` module. The constructors are the following:

`Range a`                        |
-------------------------------- |
`EmtpyRange`                     |
`BoundRange (Bound a) (Bound a)` |

The bounds of `BoundRange` are the lower and upper bound respectively with the following constructors:

`Bound a`       |
----------------|
`InfiniteBound` |
`OpenBound a`   |
`ClosedBound a` |

A `Range` is a `Functor` and `Traversable`. Therefore, `fmap` and `mapM` can be used to transform them.

### Multiranges

Mutiranges are represented in Haskell by the `MultiRange a` type with the constructor `MultiRange [Range a]`.

A `MultiRange` is a `Functor` and `Traversable`. Therefore, `fmap` and `mapM` can be used to transform them.

## Inline Code

The extension supports anonymous code blocks with the use of the `DO` keyword. Code is written the same way as in functions. The Haskell function to be run must be named `_'` and have signature `_' :: PGm ()` or `_' :: IO ()` depending on the language variant used.

## Returning Sets

Functions can return sets of values by returning type `PGm [Maybe `*`result`*`]` or `IO [Maybe `*`result`*`]` where *`result`* is the appropriate Haskell type as determined by the return type of function.

Set returning functions can return sets of voids by the Haskell code returning type `PGm [()]` or `IO [()]`.

## Reporting Messages and Raising Error

To report a message or raise an error, use the function `report :: ErrorLevel -> Text -> PGm ()`.

The constructors for `ErrorLevel` are the following:

`ErrorLevel` |
------------ |
`Fatal`      |
`Exception`  |
`Warning`    |
`Notice`     |
`Info`       |
`Log`        |
`Debug1`     |
`Debug2`     |
`Debug3`     |
`Debug4`     |
`Debug5`     |

In addition, the functions `raiseError :: Text -> a` and `raiseFatal :: Text -> a` stop execution and raise an error.

### Alternate functions

The following functions are available but deprecated:

* `fatal :: ErrorLevel`
* `exception :: ErrorLevel`
* `warning :: ErrorLevel`
* `notice :: ErrorLevel`
* `info :: ErrorLevel`
* `log' :: ErrorLevel`
* `debug1 :: ErrorLevel`
* `debug2 :: ErrorLevel`
* `debug3 :: ErrorLevel`
* `debug4 :: ErrorLevel`
* `debug5 :: ErrorLevel`

## Queries

Queries can be performed from inside PL/Haskell with the function `query :: Text -> [QueryParam] -> PGm QueryResults`. The first argument is the query itself. The second argument is a list of values to be substituted into the query using the expressions `$`*`n`*. This should be used rather than constructing a query string directly to prevent [SQL injection attacks](https://xkcd.com/327/).

Queries in stable and immutable functions are executed in read-only mode, while volatile functions execute queries in read-write mode.

Any type that can be passed to PL/Haskell functions can be used as a parameter by using the appropriate constructor for the type `QueryParam`. The constructors are the following:

`QueryParam`                                                              |
------------------------------------------------------------------------- |
`QueryParamByteA                         (Maybe ByteString)`              |
`QueryParamText                          (Maybe Text)`                    |
`QueryParamChar                          (Maybe Char)`                    |
`QueryParamBool                          (Maybe Bool)`                    |
`QueryParamInt2                          (Maybe Int16)`                   |
`QueryParamInt4                          (Maybe Int32)`                   |
`QueryParamInt8                          (Maybe Int64)`                   |
`QueryParamFloat4                        (Maybe Float)`                   |
`QueryParamFloat8                        (Maybe Double)`                  |
`QueryParamDate                          (Maybe Date)`                    |
`QueryParamTime                          (Maybe Time)`                    |
`QueryParamTimestamp                     (Maybe Timestamp)`               |
`QueryParamInterval                      (Maybe Interval)`                |
`QueryParamComposite  (Maybe Text, Text) (Maybe [QueryParam])`            |
`QueryParamArray      (Maybe Text, Text) (Maybe (Array QueryParam))`      |
`QueryParamRange      (Maybe Text, Text) (Maybe (Range QueryParam))`      |
`QueryParamMultiRange (Maybe Text, Text) (Maybe (MultiRange QueryParam))` |

The `(Maybe Text, Text)` tuple in the `QueryParamComposite`, `QueryParamRange`, and `QueryParamMultiRange` constructors is the schema and name of the composite or range type. The `(Maybe Text, Text)` tuple in the `QueryParamArray` constructor is the schema and name of the element type. If `Nothing` is used as the schema, the first matching type in the search path is used.

Note that all bounds of a `QueryParamRange` or `QueryParamMultiRange` must not be `Nothing`.

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

`QueryResultValue`                                                              |
------------------------------------------------------------------------------- |
`QueryResultValueByteA                   (Maybe ByteString)`                    |
`QueryResultValueText                    (Maybe Text)`                          |
`QueryResultValueChar                    (Maybe Char)`                          |
`QueryResultValueBool                    (Maybe Bool)`                          |
`QueryResultValueInt2                    (Maybe Int16)`                         |
`QueryResultValueInt4                    (Maybe Int32)`                         |
`QueryResultValueInt8                    (Maybe Int64)`                         |
`QueryResultValueFloat4                  (Maybe Float)`                         |
`QueryResultValueFloat8                  (Maybe Double)`                        |
`QueryResultValueDate                    (Maybe Date)`                          |
`QueryResultValueTime                    (Maybe Time)`                          |
`QueryResultValueTimestamp               (Maybe Timestamp)`                     |
`QueryResultValueInterval                (Maybe Interval)`                      |
`QueryResultValueComposite  (Text, Text) (Maybe [QueryResultValue])`            |
`QueryResultValueArray      (Text, Text) (Maybe (Array QueryResultValue))`      |
`QueryResultValueRange      (Text, Text) (Maybe (Range QueryResultValue))`      |
`QueryResultValueMultiRange (Text, Text) (Maybe (MultiRange QueryResultValue))` |

The `(Text, Text)` tuple in the `QueryResultValueComposite` constructor is the schema and name of the composite type. The `(Text, Text)` tuple in the `QueryResultValueArray` constructor is the schema and name of the element type. The `(Text, Text)` tuple in the `QueryResultValueRange` and `QueryResultValueMultiRange` constructors are the schema and name of the (multi)range type.

Note that the elements of (multi)range values always have the constructor `Just`. This is due to the fact that ranges do not support NULL bounds.

### Commit and Rollback

The functions `commit :: Bool -> PGm ()` and `rollback :: Bool -> PGm ()` can be used to commit or rollback transactions in a non-atomic connection.

An argument of `True` causes a new transaction to be started with the same characteristics as the one just completed. This is the equivalent of `COMMIT AND CHAIN` or `ROLLBACK AND CHAIN`.

An argument of `False` causes a new transaction to be started with default characteristics. This is the equivalent of `COMMIT` or `ROLLBACK`.

## Converting `PGm` to `IO`

The function `unPGm :: PGm a -> IO a` from the `PGutils` module can be used to convert a `PGm` action to an `IO` action.
