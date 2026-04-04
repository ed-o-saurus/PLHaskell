# Known Bugs

## `lc_messages` Language Issue

It is known that the extension does not function properly if `lc_messages` runtime parameter is set to a value other than `'C'` or `'en_US.utf8'`. The workaround is to use the `SET` clause in function declarations.

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
    return (Just 42)
$$
LANGUAGE plhaskell
SET lc_messages TO 'C';
```

## Multiple Set Returning Calls

If a query calls a set returning function multiple times, the query can be prohibitively slow. There is currently no workaround.
