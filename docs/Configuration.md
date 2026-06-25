# Configuration

## Maximum Memory

By default, the maximum memory that can be used by the Haskell runtime system is 128 MB. This can be changed by setting the `plhaskell.max_memory` variable in the `postgresql.conf` file. Setting the value to zero disables the limit.

## Package Path

By default, the package search path is the path for `PGutils` and the global path. This can be changed by setting the `plhaskell.package_path` and `plhaskell.package_path_untrusted` variables in the `postgres.conf` file. The values list locations to search separated by `:`. The name `$pkglibdir` can be used to specify the directory returned by the command `pg_config --pkglibdir`. A trailing `:` causes the global path to be searched last.

For example, the string
```
$pkglibdir/dir1:/dir2/dir3:
```
Will cause the system to search the `dir1` subdirectory of the package library, followed by `/dir2/dir3` and finally the global path.
