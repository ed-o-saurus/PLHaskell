#!/bin/sh

## This is a "procedural language" extension of PostgreSQL
## allowing the execution of code in Haskell within SQL code.
##
## Copyright (C) 2023 Edward F. Behn, Jr.
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

set -e

export SHARE_DIR=`pg_config --sharedir`
export PKG_LIB_DIR=`pg_config --pkglibdir`

export EXTENSION_DIR=$SHARE_DIR/extension
export GHC_PACKAGE_PATH=$PKG_LIB_DIR/plhaskell_pkg_db

cp -vf src/plhaskell.control  $EXTENSION_DIR
cp -vf src/plhaskell--1.1.sql $EXTENSION_DIR
cp -vf src/plhaskell.so     $PKG_LIB_DIR
cp -vf src/PGutils.dyn_hi   $PKG_LIB_DIR
cp -vf src/PGsupport.dyn_hi $PKG_LIB_DIR

mkdir $GHC_PACKAGE_PATH
cp -vf src/pgutils-1.1.conf $GHC_PACKAGE_PATH
ghc-pkg recache
