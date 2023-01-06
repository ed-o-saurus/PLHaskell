#!/bin/sh

## This is a "procedural language" extension of PostgreSQL
## allowing the execution of code in Haskell within SQL code.
##
## Copyright (C) 2022 Edward F. Behn, Jr.
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

rm -vf $EXTENSION_DIR/plhaskell.control
rm -vf $EXTENSION_DIR/plhaskell--1.1.sql
rm -vf $PKG_LIB_DIR/plhaskell.so
rm -vf $PKG_LIB_DIR/PGutils.dyn_hi

rm -vfr $GHC_PACKAGE_PATH
