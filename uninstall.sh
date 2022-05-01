#!/bin/sh

set -e

export SHARE_DIR=`pg_config --sharedir`
export PKG_LIB_DIR=`pg_config --pkglibdir`

export EXTENSION_DIR=$SHARE_DIR/extension
export GHC_PACKAGE_PATH=$PKG_LIB_DIR/plhaskell_pkg_db

rm -vf $EXTENSION_DIR/plhaskell.control
rm -vf $EXTENSION_DIR/plhaskell--1.0.sql
rm -vf $PKG_LIB_DIR/plhaskell.so
rm -vf $PKG_LIB_DIR/PGutils.dyn_hi

rm -vfr $GHC_PACKAGE_PATH
