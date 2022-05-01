#!/bin/sh

set -e

export SHARE_DIR=`pg_config --sharedir`
export PKG_LIB_DIR=`pg_config --pkglibdir`

export EXTENSION_DIR=$SHARE_DIR/extension
export GHC_PACKAGE_PATH=$PKG_LIB_DIR/plhaskell_pkg_db

cp -vf src/plhaskell.control  $EXTENSION_DIR
cp -vf src/plhaskell--1.0.sql $EXTENSION_DIR
cp -vf src/plhaskell.so $PKG_LIB_DIR
cp -vf src/PGutils.dyn_hi $PKG_LIB_DIR

ghc-pkg init $GHC_PACKAGE_PATH

ghc-pkg register - << EOF
name:            pgutils
version:         1.0
visibility:      public
id:              pgutils-1.0
key:             pgutils-1.0
license:         GPL
synopsis:        PL/Haskell Utilities
exposed:         True
exposed-modules: PGutils
import-dirs:     $PKG_LIB_DIR
EOF
