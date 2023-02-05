#!/bin/sh

cat - << EOF
name:            pgutils
version:         1.1
visibility:      public
id:              pgutils-1.1
key:             pgutils-1.1
license:         GPL
synopsis:        PL/Haskell Utilities
exposed:         True
exposed-modules: PGutils PGsupport
EOF

echo import-dirs:\ \ \ \ \ $1
