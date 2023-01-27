#!/usr/bin/bash

cat - > pgutils-1.1.conf << EOF
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

echo -n import-dirs:\ \ \ \ \ >> pgutils-1.1.conf
pg_config --pkglibdir         >> pgutils-1.1.conf