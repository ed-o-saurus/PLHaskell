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

cat - << EOF
name:            pgutils
version:         2.1
visibility:      public
id:              pgutils-2.1
key:             pgutils-2.1
license:         GPL
synopsis:        PL/Haskell Utilities
exposed:         True
exposed-modules: PGutils PGsupport
EOF

echo import-dirs:\ \ \ \ \ $1
