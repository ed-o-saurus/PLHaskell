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

# This Makefile should be replace by either cabal or stack.
# If you know how, drop me a note.

GHC_VERSION = $(shell ghc --numeric-version)

RTS_NAME  = $(shell ghc-pkg describe rts  | grep hs-libraries | awk '{print $$2}')
HINT_NAME = $(shell ghc-pkg describe hint | grep hs-libraries | awk '{print $$2}')
TEXT_NAME = $(shell ghc-pkg describe text | grep hs-libraries | awk '{print $$2}')

PG_INCLUDE_DIR = $(shell pg_config --includedir-server)

.PHONY: all clean
all : PGutils.dyn_hi PGsupport.dyn_hi plhaskell.so

clean :
	rm -fv *.hi *.dyn_hi *_stub.h *.hs *.o *.so *_hsc_make.c

plhaskell.so : plhaskell.o PLHaskell.o PGutils.o PGsupport.o MemoryUtils.o
	ghc -Weverything -Werror -optc -Wall -fforce-recomp $^ -o $@ -dynamic -shared -l$(RTS_NAME)-ghc$(GHC_VERSION) -l$(HINT_NAME)-ghc$(GHC_VERSION) -l$(TEXT_NAME)-ghc$(GHC_VERSION)

plhaskell.o : plhaskell.c PLHaskell_stub.h plhaskell.h
	ghc -Weverything -Werror -Wno-unsafe -optc -Wall -fforce-recomp -c plhaskell.c -o $@ -I$(PG_INCLUDE_DIR) -I. -D_GNU_SOURCE -fPIC

PLHaskell.o PLHaskell_stub.h PLHaskell.hi : PLHaskell.hs MemoryUtils.hi plhaskell.h
	ghc -Weverything -Werror -Wno-unsafe -optc -Wall -fforce-recomp -optc -fvisibility=hidden -c PLHaskell.hs   -o PLHaskell.o   -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-1.1

PGutils.o PGutils.hi : PGutils.hs PGsupport.hi MemoryUtils.hi plhaskell.h
	ghc -Weverything -Werror -Wno-unsafe -optc -Wall -fforce-recomp -optc -fvisibility=hidden -c PGutils.hs     -o PGutils.o     -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-1.1

PGsupport.o PGsupport.hi : PGsupport.hs MemoryUtils.hi plhaskell.h
	ghc -Weverything -Werror -Wno-unsafe -optc -Wall -fforce-recomp -optc -fvisibility=hidden -c PGsupport.hs   -o PGsupport.o   -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-1.1

MemoryUtils.o MemoryUtils.hi : MemoryUtils.hs
	ghc -Weverything -Werror -Wno-unsafe -optc -Wall -fforce-recomp -optc -fvisibility=hidden -c MemoryUtils.hs -o MemoryUtils.o -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-1.1

%.hs : %.hsc plhaskell.h
	hsc2hs $< -I$(PG_INCLUDE_DIR)

%.dyn_hi : %.hi
	cp $^ $@
