# This Makefile should be replace by either cabal or stack.
# If you know how, drop me a note.

GHC_VERSION = $(shell ghc --numeric-version)

RTS_NAME  = $(shell ghc-pkg describe rts  | grep hs-libraries | awk '{print $$2}')
HINT_NAME = $(shell ghc-pkg describe hint | grep hs-libraries | awk '{print $$2}')

PG_INCLUDE_DIR = /usr/pgsql-14/include/server

.PHONY: all clean
all : PGutils.dyn_hi plhaskell.so

clean :
	rm -fv *.hi *.dyn_hi *_stub.h *.hs *.o *.so

plhaskell.so : plhaskell.o PLHaskell.o PGutils.o
	ghc -Weverything -Werror -fforce-recomp $^ -o $@ -dynamic -shared -l$(RTS_NAME)-ghc$(GHC_VERSION) -l$(HINT_NAME)-ghc$(GHC_VERSION)

plhaskell.o : plhaskell.c PLHaskell_stub.h plhaskell.h
	ghc -Weverything -Werror -Wno-unsafe -fforce-recomp -c plhaskell.c -o $@ -I$(PG_INCLUDE_DIR) -I. -D_GNU_SOURCE -fPIC

PLHaskell.o PLHaskell_stub.h PLHaskell.hi : PLHaskell.hs plhaskell.h
	ghc -Weverything -Werror -Wno-unsafe -fforce-recomp -c PLHaskell.hs -o PLHaskell.o -dynamic -I$(PG_INCLUDE_DIR) -fPIC

PGutils.o PGutils.hi : PGutils.hs
	ghc -Weverything -Werror -Wno-unsafe -fforce-recomp -c PGutils.hs   -o PGutils.o   -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-1.0

%.hs : %.hsc
	hsc2hs $< -I$(PG_INCLUDE_DIR) -I/usr/lib64/ghc-$(GHC_VERSION)/ -t template-hsc-plhaskell.h

%.dyn_hi : %.hi
	cp $^ $@
