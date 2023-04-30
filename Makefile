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

SHELL = /bin/sh

GHC_VERSION = $(shell ghc --numeric-version)

RTS_NAME  = $(shell ghc-pkg field rts  hs-libraries | awk '{print $$2}')
HINT_NAME = $(shell ghc-pkg field hint hs-libraries | awk '{print $$2}')
TEXT_NAME = $(shell ghc-pkg field text hs-libraries | awk '{print $$2}')

RTS_LIB_DIR      = $(shell ghc-pkg field rts  library-dirs         | awk '{print $$2}')
HINT_DYN_LIB_DIR = $(shell ghc-pkg field hint dynamic-library-dirs | awk '{print $$2}')
TEXT_DYN_LIB_DIR = $(shell ghc-pkg field text dynamic-library-dirs | awk '{print $$2}')

PG_INCLUDE_DIR = $(shell pg_config --includedir-server)
PG_SHARE_DIR   = $(shell pg_config --sharedir)
PG_PKG_LIB_DIR = $(shell pg_config --pkglibdir)

.PHONY: all install clean distclean uninstall

all : src/PGutils.dyn_hi src/PGsupport.dyn_hi src/plhaskell.so src/pgutils-2.0.conf selinux/plhaskell.pp

clean :
	rm -fv src/*.hi src/*.dyn_hi src/*_stub.h src/*.hs src/*.o src/*.so src/*.conf selinux/*.mod selinux/*.pp

distclean: clean

src/plhaskell.so : src/plhaskell.o src/PLHaskell.o src/PGutils.o src/PGsupport.o src/MemoryUtils.o
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp $^ -o $@ -dynamic -shared -L$(RTS_LIB_DIR) -L$(HINT_DYN_LIB_DIR) -L$(TEXT_DYN_LIB_DIR) -l$(RTS_NAME)-ghc$(GHC_VERSION) -l$(HINT_NAME)-ghc$(GHC_VERSION) -l$(TEXT_NAME)-ghc$(GHC_VERSION) -optl-Wl,-rpath,$(RTS_LIB_DIR):$(HINT_DYN_LIB_DIR):$(TEXT_DYN_LIB_DIR)

src/plhaskell.o : src/plhaskell.c src/PLHaskell_stub.h src/plhaskell.h
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp -c src/plhaskell.c -o $@ -I$(PG_INCLUDE_DIR) -I. -D_GNU_SOURCE -fPIC

src/PLHaskell.o src/PLHaskell_stub.h src/PLHaskell.hi : src/PLHaskell.hs src/MemoryUtils.hi src/plhaskell.h
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp -optc -fvisibility=hidden -isrc -c src/PLHaskell.hs   -o src/PLHaskell.o   -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-2.0

src/PGutils.o src/PGutils.hi : src/PGutils.hs src/PGsupport.hi src/MemoryUtils.hi src/plhaskell.h
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp -optc -fvisibility=hidden -isrc -c src/PGutils.hs     -o src/PGutils.o     -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-2.0

src/PGsupport.o src/PGsupport.hi : src/PGsupport.hs src/MemoryUtils.hi src/plhaskell.h
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp -optc -fvisibility=hidden -isrc -c src/PGsupport.hs   -o src/PGsupport.o   -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-2.0

src/MemoryUtils.o src/MemoryUtils.hi : src/MemoryUtils.hs
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp -optc -fvisibility=hidden -isrc -c src/MemoryUtils.hs -o src/MemoryUtils.o -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-2.0

%.hs : %.hsc src/plhaskell.h
	hsc2hs $< -I$(PG_INCLUDE_DIR)

%.dyn_hi : %.hi
	cp $^ $@

src/pgutils-2.0.conf :
	./src/mk_pgutils_conf.sh $(PG_PKG_LIB_DIR) > $@


ifeq ($(SELINUX),1)
%.mod : %.te
	checkmodule -M -m -o $@ $^

%.pp : %.mod
	semodule_package -o $@ -m $^
else
%.pp :
	touch $@
endif

install : export GHC_PACKAGE_PATH = $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell_pkg_db

install : src/plhaskell.control src/plhaskell--2.0.sql src/PGutils.dyn_hi src/PGsupport.dyn_hi src/plhaskell.so src/pgutils-2.0.conf selinux/plhaskell.pp
	install -m 0644 -D -t $(DESTDIR)$(PG_SHARE_DIR)/extension src/plhaskell.control
	install -m 0644 -D -t $(DESTDIR)$(PG_SHARE_DIR)/extension src/plhaskell--2.0.sql
	install -m 0755 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/plhaskell.so
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGutils.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGsupport.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell_pkg_db src/pgutils-2.0.conf
	ghc-pkg recache
	if [ -s selinux/plhaskell.pp ]; then install -m 0644 -D -t $(DESTDIR)/usr/share/selinux/packages selinux/plhaskell.pp; fi

uninstall :
	-rm -f  $(DESTDIR)$(PG_SHARE_DIR)/extension/plhaskell.control
	-rm -f  $(DESTDIR)$(PG_SHARE_DIR)/extension/plhaskell--2.0.sql
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell.so
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils.dyn_hi
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGsupport.dyn_hi
	-rm -fr $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell_pkg_db
	-rm -f  $(DESTDIR)/usr/share/selinux/packages/plhaskell.pp

