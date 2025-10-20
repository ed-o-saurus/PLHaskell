## This is a "procedural language" extension of PostgreSQL
## allowing the execution of code in Haskell within SQL code.
##
## Copyright (C) 2025 Edward F. Behn, Jr.
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

RTS_NAME        = $(shell ghc-pkg --simple-output field rts        hs-libraries)
HINT_NAME       = $(shell ghc-pkg --simple-output field hint       hs-libraries)
TEXT_NAME       = $(shell ghc-pkg --simple-output field text       hs-libraries)
BYTESTRING_NAME = $(shell ghc-pkg --simple-output field bytestring hs-libraries)

RTS_LIB_DIR            = $(shell ghc-pkg --simple-output field rts        library-dirs)
HINT_DYN_LIB_DIR       = $(shell ghc-pkg --simple-output field hint       dynamic-library-dirs)
TEXT_DYN_LIB_DIR       = $(shell ghc-pkg --simple-output field text       dynamic-library-dirs)
BYTESTRING_DYN_LIB_DIR = $(shell ghc-pkg --simple-output field bytestring dynamic-library-dirs)

PG_INCLUDE_DIR = $(shell pg_config --includedir-server)
PG_SHARE_DIR   = $(shell pg_config --sharedir)
PG_PKG_LIB_DIR = $(shell pg_config --pkglibdir)

RTS_INCLUDE_DIR = $(shell ghc-pkg --simple-output field rts include-dirs)

.NOTPARALLEL:

.PHONY: all install clean distclean uninstall

# touch is to force update of .hi file
define HS_COMPILE =
ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp -optc -fvisibility=hidden -isrc -c $< -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-5.0
touch $@
endef

define C_COMPILE =
ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp -c $< -I$(PG_INCLUDE_DIR) -I. -D_GNU_SOURCE -fPIC
endef

define HS_BUILD =
hsc2hs $< -I$(PG_INCLUDE_DIR) -I$(RTS_INCLUDE_DIR)
endef

all : src/PGcommon.dyn_hi src/PGutils.dyn_hi src/PGsupport.dyn_hi src/PGarray.dyn_hi src/PGdatetime.dyn_hi src/PGlock.dyn_hi src/plhaskell.so src/pgutils-5.0.conf selinux/plhaskell.pp

clean :
	rm -fv src/*.hi src/*.dyn_hi src/*_stub.h src/*.hs src/*.o src/*.so src/*.conf src/*_hsc_make.c selinux/*.mod selinux/*.pp

distclean: clean

src/plhaskell.so : src/plhaskell.o src/array_plh.o src/datetime_plh.o src/error_plh.o src/lock_plh.o src/spi_plh.o src/PLHaskell.o src/PGutils.o src/PGsupport.o src/PGarray.o src/PGdatetime.o src/PGlock.o src/PGcommon.o
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp $^ -o $@ -dynamic -shared -L$(RTS_LIB_DIR) -L$(HINT_DYN_LIB_DIR) -L$(TEXT_DYN_LIB_DIR) -L$(BYTESTRING_DYN_LIB_DIR) -l$(RTS_NAME)-ghc$(GHC_VERSION) -l$(HINT_NAME)-ghc$(GHC_VERSION) -l$(TEXT_NAME)-ghc$(GHC_VERSION) -l$(BYTESTRING_NAME)-ghc$(GHC_VERSION) -optl-Wl,-rpath,$(RTS_LIB_DIR):$(HINT_DYN_LIB_DIR):$(TEXT_DYN_LIB_DIR):$(BYTESTRING_DYN_LIB_DIR)

src/plhaskell.o : src/plhaskell.c src/PLHaskell_stub.h src/plhaskell.h src/spi_plh.h src/error_plh.h
	$(call C_COMPILE)

src/array_plh.o : src/array_plh.c src/array_plh.h src/plhaskell.h src/spi_plh.h
	$(call C_COMPILE)

src/datetime_plh.o : src/datetime_plh.c src/datetime_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/error_plh.o : src/error_plh.c src/error_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/lock_plh.o : src/lock_plh.c src/lock_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/spi_plh.o : src/spi_plh.c src/spi_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/PLHaskell.o src/PLHaskell_stub.h src/PLHaskell.hi : src/PLHaskell.hs src/PGcommon.hi src/plhaskell.h src/error_plh.h
	$(call HS_COMPILE)

src/PGutils.o src/PGutils.hi : src/PGutils.hs src/PGsupport.hi src/PGarray.hi src/PGdatetime.hi src/PGlock.hi src/PGcommon.hi src/plhaskell.h src/error_plh.h src/spi_plh.h
	$(call HS_COMPILE)

src/PGsupport.o src/PGsupport.hi : src/PGsupport.hs src/PGcommon.hi src/plhaskell.h src/spi_plh.h
	$(call HS_COMPILE)

src/PGarray.o src/PGarray.hi : src/PGarray.hs src/PGcommon.hi src/array_plh.h src/error_plh.h
	$(call HS_COMPILE)

src/PGdatetime.o src/PGdatetime.hi : src/PGdatetime.hs src/PGcommon.hi src/datetime_plh.h
	$(call HS_COMPILE)

src/PGlock.o src/PGlock.hi : src/PGlock.hs src/PGcommon.hi src/lock_plh.h
	$(call HS_COMPILE)

src/PGcommon.o src/PGcommon.hi : src/PGcommon.hs src/error_plh.h
	$(call HS_COMPILE)

src/PLHaskell.hs : src/PLHaskell.hsc src/plhaskell.h
	$(call HS_BUILD)

src/PGarray.hs : src/PGarray.hsc
	$(call HS_BUILD)

src/PGdatetime.hs : src/PGdatetime.hsc
	$(call HS_BUILD)

src/PGlock.hs : src/PGlock.hsc
	$(call HS_BUILD)

src/PGcommon.hs : src/PGcommon.hsc src/plhaskell.h
	$(call HS_BUILD)

src/PGsupport.hs : src/PGsupport.hsc
	$(call HS_BUILD)

src/PGutils.hs : src/PGutils.hsc src/plhaskell.h
	$(call HS_BUILD)

%.dyn_hi : %.hi
	cp $^ $@

src/pgutils-5.0.conf :
	echo "name:            pgutils"                                               > $@
	echo "version:         5.0"                                                  >> $@
	echo "visibility:      public"                                               >> $@
	echo "id:              pgutils-5.0"                                          >> $@
	echo "key:             pgutils-5.0"                                          >> $@
	echo "license:         GPL"                                                  >> $@
	echo "synopsis:        PL/Haskell Utilities"                                 >> $@
	echo "exposed:         True"                                                 >> $@
	echo "exposed-modules: PGutils PGsupport PGarray PGdatetime PGlock PGcommon" >> $@
	echo "import-dirs:     $(PG_PKG_LIB_DIR)"                                    >> $@

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

install : src/plhaskell.control src/plhaskell--5.0.sql src/PGutils.dyn_hi src/PGsupport.dyn_hi src/PGarray.dyn_hi src/PGdatetime.dyn_hi src/PGcommon.dyn_hi src/plhaskell.so src/pgutils-5.0.conf selinux/plhaskell.pp
	install -m 0644 -D -t $(DESTDIR)$(PG_SHARE_DIR)/extension src/plhaskell.control
	install -m 0644 -D -t $(DESTDIR)$(PG_SHARE_DIR)/extension src/plhaskell--5.0.sql
	install -m 0755 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/plhaskell.so
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGutils.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGsupport.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGarray.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGdatetime.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGlock.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGcommon.dyn_hi
	install -m 0644 -D -t $(GHC_PACKAGE_PATH) src/pgutils-5.0.conf
	ghc-pkg recache
	if [ -s selinux/plhaskell.pp ]; then install -m 0644 -D -t $(DESTDIR)/usr/share/selinux/packages selinux/plhaskell.pp; fi

uninstall :
	-rm -f  $(DESTDIR)$(PG_SHARE_DIR)/extension/plhaskell.control
	-rm -f  $(DESTDIR)$(PG_SHARE_DIR)/extension/plhaskell--5.0.sql
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell.so
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils.dyn_hi
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGsupport.dyn_hi
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGarray.dyn_hi
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGdatetime.dyn_hi
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGlock.dyn_hi
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGcommon.dyn_hi
	-rm -fr $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell_pkg_db
	-rm -f  $(DESTDIR)/usr/share/selinux/packages/plhaskell.pp
