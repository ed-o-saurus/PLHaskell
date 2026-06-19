## This is a "procedural language" extension of PostgreSQL
## allowing the execution of code in Haskell within SQL code.
##
## Copyright (C) 2026 Edward F. Behn, Jr.
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

PLHASKELL_VERSION = 5.0

.NOTPARALLEL:

.PHONY: all install clean distclean uninstall

# touch is to force update of .hi file
define HS_COMPILE =
ghc -Wall -O1 -Werror -optc -Wall -optc -Werror -fforce-recomp -optc -fvisibility=hidden -isrc -c $< -dynamic -I$(PG_INCLUDE_DIR) -fPIC -package-name pgutils-$(PLHASKELL_VERSION)
touch $@
endef

define C_COMPILE =
ghc -Wall -O1 -Werror -optc -Wall -optc -Werror -fforce-recomp -c $< -I$(PG_INCLUDE_DIR) -I. -D_GNU_SOURCE -fPIC
endef

define HS_BUILD =
hsc2hs $< -I$(PG_INCLUDE_DIR) -I$(RTS_INCLUDE_DIR)
endef

all : src/PGutils/Common.dyn_hi src/PGutils.dyn_hi src/PGutils/Support.dyn_hi src/PGutils/Array.dyn_hi src/PGutils/Datetime.dyn_hi src/PGutils/Lock.dyn_hi src/PGutils/Range.dyn_hi src/plhaskell.so src/pgutils.conf selinux/plhaskell.pp

clean :
	rm -fv src/*.hi src/*.dyn_hi src/*_stub.h src/*.hs src/*.o src/PGutils/*.hi src/PGutils/*.dyn_hi src/PGutils/*_stub.h src/PGutils/*.hs src/PGutils/*.o src/*.so src/*.conf src/*_hsc_make.c src/PGutils/*_hsc_make.c selinux/*.mod selinux/*.pp

distclean: clean

src/plhaskell.so : src/plhaskell.o src/array_plh.o src/datetime_plh.o src/error_plh.o src/lock_plh.o src/range_plh.o src/spi_plh.o src/PGutils/PLHaskell.o src/PGutils.o src/PGutils/Support.o src/PGutils/Array.o src/PGutils/Datetime.o src/PGutils/Lock.o src/PGutils/Range.o src/PGutils/Common.o
	ghc -Wall -O1 -Werror -optc -Wall -fforce-recomp $^ -o $@ -dynamic -shared -L$(RTS_LIB_DIR) -L$(HINT_DYN_LIB_DIR) -L$(TEXT_DYN_LIB_DIR) -L$(BYTESTRING_DYN_LIB_DIR) -l$(RTS_NAME)-ghc$(GHC_VERSION) -l$(HINT_NAME)-ghc$(GHC_VERSION) -l$(TEXT_NAME)-ghc$(GHC_VERSION) -l$(BYTESTRING_NAME)-ghc$(GHC_VERSION) -optl-Wl,-rpath,$(RTS_LIB_DIR):$(HINT_DYN_LIB_DIR):$(TEXT_DYN_LIB_DIR):$(BYTESTRING_DYN_LIB_DIR)

src/plhaskell.o : src/plhaskell.c src/PGutils/PLHaskell_stub.h src/plhaskell.h src/spi_plh.h src/error_plh.h
	$(call C_COMPILE)

src/array_plh.o : src/array_plh.c src/array_plh.h src/plhaskell.h src/spi_plh.h
	$(call C_COMPILE)

src/datetime_plh.o : src/datetime_plh.c src/datetime_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/error_plh.o : src/error_plh.c src/error_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/lock_plh.o : src/lock_plh.c src/lock_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/range_plh.o : src/range_plh.c src/range_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/spi_plh.o : src/spi_plh.c src/spi_plh.h src/plhaskell.h
	$(call C_COMPILE)

src/PGutils/PLHaskell.o src/PGutils/PLHaskell_stub.h src/PGutils/PLHaskell.hi : src/PGutils/PLHaskell.hs src/PGutils/Common.hi src/plhaskell.h src/error_plh.h
	$(call HS_COMPILE)

src/PGutils.o src/PGutils.hi : src/PGutils.hs src/PGutils/Support.hi src/PGutils/Array.hi src/PGutils/Datetime.hi src/PGutils/Lock.hi src/PGutils/Range.hi src/PGutils/Common.hi src/plhaskell.h src/error_plh.h src/spi_plh.h
	$(call HS_COMPILE)

src/PGutils/Support.o src/PGutils/Support.hi : src/PGutils/Support.hs src/PGutils/Common.hi src/plhaskell.h src/spi_plh.h
	$(call HS_COMPILE)

src/PGutils/Array.o src/PGutils/Array.hi : src/PGutils/Array.hs src/PGutils/Common.hi src/array_plh.h src/error_plh.h
	$(call HS_COMPILE)

src/PGutils/Datetime.o src/PGutils/Datetime.hi : src/PGutils/Datetime.hs src/PGutils/Common.hi src/datetime_plh.h
	$(call HS_COMPILE)

src/PGutils/Lock.o src/PGutils/Lock.hi : src/PGutils/Lock.hs src/PGutils/Common.hi src/lock_plh.h
	$(call HS_COMPILE)

src/PGutils/Range.o src/PGutils/Range.hi : src/PGutils/Range.hs src/PGutils/Common.hi src/range_plh.h
	$(call HS_COMPILE)

src/PGutils/Common.o src/PGutils/Common.hi : src/PGutils/Common.hs src/error_plh.h
	$(call HS_COMPILE)

src/PGutils/PLHaskell.hs : src/PGutils/PLHaskell.hsc src/plhaskell.h
	$(call HS_BUILD)

src/PGutils/Array.hs : src/PGutils/Array.hsc
	$(call HS_BUILD)

src/PGutils/Datetime.hs : src/PGutils/Datetime.hsc
	$(call HS_BUILD)

src/PGutils/Lock.hs : src/PGutils/Lock.hsc
	$(call HS_BUILD)

src/PGutils/Range.hs : src/PGutils/Range.hsc
	$(call HS_BUILD)

src/PGutils/Common.hs : src/PGutils/Common.hsc src/plhaskell.h
	$(call HS_BUILD)

src/PGutils/Support.hs : src/PGutils/Support.hsc
	$(call HS_BUILD)

src/PGutils.hs : src/PGutils.hsc src/plhaskell.h
	$(call HS_BUILD)

%.dyn_hi : %.hi
	cp $^ $@

src/pgutils.conf :
	echo "name:            pgutils"                                                       > $@
	echo "version:         $(PLHASKELL_VERSION)"                                         >> $@
	echo "visibility:      public"                                                       >> $@
	echo "id:              pgutils-$(PLHASKELL_VERSION)"                                 >> $@
	echo "key:             pgutils-$(PLHASKELL_VERSION)"                                 >> $@
	echo "license:         GPL"                                                          >> $@
	echo "synopsis:        PL/Haskell Utilities"                                         >> $@
	echo "exposed:         True"                                                         >> $@
	echo "exposed-modules: PGutils PGutils.Support PGutils.Array PGutils.Datetime PGutils.Lock PGutils.Range PGutils.Common" >> $@
	echo "import-dirs:     $(PG_PKG_LIB_DIR)"                                            >> $@

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

install : src/plhaskell.control src/plhaskell.sql src/PGutils.dyn_hi src/PGutils/Support.dyn_hi src/PGutils/Array.dyn_hi src/PGutils/Datetime.dyn_hi src/PGutils/Lock.dyn_hi src/PGutils/Range.dyn_hi src/PGutils/Common.dyn_hi src/plhaskell.so src/pgutils.conf selinux/plhaskell.pp
	install -m 0644 -D -t $(DESTDIR)$(PG_SHARE_DIR)/extension src/plhaskell.control
	install -m 0644 -D src/plhaskell.sql $(DESTDIR)$(PG_SHARE_DIR)/extension/plhaskell--$(PLHASKELL_VERSION).sql
	install -m 0755 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/plhaskell.so
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR) src/PGutils.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils src/PGutils/Support.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils src/PGutils/Array.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils src/PGutils/Datetime.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils src/PGutils/Lock.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils src/PGutils/Range.dyn_hi
	install -m 0644 -D -t $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils src/PGutils/Common.dyn_hi
	install -m 0644 -D src/pgutils.conf $(GHC_PACKAGE_PATH)/pgutils-$(PLHASKELL_VERSION).conf
	ghc-pkg recache
	if [ -s selinux/plhaskell.pp ]; then install -m 0644 -D -t $(DESTDIR)/usr/share/selinux/packages selinux/plhaskell.pp; fi

uninstall :
	-rm -f  $(DESTDIR)$(PG_SHARE_DIR)/extension/plhaskell.control
	-rm -f  $(DESTDIR)$(PG_SHARE_DIR)/extension/plhaskell--$(PLHASKELL_VERSION).sql
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell.so
	-rm -f  $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils.dyn_hi
	-rm -fr $(DESTDIR)$(PG_PKG_LIB_DIR)/PGutils
	-rm -fr $(DESTDIR)$(PG_PKG_LIB_DIR)/plhaskell_pkg_db
	-rm -f  $(DESTDIR)/usr/share/selinux/packages/plhaskell.pp
