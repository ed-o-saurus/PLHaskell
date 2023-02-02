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

Name:           plhaskell
Version:        1.1
Release:        1%{?dist}
Summary:        PL/Haskell language extention for PostgreSQL

License:        GPLv3
URL:            https://github.com/ed-o-saurus/PLHaskell
Source0:        PLHaskell.tar.gz

BuildRequires:  make selinux-policy-devel postgresql-server-devel ghc-compiler ghc-bytestring-devel ghc-text-devel ghc-hint-devel
Requires(pre):  policycoreutils-python-utils
Requires:       postgresql-server ghc ghc-bytestring ghc-text ghc-hint

%description
This project is a "procedural language" extension of PostgreSQL allowing
the execution of code in Haskell within SQL code. Despite the name,
Haskell is, of course, not a procedural language as it is a functional
language. However, "procedural language" is the term that PostgreSQL
uses to describe languages that can be embedded in its SQL code.

The extension allows users, even unprivileged ones, to write, install,
and run functions written in Haskell.

%prep

tar zxfv %{SOURCE0}

%build

cd PLHaskell/src
make all

cd ../rpm
make -f /usr/share/selinux/devel/Makefile %{name}.pp

%install

# Suppress RPATHS warnings
export QA_RPATHS=1

cd PLHaskell

mkdir -p %{buildroot}/%{_datadir}/licenses/%{name}
install -m 0644 LICENSE %{buildroot}/%{_datadir}/licenses/%{name}/LICENSE

mkdir -p %{buildroot}/%{_datadir}/doc/%{name}
install -m 0644 README.md %{buildroot}/%{_datadir}/doc/%{name}/README.md

mkdir -p %{buildroot}/%{_datadir}/pgsql/extension
install -m 0644 src/plhaskell.control  %{buildroot}/%{_datadir}/pgsql/extension/plhaskell.control
install -m 0644 src/plhaskell--%{version}.sql %{buildroot}/%{_datadir}/pgsql/extension/plhaskell--%{version}.sql

mkdir -p %{buildroot}/%{_libdir}/pgsql
install -m 0755 src/plhaskell.so     %{buildroot}/%{_libdir}/pgsql/plhaskell.so
install -m 0644 src/PGutils.dyn_hi   %{buildroot}/%{_libdir}/pgsql/PGutils.dyn_hi
install -m 0644 src/PGsupport.dyn_hi %{buildroot}/%{_libdir}/pgsql/PGsupport.dyn_hi

export GHC_PACKAGE_PATH=%{buildroot}/%{_libdir}/pgsql/plhaskell_pkg_db
mkdir -p $GHC_PACKAGE_PATH
cp -vf src/pgutils-%{version}.conf $GHC_PACKAGE_PATH
ghc-pkg recache

mkdir -p %{buildroot}/%{_datadir}/selinux/packages
install -m 0644 rpm/%{name}.pp %{buildroot}/%{_datadir}/selinux/packages/%{name}.pp

%post

%{_sbindir}/semodule -i %{_datadir}/selinux/packages/%{name}.pp &> /dev/null

%postun

%{_sbindir}/semodule -r %{name} &> /dev/null

%files

%dir %{_datadir}/licenses/%{name}
%dir %{_datadir}/doc/%{name}

%license %{_datadir}/licenses/%{name}/LICENSE
%doc %{_datadir}/doc/%{name}/README.md

%{_datadir}/pgsql/extension/plhaskell.control
%{_datadir}/pgsql/extension/plhaskell--%{version}.sql

%{_libdir}/pgsql/plhaskell.so
%{_libdir}/pgsql/PGutils.dyn_hi
%{_libdir}/pgsql/PGsupport.dyn_hi

%dir %{_libdir}/pgsql/plhaskell_pkg_db
%{_libdir}/pgsql/plhaskell_pkg_db/pgutils-%{version}.conf
%{_libdir}/pgsql/plhaskell_pkg_db/package.cache
%{_libdir}/pgsql/plhaskell_pkg_db/package.cache.lock

%{_datadir}/selinux/packages/%{name}.pp

%clean

rm -frv *
rm -frv %{buildroot}

%changelog
* Sun Jan 29 2023 Ed Behn <ed@behn.us>
- Initial config
