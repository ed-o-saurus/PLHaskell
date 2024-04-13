## This is a "procedural language" extension of PostgreSQL
## allowing the execution of code in Haskell within SQL code.
##
## Copyright (C) 2024 Edward F. Behn, Jr.
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
Version:        4.0
Release:        1%{?dist}
Summary:        PL/Haskell language extention for PostgreSQL

License:        GPLv3
URL:            https://github.com/ed-o-saurus/PLHaskell
Source0:        PLHaskell.tar.gz

BuildRequires:  make checkpolicy policycoreutils postgresql-server-devel ghc-compiler ghc-bytestring-devel ghc-text-devel ghc-hint-devel
Requires(pre):  policycoreutils
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

cd PLHaskell
rm -fv selinux/plhaskell.pp
make SELINUX=1

%install

# Suppress RPATHS errors
export QA_RPATHS=0x0021

rm -rf $RPM_BUILD_ROOT

cd PLHaskell

mkdir -p %{buildroot}/%{_datadir}/licenses/%{name}
install -m 0644 LICENSE %{buildroot}/%{_datadir}/licenses/%{name}/LICENSE

mkdir -p %{buildroot}/%{_datadir}/doc/%{name}
install -m 0644 README.md %{buildroot}/%{_datadir}/doc/%{name}/README.md

%make_install

%post

%{_sbindir}/semodule -i %{_datadir}/selinux/packages/%{name}.pp &> /dev/null

%postun

%{_sbindir}/semodule -r %{name} &> /dev/null

%files

%dir %{_datadir}/licenses/%{name}
%dir %{_datadir}/doc/%{name}

%license %{_datadir}/licenses/%{name}/LICENSE
%doc %{_datadir}/doc/%{name}/README.md

%{_datadir}/pgsql/extension/%{name}.control
%{_datadir}/pgsql/extension/%{name}--%{version}.sql

%{_libdir}/pgsql/%{name}.so
%{_libdir}/pgsql/PGutils.dyn_hi
%{_libdir}/pgsql/PGsupport.dyn_hi
%{_libdir}/pgsql/PGcommon.dyn_hi

%dir %{_libdir}/pgsql/plhaskell_pkg_db
%{_libdir}/pgsql/plhaskell_pkg_db/pgutils-%{version}.conf
%{_libdir}/pgsql/plhaskell_pkg_db/package.cache
%{_libdir}/pgsql/plhaskell_pkg_db/package.cache.lock

%{_datadir}/selinux/packages/%{name}.pp

%clean

rm -frv *
rm -frv %{buildroot}

%changelog
* Sat Apr 13 2024 Ed Behn <ed@behn.us>
- Initial config
