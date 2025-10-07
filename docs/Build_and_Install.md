# Build and Install

This extension is intended to be built and installed in a Linux environment. It has only been tested on the x86-64 architecture.

## Red Hat Package Management

The following should be done as a non-root user.

Install git, rpmdevtools, make, checkpolicy, policycoreutils, postgresql-server-devel, ghc-compiler, ghc-bytestring-devel, ghc-text-devel, and ghc-hint-devel:

**`$>`** `sudo dnf install git rpmdevtools make selinux-policy-devel postgresql-server-devel ghc-compiler ghc-bytestring-devel ghc-text-devel ghc-hint-devel`

Create the directories necessary to build the .rpm:

**`$>`** `rpmdev-setuptree`

Download the project code:

**`$>`** `git clone https://github.com/ed-o-saurus/PLHaskell`

Copy the `.spec` file to the `~/rpmbuild/SPECS` directory:

**`$>`** `cp PLHaskell/spec/plhaskell.spec ~/rpmbuild/SPECS`

Copy the source to the `~/rpmbuild/SOURCES` directory:

**`$>`** `tar --exclude-vcs -czf ~/rpmbuild/SOURCES/PLHaskell.tar.gz PLHaskell`

Build the `.rpm` package:

**`$>`** `rpmbuild -bb ~/rpmbuild/SPECS/plhaskell.spec`

Install the `.rpm` package written to the `~/rpmbuild/RPMS/`*`<arch>`* directory on the target machine.

## Debian Package Management

The following should be done as a non-root user.

Install git, devscripts, debhelper-compat, postgresql-server-dev-all ghc, libghc-hint-dev:

**`$>`** `sudo apt install git devscripts debhelper-compat postgresql-server-dev-all ghc libghc-hint-dev`

Download the project code:

**`$>`** `git clone https://github.com/ed-o-saurus/PLHaskell`

Build the `.deb` package:

**`$>`** `cd PLHaskell`

**`$>`** `debuild --no-tgz-check`

Install the `.deb` package written to the parent directory on the target machine.

## Other

The alternative to building an `.rpm` or `.deb` package is to build and install the project manually.

The following are needed to build and install PL/Haskell:
* PostgreSQL server
* PostgreSQL development files
* Glasgow Haskell Compiler (GHC)
* The GHC hint development package
* The GHC bytestring development package
* The GHC text development package
* libpq-devel

### Build

Ensure that `pg_config` is available in the search path.

From the repository's root directory, build all the files needed for the extension:

**`$>`** `make`

### Install

Install the extension:

**`$>`** `sudo make install`

### Uninstall

To uninstall the extension:

**`$>`** `sudo make uninstall`

## Security Enhanced Linux

Systems that use Security Enhanced Linux (SELinux) may encounter problems running the extension. This manifests as the ability to create functions and the inability to call them. Appropriate policies must be implemented. Full details are beyond the scope of this document.

A policy file designed to accommodate Red Hat based systems can be built by running `make SELINUX=1`. Upon installation, this file is saved to `/usr/share/selinux/packages/plhaskell.pp`. It is the user's responsibility to install the policy.
