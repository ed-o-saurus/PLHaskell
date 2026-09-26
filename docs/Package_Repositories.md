# Package Repositories

The easiest way to install the project is to use the RPM package repository for Fedora or the apt package repository for Ubuntu.

## Fedora

Add the repository:

**`$>`** `sudo dnf config-manager addrepo --from-repofile=https://ed-o-saurus.github.io/repos/plhaskell/fedora/plhaskell.repo`

Update the repository information:

**`$>`** `sudo dnf update`

Install the package:

**`$>`** `sudo dnf install plhaskell`

## Ubuntu

Add the repository:

**`$>`** `wget -P /etc/apt/sources.list.d https://ed-o-saurus.github.io/repos/plhaskell/ubuntu/$(lsb_release -cs)/plhaskell.sources`

Update the repository information:

**`$>`** `sudo apt update`

Install the package:

**`$>`** `sudo apt install plhaskell`
