# Package Repositories

The easiest way to install the project is to use the RPM package repository for Fedora or the apt package repository for Ubuntu.

## Fedora

Add the repository:

**`$>`** `wget --quiet -O- https://ed-o-saurus.github.io/repos/plhaskell/fedora/plhaskell.repo | sudo tee /etc/yum.repos.d/plhaskell.repo > /dev/null`

Update the repository information:

**`$>`** `sudo dnf update`

Install the package:

**`$>`** `sudo dnf install plhaskell`

## Ubuntu

Add the signing key to the apt keys:

**`$>`** `wget --quiet -O- https://ed-o-saurus.github.io/keys/A9DD4516.asc | sudo gpg --dearmor -o /usr/share/keyrings/plhaskell-keyring.gpg`

Add the repository:

**`$>`** `echo deb \[signed-by=/usr/share/keyrings/plhaskell-keyring.gpg\] https://ed-o-saurus.github.io/repos/plhaskell/ubuntu/$(lsb_release -cs)/apt-repo stable main | sudo tee /etc/apt/sources.list.d/plhaskell.list > /dev/null`

Update the repository information:

**`$>`** `sudo apt update`

Install the package:

**`$>`** `sudo apt install plhaskell`
