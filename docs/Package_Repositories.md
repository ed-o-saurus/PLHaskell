# Package Repositories

The easiest way to install the project is to use the RPM package repository for Fedora or the apt package repository for Ubuntu.

## Fedora

Add the repository:

**`$>`** `sudo dnf config-manager --add-repo https://ed-o-saurus.github.io/repos/plhaskell/fedora/plhaskell.repo`

Update the repository information:

**`$>`** `sudo dnf update`

Install the package:

**`$>`** `sudo dnf install plhaskell`

## Ubuntu

Add the signing key to the apt keys:

**`$>`** `wget --quiet -O- https://ed-o-saurus.github.io/keys/A9DD4516.asc | gpg --dearmor | sudo tee /usr/share/keyrings/plhaskell-keyring.gpg > /dev/null`

Add the repository:

**`$>`** `echo deb \[signed-by=/usr/share/keyrings/plhaskell-keyring.gpg\] https://ed-o-saurus.github.io/repos/plhaskell/ubuntu/$(lsb_release -cs)/apt-repo stable main | sudo tee /etc/apt/sources.list.d/plhaskell.list > /dev/null`

Update the repository information:

**`$>`** `sudo apt update`

Install the package:

**`$>`** `sudo apt install plhaskell`
