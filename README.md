# PL/Haskell

This project is a "procedural language" extension of PostgreSQL allowing the execution of code in Haskell within SQL code. Despite the name, Haskell is, of course, not a procedural language as it is a functional language. However, "procedural language" is the term that PostgreSQL uses to describe languages that can be embedded in its SQL code.

The extension allows users, even unprivileged ones, to write, install, and run functions written in Haskell.

## Copyright

Copyright (C) 2026 Edward F. Behn, Jr.

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Bugs and Features

Bug reports and feature requests should be submitted in the Github issues page.

## Prerequisites

This document assumes that the reader is familiar with PostgreSQL (specifically procedural languages) and with the Haskell language.

## Further Reading

* [Packages Repositories](docs/Package_Repositories.md)
* [Build and Install](docs/Build_and_Install.md)
* [Trust](docs/Trust.md)
* [Usage](docs/Usage.md)
* [Configuration](docs/Configuration.md)
* [GHC Version](docs/GHC_Version.md)
* [Known Bugs](docs/Known_Bugs.md)
* [Examples](docs/Examples.md)
