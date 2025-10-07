# PL/Haskell Testing

This directory contains a battery of regression tests. They are designed to ensure proper functioning of the module. In the normal course of operations, they do not need to be run by most users.

They are designed to be run by python3. This document assumes that the user is familiar with python testing.

## Module Installation

Python modules required for the tests are listed in the file `test/requirements.txt`.

They can be installed by running:

**`$>`** `pip install -r test/requirements.txt`

## Database Setup

Before running tests, the user must create a database with the PL/Haskell extension installed. The user must also configure the standard PostgreSQL environment variables (`$PG*`).

Each test will create a temporary schema named `plhaskell_test`.

## Running Tests

Tests must be run from the `test` directory:

**`$>`** `cd test`

To run the tests:

**`$>`** `pytest -v`
