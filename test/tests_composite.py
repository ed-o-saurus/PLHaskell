#!./bin/python3

# This is a "procedural language" extension of PostgreSQL
# allowing the execution of code in Haskell within SQL code.
#
# Copyright (C) 2024 Edward F. Behn, Jr.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

from unittest import TestCase
from psycopg import connect
from psycopg.rows import dict_row
from psycopg.errors import InternalError


class TestComposite(TestCase):
    def setUp(self):
        self.conn = connect(row_factory=dict_row)

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA IF EXISTS plhaskell_test CASCADE")
            cur.execute("CREATE SCHEMA plhaskell_test")
            cur.execute("SET search_path TO plhaskell_test")

            cur.execute("CREATE TYPE alpha AS (a text, b int, c float)")
            cur.execute("CREATE TYPE bravo AS (d alpha, e int)")
            cur.execute("CREATE TYPE charlie AS ()")
            cur.execute("CREATE TYPE delta AS (f bravo, g charlie)")
            cur.execute("CREATE TYPE n_p AS (n int, p int)")

        self.conn.commit()

    def tearDown(self):
        if self.conn.closed:
            self.conn = connect(row_factory=dict_row)

        self.conn.rollback()

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA plhaskell_test CASCADE")

        self.conn.commit()

        self.conn.close()

    def test_alpha(self):
        with self.conn.cursor() as cur:
            with open("sql/alpha_func.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/alpha_func_test.sql", "rt") as file:
                cur.execute(file.read())

            with self.assertRaisesRegex(InternalError, "Invalid"):
                cur.execute("SELECT alpha_func(-1)")

    def test_delta(self):
        with self.conn.cursor() as cur:
            with open("sql/delta_func.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/delta_func_test.sql", "rt") as file:
                cur.execute(file.read())

    def test_echo_delta(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_delta.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/echo_delta_test.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_primes(self):
        with self.conn.cursor() as cur:
            with open("sql/primes.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT n, p FROM primes(10)")

            row = cur.fetchone()
            self.assertEqual(row["n"], 1)
            self.assertEqual(row["p"], 2)

            row = cur.fetchone()
            self.assertEqual(row["n"], 2)
            self.assertEqual(row["p"], 3)

            row = cur.fetchone()
            self.assertEqual(row["n"], 3)
            self.assertEqual(row["p"], 5)

            row = cur.fetchone()
            self.assertEqual(row["n"], 4)
            self.assertEqual(row["p"], 7)

            row = cur.fetchone()
            self.assertEqual(row["n"], 5)
            self.assertEqual(row["p"], 11)

            row = cur.fetchone()
            self.assertEqual(row["n"], 6)
            self.assertEqual(row["p"], 13)

            row = cur.fetchone()
            self.assertEqual(row["n"], 7)
            self.assertEqual(row["p"], 17)

            row = cur.fetchone()
            self.assertEqual(row["n"], 8)
            self.assertEqual(row["p"], 19)

            row = cur.fetchone()
            self.assertEqual(row["n"], 9)
            self.assertEqual(row["p"], 23)

            row = cur.fetchone()
            self.assertEqual(row["n"], 10)
            self.assertEqual(row["p"], 29)

            self.assertIsNone(cur.fetchone())
