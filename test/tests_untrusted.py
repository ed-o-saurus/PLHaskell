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


class TestUntrusted(TestCase):
    def setUp(self):
        self.conn = connect(row_factory=dict_row)

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA IF EXISTS plhaskell_test CASCADE")
            cur.execute("CREATE SCHEMA plhaskell_test")
            cur.execute("SET search_path TO plhaskell_test")

            cur.execute("CREATE TYPE n_p AS (n int, p int)")

            cur.execute("CREATE TABLE primes(n int, p int)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 1,  2)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 2,  3)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 3,  5)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 4,  7)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 5, 11)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 6, 13)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 7, 17)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 8, 19)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES ( 9, 23)")
            cur.execute("INSERT INTO PRIMES(n, p) VALUES (10, 29)")

            cur.execute("CREATE TABLE primes_upper(n int, p int)")

        self.conn.commit()

    def tearDown(self):
        if self.conn.closed:
            self.conn = connect(row_factory=dict_row)

        self.conn.rollback()

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA plhaskell_test CASCADE")

        self.conn.commit()

        self.conn.close()

    def test_primes_upper(self):
        with self.conn.cursor() as cur:
            with open("sql/primes_upper.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute(
                "INSERT INTO primes_upper(n, p) SELECT n, p FROM primes_upper(13)"
            )

            cur.execute("SELECT n, p FROM primes_upper")

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

    def test_forty_two(self):
        with self.conn.cursor() as cur:
            with open("sql/forty_two.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT forty_two()")
            self.assertEqual(cur.fetchone()["forty_two"], 42)
