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

from plhaskell_test_base import PLHaskellTestBase


class TestUntrusted(PLHaskellTestBase):
    @staticmethod
    def type_setup(cur):
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

    def test_primes_upper(self):
        self.execute_file("sql/untrusted/primes_upper.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                "INSERT INTO primes_upper(n, p) SELECT n, p FROM primes_upper(13)"
            )

            cur.execute("SELECT n, p FROM primes_upper ORDER BY n")

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
        self.execute_file("sql/untrusted/forty_two.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT forty_two()")
            self.assertEqual(cur.fetchone()["forty_two"], 42)
