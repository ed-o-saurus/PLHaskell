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

from math import isnan
from plhaskell_test_base import PLHaskellTestBase


class TestBaseTypes(PLHaskellTestBase):
    def test_echo_bytea(self):
        self.execute_file("sql/echo_bytea.sql")

        with self.conn.cursor() as cur:
            data = b"\xab\xcd\xef"
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_text(self):
        self.execute_file("sql/echo_text.sql")

        with self.conn.cursor() as cur:
            data = "ABCDEF"
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_char(self):
        self.execute_file("sql/echo_char.sql")

        with self.conn.cursor() as cur:
            data = "A"
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_bool(self):
        self.execute_file("sql/echo_bool.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT echo(%(data)s)", {"data": True})
            self.assertTrue(cur.fetchone()["echo"])

            cur.execute("SELECT echo(%(data)s)", {"data": False})
            self.assertFalse(cur.fetchone()["echo"])

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_smallint(self):
        self.execute_file("sql/echo_smallint.sql")

        with self.conn.cursor() as cur:
            data = 20488
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            data = -29268
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_int(self):
        self.execute_file("sql/echo_int.sql")

        with self.conn.cursor() as cur:
            data = 1372801355
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            data = -1042672097
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_bigint(self):
        self.execute_file("sql/echo_bigint.sql")

        with self.conn.cursor() as cur:
            data = 2263727920641201613
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            data = -591947113936367256
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_real(self):
        self.execute_file("sql/echo_real.sql")

        with self.conn.cursor() as cur:
            data = 42.0
            cur.execute("SELECT echo(%(data)s::real)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_float(self):
        self.execute_file("sql/echo_float.sql")

        with self.conn.cursor() as cur:
            data = 42.0
            cur.execute("SELECT echo(%(data)s::float)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_nan(self):
        self.execute_file("sql/nan.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT nan()")
            self.assertTrue(isnan(cur.fetchone()["nan"]))

    def test_poop(self):
        self.execute_file("sql/poop.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT poop()")
            self.assertEqual(cur.fetchone()["poop"], "💩")

    def test_shrug(self):
        self.execute_file("sql/shrug.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT shrug()")
            self.assertEqual(cur.fetchone()["shrug"], r"¯\_(ツ)_/¯")

    def test_length_bytea(self):
        self.execute_file("sql/length_bytea.sql")

        with self.conn.cursor() as cur:
            data = b"\xab\xcd\xef"
            cur.execute("SELECT length_bytea(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["length_bytea"], len(data))

            cur.execute("SELECT length_bytea(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["length_bytea"])

    def test_length_text(self):
        self.execute_file("sql/length_text.sql")

        with self.conn.cursor() as cur:
            data = "          "
            cur.execute("SELECT length_text(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["length_text"], len(data))

            cur.execute("SELECT length_text(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["length_text"])

    def test_make_length_bytea(self):
        self.execute_file("sql/make_length_bytea.sql")

        with self.conn.cursor() as cur:
            length = 0
            cur.execute(
                "SELECT make_length_bytea(%(length)s)",
                {"length": length},
            )
            self.assertEqual(cur.fetchone()["make_length_bytea"], length * b"\x00")

            length = 10
            cur.execute(
                "SELECT make_length_bytea(%(length)s)",
                {"length": length},
            )
            self.assertEqual(cur.fetchone()["make_length_bytea"], length * b"\x00")

            cur.execute("SELECT make_length_bytea(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["make_length_bytea"])

    def test_make_length_text(self):
        self.execute_file("sql/make_length_text.sql")

        with self.conn.cursor() as cur:
            length = 0
            cur.execute(
                "SELECT make_length_text(%(length)s)",
                {"length": length},
            )
            self.assertEqual(cur.fetchone()["make_length_text"], length * "_")

            length = 10
            cur.execute(
                "SELECT make_length_text(%(length)s)",
                {"length": length},
            )
            self.assertEqual(cur.fetchone()["make_length_text"], length * "_")

            cur.execute("SELECT make_length_text(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["make_length_text"])

    def test_inv(self):
        self.execute_file("sql/inv.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT inv(%(val)s)", {"val": True})
            self.assertFalse(cur.fetchone()["inv"])

            cur.execute("SELECT inv(%(val)s)", {"val": False})
            self.assertTrue(cur.fetchone()["inv"])

            cur.execute("SELECT inv(%(val)s)", {"val": None})
            self.assertIsNone(cur.fetchone()["inv"])

    def test_add(self):
        self.execute_file("sql/add.sql")

        with self.conn.cursor() as cur:
            a = 3
            b = 4

            cur.execute("SELECT add(%(a)s, %(b)s)", {"a": a, "b": b})
            self.assertEqual(cur.fetchone()["add"], a + b)

            cur.execute("SELECT add(%(a)s, %(b)s)", {"a": None, "b": b})
            self.assertIsNone(cur.fetchone()["add"])

            cur.execute("SELECT add(%(a)s, %(b)s)", {"a": a, "b": None})
            self.assertEqual(cur.fetchone()["add"], a)

            cur.execute("SELECT add(%(a)s, %(b)s)", {"a": None, "b": None})
            self.assertIsNone(cur.fetchone()["add"])
