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
from math import isnan
from psycopg import connect
from psycopg.rows import dict_row


class TestBaseTypes(TestCase):
    def setUp(self):
        self.conn = connect(row_factory=dict_row)

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA IF EXISTS plhaskell_test CASCADE")
            cur.execute("CREATE SCHEMA plhaskell_test")
            cur.execute("SET search_path TO plhaskell_test")

        self.conn.commit()

    def tearDown(self):
        if self.conn.closed:
            self.conn = connect(row_factory=dict_row)

        self.conn.rollback()

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA plhaskell_test CASCADE")

        self.conn.commit()

        self.conn.close()

    def test_echo_bytea(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_bytea.sql", "rt") as file:
                cur.execute(file.read())

            data = b"\xab\xcd\xef"
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_text(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_text.sql", "rt") as file:
                cur.execute(file.read())

            data = "ABCDEF"
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_char(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_char.sql", "rt") as file:
                cur.execute(file.read())

            data = "A"
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_bool(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_bool.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT echo(%(data)s)", {"data": True})
            self.assertTrue(cur.fetchone()["echo"])

            cur.execute("SELECT echo(%(data)s)", {"data": False})
            self.assertFalse(cur.fetchone()["echo"])

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_smallint(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_smallint.sql", "rt") as file:
                cur.execute(file.read())

            data = 20488
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            data = -29268
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_int(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_int.sql", "rt") as file:
                cur.execute(file.read())

            data = 1372801355
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            data = -1042672097
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_bigint(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_bigint.sql", "rt") as file:
                cur.execute(file.read())

            data = 2263727920641201613
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            data = -591947113936367256
            cur.execute("SELECT echo(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_real(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_real.sql", "rt") as file:
                cur.execute(file.read())

            data = 42.0
            cur.execute("SELECT echo(%(data)s::real)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_echo_float(self):
        with self.conn.cursor() as cur:
            with open("sql/echo_float.sql", "rt") as file:
                cur.execute(file.read())

            data = 42.0
            cur.execute("SELECT echo(%(data)s::float)", {"data": data})
            self.assertEqual(cur.fetchone()["echo"], data)

            cur.execute("SELECT echo(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["echo"])

    def test_nan(self):
        with self.conn.cursor() as cur:
            with open("sql/nan.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT nan()")
            self.assertTrue(isnan(cur.fetchone()["nan"]))

    def test_poop(self):
        with self.conn.cursor() as cur:
            with open("sql/poop.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT poop()")
            self.assertEqual(cur.fetchone()["poop"], "💩")

    def test_shrug(self):
        with self.conn.cursor() as cur:
            with open("sql/shrug.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT shrug()")
            self.assertEqual(cur.fetchone()["shrug"], "¯\\_(ツ)_/¯")

    def test_length_bytea(self):
        with self.conn.cursor() as cur:
            with open("sql/length_bytea.sql", "rt") as file:
                cur.execute(file.read())

            data = b"\xab\xcd\xef"
            cur.execute("SELECT length_bytea(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["length_bytea"], len(data))

            cur.execute("SELECT length_bytea(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["length_bytea"])

    def test_length_text(self):
        with self.conn.cursor() as cur:
            with open("sql/length_text.sql", "rt") as file:
                cur.execute(file.read())

            data = "          "
            cur.execute("SELECT length_text(%(data)s)", {"data": data})
            self.assertEqual(cur.fetchone()["length_text"], len(data))

            cur.execute("SELECT length_text(%(data)s)", {"data": None})
            self.assertIsNone(cur.fetchone()["length_text"])

    def test_make_length_bytea(self):
        with self.conn.cursor() as cur:
            with open("sql/make_length_bytea.sql", "rt") as file:
                cur.execute(file.read())

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
        with self.conn.cursor() as cur:
            with open("sql/make_length_text.sql", "rt") as file:
                cur.execute(file.read())

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
        with self.conn.cursor() as cur:
            with open("sql/inv.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT inv(%(val)s)", {"val": True})
            self.assertFalse(cur.fetchone()["inv"])

            cur.execute("SELECT inv(%(val)s)", {"val": False})
            self.assertTrue(cur.fetchone()["inv"])

            cur.execute("SELECT inv(%(val)s)", {"val": None})
            self.assertIsNone(cur.fetchone()["inv"])

    def test_add(self):
        with self.conn.cursor() as cur:
            with open("sql/add.sql", "rt") as file:
                cur.execute(file.read())

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
