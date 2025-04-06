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


class TestQuery(TestCase):
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

    def test_query(self):
        with self.conn.cursor() as cur:
            with open("sql/query_create.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/query_insert.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/query_insert_returning.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/query_select.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/query_delete.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/query_drop.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT query_create()")

            cur.execute("SELECT query_insert(%(i)s, %(l)s)", {"i": None, "l": None})
            cur.execute("SELECT query_insert(%(i)s, %(l)s)", {"i": 1, "l": "A"})
            cur.execute("SELECT query_insert(%(i)s, %(l)s)", {"i": 2, "l": "B"})
            cur.execute("SELECT query_insert(%(i)s, %(l)s)", {"i": 3, "l": "C"})

            cur.execute("SELECT query_insert_returning()")

            cur.execute("SELECT query_select()")

            cur.execute("SELECT query_delete()")

            cur.execute("SELECT query_drop()")

    def test_query_composite(self):
        with self.conn.cursor() as cur:
            with open("sql/query_composite.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/query_pass_composite.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("CREATE TABLE deltas(i int, d delta)")
            cur.execute(
                "INSERT INTO deltas(i, d) VALUES (1, ((('Hello', 12, 3.4)::alpha, 76)::bravo, '()'::charlie)::delta)"
            )
            cur.execute(
                "INSERT INTO deltas(i, d) VALUES (2, ((('world', 42, 1.0)::alpha, -12)::bravo, NULL::charlie)::delta)"
            )
            cur.execute(
                "INSERT INTO deltas(i, d) VALUES (3, (NULL::bravo, '()'::charlie)::delta)"
            )
            cur.execute("INSERT INTO deltas(i, d) VALUES (4, NULL::delta)")

            cur.execute("SELECT query_composite()")

            cur.execute("SELECT query_pass_composite()")

    def test_query_array(self):
        with self.conn.cursor() as cur:
            with open("sql/query_array_insert.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/query_array_select.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/mk_array.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("CREATE TABLE query_arrays(a int[])")

            cur.execute("SELECT query_array_insert()")

            with open("sql/query_array_select_test.sql", "rt") as file:
                cur.execute(file.read())
