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


class TestArray(TestCase):
    def setUp(self):
        self.conn = connect(row_factory=dict_row)

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA IF EXISTS plhaskell_test CASCADE")
            cur.execute("CREATE SCHEMA plhaskell_test")
            cur.execute("SET search_path TO plhaskell_test")

            cur.execute("CREATE TYPE alpha AS (a text, b int, c float)")

        self.conn.commit()

    def tearDown(self):
        if self.conn.closed:
            self.conn = connect(row_factory=dict_row)

        self.conn.rollback()

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA plhaskell_test CASCADE")

        self.conn.commit()

        self.conn.close()

    def test_array(self):
        with self.conn.cursor() as cur:
            with open("sql/mk_array.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/echo_int_array.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/array_test.sql", "rt") as file:
                cur.execute(file.read())

    def test_array_alpha(self):
        with self.conn.cursor() as cur:
            with open("sql/alpha_func.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/echo_alpha_array.sql", "rt") as file:
                cur.execute(file.read())

            with open("sql/array_alpha_test.sql", "rt") as file:
                cur.execute(file.read())
