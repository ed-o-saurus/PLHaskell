# This is a "procedural language" extension of PostgreSQL
# allowing the execution of code in Haskell within SQL code.
#
# Copyright (C) 2026 Edward F. Behn, Jr.
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


class TestQuery(PLHaskellTestBase):
    @staticmethod
    def type_setup(cur):
        cur.execute("CREATE TYPE alpha AS (a text, b int, c float)")
        cur.execute("CREATE TYPE bravo AS (d alpha, e int)")
        cur.execute("CREATE TYPE charlie AS ()")
        cur.execute("CREATE TYPE delta AS (f bravo, g charlie)")
        cur.execute("CREATE TYPE n_p AS (n int, p int)")

    def test_query(self):
        self.execute_file("sql/query/query_create.sql")
        self.execute_file("sql/query/query_insert.sql")
        self.execute_file("sql/query/query_insert_returning.sql")
        self.execute_file("sql/query/query_select.sql")
        self.execute_file("sql/query/query_delete.sql")
        self.execute_file("sql/query/query_drop.sql")

        with self.conn.cursor() as cur:
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
        self.execute_file("sql/query/query_composite.sql")
        self.execute_file("sql/query/query_pass_composite.sql")

        with self.conn.cursor() as cur:
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
        self.execute_file("sql/query/query_array_insert.sql")
        self.execute_file("sql/query/query_array_select.sql")
        self.execute_file("sql/common/mk_array.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE query_arrays(a int[])")

            cur.execute("SELECT query_array_insert()")

        self.execute_file("sql/query/query_array_select_test.sql")
