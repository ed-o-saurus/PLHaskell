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


class TestInline(PLHaskellTestBase):
    @staticmethod
    def type_setup(cur):
        cur.execute("CREATE TABLE inline_test(i int, i_sq int)")

    def test_inline(self):
        self.execute_file("sql/inline/inline.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT i, i_sq FROM inline_test")

            row = cur.fetchone()
            self.assertEqual(row["i"], 0)
            self.assertEqual(row["i_sq"], 0)

            row = cur.fetchone()
            self.assertEqual(row["i"], 1)
            self.assertEqual(row["i_sq"], 1)

            row = cur.fetchone()
            self.assertEqual(row["i"], 2)
            self.assertEqual(row["i_sq"], 4)

            row = cur.fetchone()
            self.assertEqual(row["i"], 3)
            self.assertEqual(row["i_sq"], 9)

            row = cur.fetchone()
            self.assertEqual(row["i"], 4)
            self.assertEqual(row["i_sq"], 16)

            row = cur.fetchone()
            self.assertEqual(row["i"], 5)
            self.assertEqual(row["i_sq"], 25)

            self.assertIsNone(cur.fetchone())
