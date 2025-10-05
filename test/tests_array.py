# This is a "procedural language" extension of PostgreSQL
# allowing the execution of code in Haskell within SQL code.
#
# Copyright (C) 2025 Edward F. Behn, Jr.
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


class TestsArray(PLHaskellTestBase):
    @staticmethod
    def type_setup(cur):
        cur.execute("CREATE TYPE alpha AS (a text, b int, c float)")

    def test_array(self):
        self.execute_file("sql/common/mk_array.sql")
        self.execute_file("sql/array/echo_int_array.sql")
        self.execute_file("sql/array/array_test.sql")

    def test_array_alpha(self):
        self.execute_file("sql/common/alpha_func.sql")
        self.execute_file("sql/array/echo_alpha_array.sql")
        self.execute_file("sql/array/array_alpha_test.sql")
