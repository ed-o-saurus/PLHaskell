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

from random import seed
from unittest import TestCase
from psycopg import connect
from psycopg.rows import dict_row


class PLHaskellTestBase(TestCase):
    def get_notice(self, diag):
        self.notices.append((diag.severity, diag.message_primary))

    def setUp(self):
        seed(self.id())

        self.conn = connect(row_factory=dict_row)

        with self.conn.cursor() as cur:
            cur.execute("SET lc_messages TO 'C'")
            cur.execute("DROP SCHEMA IF EXISTS plhaskell_test CASCADE")
            cur.execute("CREATE SCHEMA plhaskell_test")
            cur.execute("SET search_path TO plhaskell_test")

            self.type_setup(cur)

        self.conn.commit()

        self.notices = []
        self.conn.add_notice_handler(self.get_notice)

    def tearDown(self):
        if self.conn.closed:
            self.conn = connect(row_factory=dict_row)

        self.conn.rollback()

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA plhaskell_test CASCADE")

        self.conn.commit()

        self.conn.close()

    @staticmethod
    def type_setup(_cur):
        pass

    def execute_file(self, file):
        with self.conn.cursor() as cur:
            with open(file, "rt") as file:
                cur.execute(file.read())
