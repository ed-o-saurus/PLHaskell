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

from unittest import TestCase, skip
from psycopg import connect
from psycopg.rows import dict_row
from psycopg.errors import InternalError


class TestMsgs(TestCase):
    def get_notice(self, diag):
        self.notices.append((diag.severity, diag.message_primary))

    def setUp(self):
        self.conn = connect(row_factory=dict_row)

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA IF EXISTS plhaskell_test CASCADE")
            cur.execute("CREATE SCHEMA plhaskell_test")
            cur.execute("SET search_path TO plhaskell_test")

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

    def test_fatal(self):
        with self.conn.cursor() as cur:
            with open("sql/msg_fatal.sql", "rt") as file:
                cur.execute(file.read())

            with self.assertRaisesRegex(InternalError, "Test"):
                cur.execute("SELECT msg_fatal();")

    def test_exception(self):
        with self.conn.cursor() as cur:
            with open("sql/msg_exception.sql", "rt") as file:
                cur.execute(file.read())

            with self.assertRaisesRegex(InternalError, "Test"):
                cur.execute("SELECT msg_exception();")

    def test_warning(self):
        with self.conn.cursor() as cur:
            with open("sql/msg_warning.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_warning();")

            self.assertEqual(len(self.notices), 1)

            self.assertEqual(self.notices[0][0], "WARNING")
            self.assertEqual(self.notices[0][1], "Test")

    def test_notice(self):
        with self.conn.cursor() as cur:
            with open("sql/msg_notice.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_notice();")

            self.assertEqual(len(self.notices), 1)

            self.assertEqual(self.notices[0][0], "NOTICE")
            self.assertEqual(self.notices[0][1], "Test")

    def test_info(self):
        with self.conn.cursor() as cur:
            with open("sql/msg_info.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_info();")

            self.assertEqual(len(self.notices), 1)

            self.assertEqual(self.notices[0][0], "INFO")
            self.assertEqual(self.notices[0][1], "Test")

    def test_log(self):
        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = LOG")

            with open("sql/msg_log.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_log();")

            self.assertIn(("LOG", "Test"), self.notices)

    def test_debug1(self):
        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            with open("sql/msg_debug1.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_debug1();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug2(self):
        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            with open("sql/msg_debug2.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_debug2();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug3(self):
        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            with open("sql/msg_debug3.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_debug3();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug4(self):
        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            with open("sql/msg_debug4.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_debug4();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug5(self):
        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            with open("sql/msg_debug5.sql", "rt") as file:
                cur.execute(file.read())

            cur.execute("SELECT msg_debug5();")

            self.assertIn(("DEBUG", "Test"), self.notices)
