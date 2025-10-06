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

from psycopg.errors import InternalError, OperationalError
from plhaskell_test_base import PLHaskellTestBase


class TestMsgs(PLHaskellTestBase):
    def test_fatal(self):
        self.execute_file("sql/msgs/msg_fatal.sql")

        with self.conn.cursor() as cur:
            with self.assertRaisesRegex(OperationalError, "Test"):
                cur.execute("SELECT msg_fatal();")

    def test_exception(self):
        self.execute_file("sql/msgs/msg_exception.sql")

        with self.conn.cursor() as cur:
            with self.assertRaisesRegex(InternalError, "Test"):
                cur.execute("SELECT msg_exception();")

    def test_warning(self):
        self.execute_file("sql/msgs/msg_warning.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT msg_warning();")

            self.assertEqual(len(self.notices), 1)

            self.assertEqual(self.notices[0], ("WARNING", "Test"))

    def test_notice(self):
        self.execute_file("sql/msgs/msg_notice.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT msg_notice();")

            self.assertEqual(len(self.notices), 1)

            self.assertEqual(self.notices[0], ("NOTICE", "Test"))

    def test_info(self):
        self.execute_file("sql/msgs/msg_info.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT msg_info();")

            self.assertEqual(len(self.notices), 1)

            self.assertEqual(self.notices[0], ("INFO", "Test"))

    def test_log(self):
        self.execute_file("sql/msgs/msg_log.sql")

        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = LOG")

            cur.execute("SELECT msg_log();")

            self.assertIn(("LOG", "Test"), self.notices)

    def test_debug1(self):
        self.execute_file("sql/msgs/msg_debug1.sql")

        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            cur.execute("SELECT msg_debug1();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug2(self):
        self.execute_file("sql/msgs/msg_debug2.sql")

        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            cur.execute("SELECT msg_debug2();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug3(self):
        self.execute_file("sql/msgs/msg_debug3.sql")

        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            cur.execute("SELECT msg_debug3();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug4(self):
        self.execute_file("sql/msgs/msg_debug4.sql")

        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            cur.execute("SELECT msg_debug4();")

            self.assertIn(("DEBUG", "Test"), self.notices)

    def test_debug5(self):
        self.execute_file("sql/msgs/msg_debug5.sql")

        with self.conn.cursor() as cur:
            cur.execute("SET client_min_messages = DEBUG5")

            cur.execute("SELECT msg_debug5();")

            self.assertIn(("DEBUG", "Test"), self.notices)
