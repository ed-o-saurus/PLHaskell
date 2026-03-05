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
from psycopg import connect
from random import randrange


class TestLock(PLHaskellTestBase):
    def mk_keys_dict(self):
        return {
            "key": randrange(-(2**63), 2**63),
            "key1": randrange(-(2**31), 2**31),
            "key2": randrange(-(2**31), 2**31),
        }

    def test_lock_1key_Shared_Transaction(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT lock_test('S', 'T', %(key)s::bigint)", key_dict)

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_lock_2key_Shared_Transaction(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT lock_test('S', 'T', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_lock_1key_Exclusive_Transaction(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT lock_test('X', 'T', %(key)s::bigint)", key_dict)

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_lock_2key_Exclusive_Transaction(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT lock_test('X', 'T', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_lock_1key_Shared_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT lock_test('S', 'S', %(key)s::bigint)", key_dict)

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_lock_2key_Shared_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT lock_test('S', 'S', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_lock_1key_Exclusive_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT lock_test('X', 'S', %(key)s::bigint)", key_dict)

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_lock_2key_Exclusive_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT lock_test('X', 'S', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_unlock_1key_Shared_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT pg_advisory_lock_shared(%(key)s::bigint)", key_dict)

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'"""
            )

            assert cur.fetchone()["count"] == 1

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('S', %(key)s)",
                key_dict,
            )

            assert cur.fetchone()["unlock_test"]

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_unlock_2key_Shared_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT pg_advisory_lock_shared(%(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'""",
            )

            assert cur.fetchone()["count"] == 1

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('S', %(key1)s, %(key2)s)",
                key_dict,
            )

            assert cur.fetchone()["unlock_test"]

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_unlock_1key_Exclusive_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT pg_advisory_lock(%(key)s::bigint)", key_dict)

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'"""
            )

            assert cur.fetchone()["count"] == 1

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('X', %(key)s)",
                key_dict,
            )

            assert cur.fetchone()["unlock_test"]

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_unlock_2key_Exclusive_Session(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT pg_advisory_lock(%(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'""",
            )

            assert cur.fetchone()["count"] == 1

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('X', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            assert cur.fetchone()["unlock_test"]

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    ###############################

    def test_unlock_1key_Shared_Session_fail(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('S', %(key)s)",
                key_dict,
            )

            assert not cur.fetchone()["unlock_test"]

    def test_unlock_2key_Shared_Session_fail(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('S', %(key1)s, %(key2)s)",
                key_dict,
            )

            assert not cur.fetchone()["unlock_test"]

    def test_unlock_1key_Exclusive_Session_fail(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('X', %(key)s)",
                key_dict,
            )

            assert not cur.fetchone()["unlock_test"]

    def test_unlock_2key_Exclusive_Session_fail(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/unlock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT unlock_test('X', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            assert not cur.fetchone()["unlock_test"]

    #####################

    def test_try_lock_1key_Shared_Transaction_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT try_lock_test('S', 'T', %(key)s::bigint)", key_dict)

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_try_lock_2key_Shared_Transaction_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT try_lock_test('S', 'T', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_try_lock_1key_Exclusive_Transaction_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT try_lock_test('X', 'T', %(key)s::bigint)", key_dict)

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_try_lock_2key_Exclusive_Transaction_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT try_lock_test('X', 'T', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

    def test_try_lock_1key_Shared_Session_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT try_lock_test('S', 'S', %(key)s::bigint)", key_dict)

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_try_lock_2key_Shared_Session_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT try_lock_test('S', 'S', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_try_lock_1key_Exclusive_Session_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test1.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 0

            cur.execute("SELECT try_lock_test('X', 'S', %(key)s::bigint)", key_dict)

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'"""
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_try_lock_2key_Exclusive_Session_true(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test2.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()""",
            )

            assert cur.fetchone()["count"] == 0

            cur.execute(
                "SELECT try_lock_test('X', 'S', %(key1)s::int, %(key2)s::int)",
                key_dict,
            )

            assert cur.fetchone()["try_lock_test"]

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'""",
            )

            assert cur.fetchone()["count"] == 1

        self.conn.commit()

        with self.conn.cursor() as cur:
            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
            )

            assert cur.fetchone()["count"] == 1

    def test_try_lock_1key_Shared_Transaction_false(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test1.sql")

        with connect() as conn_alt:
            with conn_alt.cursor() as cur:
                cur.execute("SELECT pg_advisory_lock(%(key)s)", key_dict)

            with self.conn.cursor() as cur:
                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
                )

                assert cur.fetchone()["count"] == 0

                cur.execute("SELECT try_lock_test('S', 'T', %(key)s::bigint)", key_dict)

                assert not cur.fetchone()["try_lock_test"]

                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'"""
                )

                assert cur.fetchone()["count"] == 0

    def test_try_lock_2key_Shared_Transaction_false(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test2.sql")

        with connect() as conn_alt:
            with conn_alt.cursor() as cur:
                cur.execute("SELECT pg_advisory_lock(%(key1)s, %(key2)s)", key_dict)

            with self.conn.cursor() as cur:
                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
                )

                assert cur.fetchone()["count"] == 0

                cur.execute(
                    "SELECT try_lock_test('S', 'T', %(key1)s::int, %(key2)s::int)",
                    key_dict,
                )

                assert not cur.fetchone()["try_lock_test"]

                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ShareLock'"""
                )

                assert cur.fetchone()["count"] == 0

    def test_try_lock_1key_Exclusive_Transaction_false(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test1.sql")

        with connect() as conn_alt:
            with conn_alt.cursor() as cur:
                cur.execute("SELECT pg_advisory_lock_shared(%(key)s)", key_dict)

            with self.conn.cursor() as cur:
                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
                )

                assert cur.fetchone()["count"] == 0

                cur.execute("SELECT try_lock_test('X', 'T', %(key)s::bigint)", key_dict)

                assert not cur.fetchone()["try_lock_test"]

                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'"""
                )

                assert cur.fetchone()["count"] == 0

    def test_try_lock_2key_Exclusive_Transaction_false(self):
        key_dict = self.mk_keys_dict()
        self.execute_file("sql/lock/try_lock_test2.sql")

        with connect() as conn_alt:
            with conn_alt.cursor() as cur:
                cur.execute(
                    "SELECT pg_advisory_lock_shared(%(key1)s, %(key2)s)", key_dict
                )

            with self.conn.cursor() as cur:
                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database()"""
                )

                assert cur.fetchone()["count"] == 0

                cur.execute(
                    "SELECT try_lock_test('X', 'T', %(key1)s::int, %(key2)s::int)",
                    key_dict,
                )

                assert not cur.fetchone()["try_lock_test"]

                cur.execute(
                    """SELECT count(*)
                    FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                    WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'ExclusiveLock'"""
                )

                assert cur.fetchone()["count"] == 0

    def test_unlock_all(self):
        self.execute_file("sql/lock/unlock_all_test.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT pg_advisory_lock(%(key)s)", self.mk_keys_dict())
            cur.execute(
                "SELECT pg_advisory_lock(%(key1)s, %(key2)s)", self.mk_keys_dict()
            )
            cur.execute("SELECT pg_advisory_lock_shared(%(key)s)", self.mk_keys_dict())
            cur.execute(
                "SELECT pg_advisory_lock_shared(%(key1)s, %(key2)s)",
                self.mk_keys_dict(),
            )

            cur.execute("SELECT unlock_all_test()")

            cur.execute(
                """SELECT count(*)
                   FROM pg_locks JOIN pg_database ON (pg_locks.database = pg_database.oid)
                   WHERE locktype = 'advisory' AND pid = pg_backend_pid() AND datname = current_database() AND mode = 'SessionLock'"""
            )

            assert cur.fetchone()["count"] == 0
