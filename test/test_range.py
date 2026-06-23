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
from datetime import date, datetime, timedelta
from random import randrange
from collections import defaultdict
from psycopg.types.range import Range
from psycopg.types.multirange import Multirange


class TestRange(PLHaskellTestBase):
    @staticmethod
    def random_range(factory):
        range_type = randrange(10)

        if range_type == 0:
            return Range(empty=True)
        elif range_type == 1:
            return Range(lower=None, upper=None, bounds="()")
        elif range_type == 2:
            return Range(lower=factory(), upper=None, bounds="()")
        elif range_type == 3:
            return Range(lower=factory(), upper=None, bounds="[)")
        elif range_type == 4:
            return Range(lower=None, upper=factory(), bounds="()")
        elif range_type == 5:
            lower, upper = sorted(factory() for _ in range(2))
            return Range(lower=lower, upper=upper, bounds="()")
        elif range_type == 6:
            lower, upper = sorted(factory() for _ in range(2))
            return Range(lower=lower, upper=upper, bounds="[)")
        elif range_type == 7:
            return Range(lower=None, upper=factory(), bounds="(]")
        elif range_type == 8:
            lower, upper = sorted(factory() for _ in range(2))
            return Range(lower=lower, upper=upper, bounds="(]")
        elif range_type == 9:
            lower, upper = sorted(factory() for _ in range(2))
            return Range(lower=lower, upper=upper, bounds="[]")

    @classmethod
    def random_multirange(cls, factory, count=3):
        return Multirange([cls.random_range(factory) for _ in range(count)])

    @staticmethod
    def random_date():
        return date.min + timedelta(days=randrange(1_461_000))

    @staticmethod
    def random_datetime():
        d = date.min + timedelta(days=randrange(1_461_000))
        return datetime(
            d.year,
            d.month,
            d.day,
            randrange(24),
            randrange(60),
            randrange(60),
            randrange(1_000_000),
        )

    def test_echo_int4range(self):
        self.execute_file("sql/range/echo_int4range.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (r int4range)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(r) VALUES(%(r)s)",
                    {"r": self.random_range(lambda: randrange(-(2**31), 2**31))},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE r != echo(r)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_echo_int8range(self):
        self.execute_file("sql/range/echo_int8range.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (r int8range)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(r) VALUES(%(r)s)",
                    {"r": self.random_range(lambda: randrange(-(2**63), 2**63))},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE r != echo(r)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_echo_daterange(self):
        self.execute_file("sql/range/echo_daterange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (r daterange)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(r) VALUES(%(r)s)",
                    {"r": self.random_range(self.random_date)},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE r != echo(r)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_echo_tsrange(self):
        self.execute_file("sql/range/echo_tsrange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (r tsrange)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(r) VALUES(%(r)s)",
                    {"r": self.random_range(self.random_datetime)},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE r != echo(r)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_properties(self):
        self.execute_file("sql/range/haskell_lower.sql")
        self.execute_file("sql/range/haskell_upper.sql")
        self.execute_file("sql/range/haskell_isempty.sql")
        self.execute_file("sql/range/haskell_lower_inc.sql")
        self.execute_file("sql/range/haskell_upper_inc.sql")
        self.execute_file("sql/range/haskell_lower_inf.sql")
        self.execute_file("sql/range/haskell_upper_inf.sql")
        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (r tsrange)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(r) VALUES(%(r)s)",
                    {"r": self.random_range(self.random_datetime)},
                )

            cur.execute(
                """SELECT lower(r), haskell_lower(r),
                          upper(r), haskell_upper(r),
                          isempty(r), haskell_isempty(r),
                          lower_inc(r), haskell_lower_inc(r),
                          upper_inc(r), haskell_upper_inc(r),
                          lower_inf(r), haskell_lower_inf(r),
                          upper_inf(r), haskell_upper_inf(r)
                   FROM t"""
            )

            for row in cur:
                assert row["lower"] == row["haskell_lower"]
                assert row["upper"] == row["haskell_upper"]
                assert row["isempty"] == row["haskell_isempty"]
                assert row["lower_inc"] == row["haskell_lower_inc"]
                assert row["upper_inc"] == row["haskell_upper_inc"]
                assert row["lower_inf"] == row["haskell_lower_inf"]
                assert row["upper_inf"] == row["haskell_upper_inf"]

    def test_echo_int4multirange(self):
        self.execute_file("sql/range/echo_int4multirange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (mr int4multirange)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(mr) VALUES(%(mr)s)",
                    {"mr": self.random_multirange(lambda: randrange(-(2**31), 2**31))},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE mr != echo(mr)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_echo_int8multirange(self):
        self.execute_file("sql/range/echo_int8multirange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (mr int8multirange)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(mr) VALUES(%(mr)s)",
                    {"mr": self.random_multirange(lambda: randrange(-(2**63), 2**63))},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE mr != echo(mr)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_echo_datemultirange(self):
        self.execute_file("sql/range/echo_datemultirange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (mr datemultirange)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(mr) VALUES(%(mr)s)",
                    {"mr": self.random_multirange(self.random_date)},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE mr != echo(mr)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_echo_tsmultirange(self):
        self.execute_file("sql/range/echo_tsmultirange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (mr tsmultirange)")

            for _ in range(1000):
                cur.execute(
                    "INSERT INTO t(mr) VALUES(%(mr)s)",
                    {"mr": self.random_multirange(self.random_datetime)},
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE mr != echo(mr)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_combine_tsranges(self):
        self.execute_file("sql/range/combine_tsranges.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (r1 tsrange, r2 tsrange, r3 tsrange)")

            for _ in range(1000):
                cur.execute(
                    """INSERT INTO t(r1, r2, r3)
                       VALUES(%(r1)s, %(r2)s, %(r3)s)""",
                    {
                        "r1": self.random_range(self.random_datetime),
                        "r2": self.random_range(self.random_datetime),
                        "r3": self.random_range(self.random_datetime),
                    },
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE tsmultirange(r1, r2, r3) != combine_tsranges(r1, r2, r3)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_unnest_tsranges(self):
        self.execute_file("sql/range/unnest_tsranges.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (i int, mr tsmultirange)")

            for i in range(1000):
                cur.execute(
                    "INSERT INTO t(i, mr) VALUES(%(i)s, %(mr)s)",
                    {
                        "i": i,
                        "mr": self.random_multirange(self.random_datetime, count=3),
                    },
                )

            cur.execute(
                """SELECT i, r
                   FROM t, unnest_tsranges(mr) AS r"""
            )

            d1 = defaultdict(set)
            for row in cur:
                d1[row["i"]].add(row["r"])

            cur.execute(
                """SELECT i, r
                   FROM t, unnest(mr) AS r"""
            )

            d2 = defaultdict(set)
            for row in cur:
                d2[row["i"]].add(row["r"])

        for i in range(1000):
            assert d1[i] == d2[i]

    def test_query_retrieve_tsrange(self):
        self.execute_file("sql/range/retrieve_tsrange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (i int, r tsrange)")

            for i in range(1000):
                cur.execute(
                    "INSERT INTO t(i, r) VALUES(%(i)s, %(r)s)",
                    {
                        "i": i,
                        "r": self.random_range(self.random_datetime),
                    },
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE r != retrieve_tsrange(i)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_query_retrieve_tsmultirange(self):
        self.execute_file("sql/range/retrieve_tsmultirange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (i int, mr tsmultirange)")

            for i in range(1000):
                cur.execute(
                    "INSERT INTO t(i, mr) VALUES(%(i)s, %(mr)s)",
                    {
                        "i": i,
                        "mr": self.random_multirange(self.random_datetime),
                    },
                )

            cur.execute(
                """SELECT count(*)
                   FROM t
                   WHERE mr != retrieve_tsmultirange(i)"""
            )

            assert cur.fetchone()["count"] == 0

    def test_query_insert_tsrange(self):
        self.execute_file("sql/range/insert_tsrange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t1 (i int, r tsrange)")
            cur.execute("CREATE TABLE t2 (i int, r tsrange)")

            d = {}
            for i in range(1000):
                cur.execute(
                    "INSERT INTO t1(i, r) VALUES(%(i)s, %(r)s)",
                    {
                        "i": i,
                        "r": self.random_range(self.random_datetime),
                    },
                )

            cur.execute(
                """SELECT insert_tsrange(i, r)
                   FROM t1"""
            )

            cur.execute(
                """SELECT count(*)
                   FROM t1 JOIN t2 USING(i)
                   WHERE t1.r != t2.r"""
            )

            assert cur.fetchone()["count"] == 0

    def test_query_insert_tsmultirange(self):
        self.execute_file("sql/range/insert_tsmultirange.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t1 (i int, mr tsmultirange)")
            cur.execute("CREATE TABLE t2 (i int, mr tsmultirange)")

            d = {}
            for i in range(1000):
                cur.execute(
                    "INSERT INTO t1(i, mr) VALUES(%(i)s, %(mr)s)",
                    {
                        "i": i,
                        "mr": self.random_multirange(self.random_datetime),
                    },
                )

            cur.execute(
                """SELECT insert_tsmultirange(i, mr)
                   FROM t1"""
            )

            cur.execute(
                """SELECT count(*)
                   FROM t1 JOIN t2 USING(i)
                   WHERE t1.mr != t2.mr"""
            )

            assert cur.fetchone()["count"] == 0
