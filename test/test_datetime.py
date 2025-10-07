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
from time import sleep
from datetime import date, time, datetime, timedelta
from random import randrange, randint, uniform


class TestDatetime(PLHaskellTestBase):
    @staticmethod
    def random_date():
        d = date.min + timedelta(days=randrange(1_461_000))
        return d.year, d.month, d.day

    @staticmethod
    def random_time():
        return (
            randrange(24),  # hour
            randrange(60),  # mintue
            randrange(60),  # second
            randrange(1_000_000),  # microsecond
        )

    @staticmethod
    def random_interval():
        return (
            randint(-4_000, 4_000),  # years
            randint(-23, 23),  # months
            randint(-104, 104),  # weeks
            randint(-60, 60),  # days
            randint(-47, 47),  # hours
            randint(-119, 119),  # mintues
            randint(-119, 119),  # seconds
            randint(-9_999_999, 9_999_999),  # microseconds
        )

    def test_statement_timestamp(self):
        self.execute_file("sql/datetime/get_statement_timestamp.sql")

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT (get_statement_timestamp() = timezone('UTC', statement_timestamp())) compare"
            )
            assert cur.fetchone()["compare"]

    def test_transaction_timestamp(self):
        self.execute_file("sql/datetime/get_transaction_timestamp.sql")

        sleep(1)

        with self.conn.cursor() as cur:
            cur.execute(
                "SELECT (get_transaction_timestamp() = timezone('UTC', transaction_timestamp())) compare"
            )
            assert cur.fetchone()["compare"]

    def test_clock_timestamp(self):
        self.execute_file("sql/datetime/get_clock_timestamp.sql")

        with self.conn.cursor() as cur:
            sleep(1)

            cur.execute(
                "SELECT extract(seconds from (get_clock_timestamp() - timezone('UTC', clock_timestamp())));"
            )
            assert abs(cur.fetchone()["extract"]) < 0.001

    def test_mk_date(self):
        self.execute_file("sql/datetime/check_mk_date.sql")

        with self.conn.cursor() as cur:
            for _ in range(100):
                year, month, day = self.random_date()

                data = date(year, month, day)
                expect_isoyear, expect_isoweek, expect_dow = data.isocalendar()
                expect_dow %= 7
                expect_doy = int(f"{data:%j}")

                cur.execute(
                    "SELECT check_mk_date(%(year)s, %(month)s, %(day)s, %(expect_dow)s, %(expect_doy)s, %(expect_isoyear)s, %(expect_isoweek)s)",
                    {
                        "year": year,
                        "month": month,
                        "day": day,
                        "expect_dow": expect_dow,
                        "expect_doy": expect_doy,
                        "expect_isoyear": expect_isoyear,
                        "expect_isoweek": expect_isoweek,
                    },
                )
                assert cur.fetchone()["check_mk_date"]

    def test_mk_time(self):
        self.execute_file("sql/datetime/check_mk_time.sql")

        with self.conn.cursor() as cur:
            for _ in range(100):
                hour, minute, second, microsecond = self.random_time()

                cur.execute(
                    "SELECT check_mk_time(%(hour)s, %(minute)s, %(second)s, %(microsecond)s)",
                    {
                        "hour": hour,
                        "minute": minute,
                        "second": second,
                        "microsecond": microsecond,
                    },
                )
                assert cur.fetchone()["check_mk_time"]

    def test_mk_timestamp(self):
        self.execute_file("sql/datetime/check_mk_timestamp.sql")

        with self.conn.cursor() as cur:
            for _ in range(100):
                year, month, day = self.random_date()
                hour, minute, second, microsecond = self.random_time()

                data = date(year, month, day)
                expect_isoyear, expect_isoweek, expect_dow = data.isocalendar()
                expect_dow %= 7
                expect_doy = int(f"{data:%j}")

                cur.execute(
                    "SELECT check_mk_timestamp(%(year)s, %(month)s, %(day)s, %(hour)s, %(minute)s, %(second)s, %(microsecond)s, %(expect_dow)s, %(expect_doy)s, %(expect_isoyear)s, %(expect_isoweek)s)",
                    {
                        "year": year,
                        "month": month,
                        "day": day,
                        "hour": hour,
                        "minute": minute,
                        "second": second,
                        "microsecond": microsecond,
                        "expect_dow": expect_dow,
                        "expect_doy": expect_doy,
                        "expect_isoyear": expect_isoyear,
                        "expect_isoweek": expect_isoweek,
                    },
                )
                assert cur.fetchone()["check_mk_timestamp"]

    def test_mk_interval(self):
        self.execute_file("sql/datetime/check_mk_interval.sql")

        with self.conn.cursor() as cur:
            for _ in range(100):
                years, months, weeks, days, hours, minutes, seconds, microseconds = (
                    self.random_interval()
                )

                total_months = 12 * years + months
                expect_years, expect_months = divmod(abs(total_months), 12)
                if total_months < 0:
                    expect_years = -expect_years
                    expect_months = -expect_months

                expect_days = 7 * weeks + days

                total_microseconds = (
                    1_000_000 * (60 * (60 * hours + minutes) + seconds) + microseconds
                )
                q, expect_microseconds = divmod(abs(total_microseconds), 1_000_000)
                q, expect_seconds = divmod(q, 60)
                expect_hours, expect_minutes = divmod(q, 60)
                if total_microseconds < 0:
                    expect_hours = -expect_hours
                    expect_minutes = -expect_minutes
                    expect_seconds = -expect_seconds
                    expect_microseconds = -expect_microseconds

                cur.execute(
                    "SELECT check_mk_interval(%(years)s, %(months)s, %(weeks)s, %(days)s, %(hours)s, %(minutes)s, %(seconds)s, %(microseconds)s,"
                    "%(expect_years)s, %(expect_months)s, %(expect_days)s, %(expect_hours)s, %(expect_minutes)s, %(expect_seconds)s, %(expect_microseconds)s)",
                    {
                        "years": years,
                        "months": months,
                        "weeks": weeks,
                        "days": days,
                        "hours": hours,
                        "minutes": minutes,
                        "seconds": seconds,
                        "microseconds": microseconds,
                        "expect_years": expect_years,
                        "expect_months": expect_months,
                        "expect_days": expect_days,
                        "expect_hours": expect_hours,
                        "expect_minutes": expect_minutes,
                        "expect_seconds": expect_seconds,
                        "expect_microseconds": expect_microseconds,
                    },
                )
                assert cur.fetchone()["check_mk_interval"]

    def test_infinity(self):
        self.execute_file("sql/datetime/mk_inf_date.sql")
        self.execute_file("sql/datetime/mk_inf_timestamp.sql")

        with self.conn.cursor() as cur:
            cur.execute("SELECT mk_inf_date(False) = '-infinity'::date as test")
            assert cur.fetchone()["test"]

            cur.execute("SELECT mk_inf_date(True) = '+infinity'::date as test")
            assert cur.fetchone()["test"]

            cur.execute(
                "SELECT mk_inf_timestamp(False) = '-infinity'::timestamp as test"
            )
            assert cur.fetchone()["test"]

            cur.execute(
                "SELECT mk_inf_timestamp(True) = '+infinity'::timestamp as test"
            )
            assert cur.fetchone()["test"]

    def test_combine_separate(self):
        self.execute_file("sql/datetime/combine_timestamp.sql")
        self.execute_file("sql/datetime/separate_date.sql")
        self.execute_file("sql/datetime/separate_time.sql")

        with self.conn.cursor() as cur:
            for _ in range(100):
                year, month, day = self.random_date()
                hour, minute, second, microsecond = self.random_time()

                d = date(year, month, day)
                t = time(hour, minute, second, microsecond)
                ts = datetime(year, month, day, hour, minute, second, microsecond)

                cur.execute(
                    "SELECT combine_timestamp(%(date)s, %(time)s)",
                    {
                        "date": d,
                        "time": t,
                    },
                )
                self.assertEqual(cur.fetchone()["combine_timestamp"], ts)

                cur.execute("SELECT separate_date(%(timestamp)s)", {"timestamp": ts})
                self.assertEqual(cur.fetchone()["separate_date"], d)

                cur.execute("SELECT separate_time(%(timestamp)s)", {"timestamp": ts})
                self.assertEqual(cur.fetchone()["separate_time"], t)

    def test_compare_dates(self):
        self.execute_file("sql/datetime/compare_dates.sql")
        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d date)")
            cur.execute("INSERT INTO t(d) VALUES ('+infinity'), ('-infinity')")

            for _ in range(10):
                year, month, day = self.random_date()
                cur.execute(
                    "INSERT INTO t(d) VALUES (%(d)s)", {"d": date(year, month, day)}
                )

            for row in cur.execute("SELECT compare_dates(t1.d, t2.d) FROM t t1, t t2"):
                assert row["compare_dates"]

    def test_compare_times(self):
        self.execute_file("sql/datetime/compare_times.sql")
        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d time)")

            for _ in range(10):
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES (%(d)s)",
                    {"d": time(hour, minute, second, microsecond)},
                )

            for row in cur.execute("SELECT compare_times(t1.d, t2.d) FROM t t1, t t2"):
                assert row["compare_times"]

    def test_compare_timestamps(self):
        self.execute_file("sql/datetime/compare_timestamps.sql")
        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d timestamp)")
            cur.execute("INSERT INTO t(d) VALUES ('+infinity'), ('-infinity')")

            for _ in range(10):
                year, month, day = self.random_date()
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES (%(d)s)",
                    {
                        "d": datetime(
                            year, month, day, hour, minute, second, microsecond
                        )
                    },
                )

            for row in cur.execute(
                "SELECT compare_timestamps(t1.d, t2.d) FROM t t1, t t2"
            ):
                assert row["compare_timestamps"]

    def test_compare_intervals(self):
        self.execute_file("sql/datetime/compare_intervals.sql")
        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d interval)")

            for _ in range(10):
                years, months, weeks, days, hours, minutes, seconds, microseconds = (
                    self.random_interval()
                )
                cur.execute(
                    "INSERT INTO t(d) VALUES (make_interval(%(years)s, %(months)s, %(weeks)s, %(days)s, %(hours)s, %(minutes)s, %(seconds)s))",
                    {
                        "years": years,
                        "months": months,
                        "weeks": weeks,
                        "days": days,
                        "hours": hours,
                        "minutes": minutes,
                        "seconds": seconds + microseconds / 1_000_000,
                    },
                )

            for row in cur.execute(
                "SELECT compare_intervals(t1.d, t2.d) FROM t t1, t t2"
            ):
                assert row["compare_intervals"]

    def test_show_read_date(self):
        self.execute_file("sql/datetime/check_show_read_date.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d date)")
            cur.execute("INSERT INTO t(d) VALUES ('+infinity'), ('-infinity')")

            for _ in range(100):
                year, month, day = self.random_date()
                cur.execute(
                    "INSERT INTO t(d) VALUES (%(d)s)", {"d": date(year, month, day)}
                )

            for row in cur.execute("SELECT check_show_read_date(d, d::text) FROM t"):
                assert row["check_show_read_date"]

    def test_show_read_time(self):
        self.execute_file("sql/datetime/check_show_read_time.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d time)")

            for _ in range(100):
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES (%(d)s)",
                    {"d": time(hour, minute, second, microsecond)},
                )

            for row in cur.execute("SELECT check_show_read_time(d, d::text) FROM t"):
                assert row["check_show_read_time"]

    def test_show_read_timestamp(self):
        self.execute_file("sql/datetime/check_show_read_timestamp.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d timestamp)")
            cur.execute("INSERT INTO t(d) VALUES ('+infinity'), ('-infinity')")

            for _ in range(100):
                year, month, day = self.random_date()
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES (%(d)s)",
                    {
                        "d": datetime(
                            year, month, day, hour, minute, second, microsecond
                        )
                    },
                )

            for row in cur.execute(
                "SELECT check_show_read_timestamp(d, d::text) FROM t"
            ):
                assert row["check_show_read_timestamp"]

    def test_show_read_interval(self):
        self.execute_file("sql/datetime/check_show_read_interval.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d interval)")

            for _ in range(100):
                years, months, weeks, days, hours, minutes, seconds, microseconds = (
                    self.random_interval()
                )
                cur.execute(
                    "INSERT INTO t(d) VALUES (make_interval(%(years)s, %(months)s, %(weeks)s, %(days)s, %(hours)s, %(minutes)s, %(seconds)s))",
                    {
                        "years": years,
                        "months": months,
                        "weeks": weeks,
                        "days": days,
                        "hours": hours,
                        "minutes": minutes,
                        "seconds": seconds + microseconds / 1_000_000,
                    },
                )

            for row in cur.execute(
                "SELECT check_show_read_interval(d, d::text) FROM t"
            ):
                assert row["check_show_read_interval"]

    def test_check_interval_diff_time(self):
        self.execute_file("sql/datetime/check_interval_diff_time.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d time)")

            for _ in range(10):
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES(%(d)s)",
                    {
                        "d": time(hour, minute, second, microsecond),
                    },
                )

            for row in cur.execute(
                "SELECT check_interval_diff_time(t1.d, t2.d, t1.d - t2.d) FROM t t1, t t2"
            ):
                assert row["check_interval_diff_time"]

    def test_check_interval_diff_timestamp(self):
        self.execute_file("sql/datetime/check_interval_diff_timestamp.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d timestamp)")

            for _ in range(10):
                year, month, day = self.random_date()
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES(%(d)s)",
                    {
                        "d": datetime(
                            year,
                            month,
                            day,
                            hour,
                            minute,
                            second,
                            microsecond,
                        ),
                    },
                )

            for row in cur.execute(
                "SELECT check_interval_diff_timestamp(t1.d, t2.d, t1.d - t2.d) FROM t t1, t t2"
            ):
                assert row["check_interval_diff_timestamp"]

    def test_check_interval_diff_interval(self):
        self.execute_file("sql/datetime/check_interval_diff_interval.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d interval)")

            for _ in range(10):
                (
                    years,
                    months,
                    weeks,
                    days,
                    hours,
                    minutes,
                    seconds,
                    microseconds,
                ) = self.random_interval()

                cur.execute(
                    "INSERT INTO t(d) VALUES(make_interval(%(years)s, %(months)s, %(weeks)s, %(days)s, %(hours)s, %(minutes)s, %(seconds)s))",
                    {
                        "years": years,
                        "months": months,
                        "weeks": weeks,
                        "days": days,
                        "hours": hours,
                        "minutes": minutes,
                        "seconds": seconds + microseconds / 1_000_000,
                    },
                )

            for row in cur.execute(
                "SELECT check_interval_diff_interval(t1.d, t2.d, t1.d - t2.d) FROM t t1, t t2"
            ):
                assert row["check_interval_diff_interval"]

    def test_check_arith_date(self):
        self.execute_file("sql/datetime/check_arith_date.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d date)")

            for _ in range(10):
                year, month, day = self.random_date()
                cur.execute(
                    "INSERT INTO t(d) VALUES(%(d)s)",
                    {
                        "d": date(
                            year,
                            month,
                            day,
                        ),
                    },
                )

            for row in cur.execute(
                "SELECT check_arith_date(t1.d, t2.d, t1.d - t2.d) FROM t t1, t t2"
            ):
                assert row["check_arith_date"]

    def test_double_times_interval(self):
        self.execute_file("sql/datetime/double_times_interval.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t1 (d float)")
            cur.execute("CREATE TABLE t2 (d interval)")

            cur.execute("INSERT INTO t1(d) VALUES(0.0)")
            cur.execute("INSERT INTO t2(d) VALUES('0')")

            for _ in range(10):
                cur.execute(
                    "INSERT INTO t1(d) VALUES(%(d)s)", {"d": uniform(-10.0, 10.0)}
                )

                (
                    years,
                    months,
                    weeks,
                    days,
                    hours,
                    minutes,
                    seconds,
                    microseconds,
                ) = self.random_interval()

                cur.execute(
                    "INSERT INTO t2(d) VALUES(make_interval(%(years)s, %(months)s, %(weeks)s, %(days)s, %(hours)s, %(minutes)s, %(seconds)s))",
                    {
                        "years": years,
                        "months": months,
                        "weeks": weeks,
                        "days": days,
                        "hours": hours,
                        "minutes": minutes,
                        "seconds": seconds + microseconds / 1_000_000,
                    },
                )

            for row in cur.execute(
                "SELECT t1.d * t2.d = double_times_interval(t1.d, t2.d) check FROM t1, t2"
            ):
                assert row["check"]

    def test_date_plus_minus_interval(self):
        self.execute_file("sql/datetime/date_plus_interval.sql")
        self.execute_file("sql/datetime/date_minus_interval.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t1 (d date)")
            cur.execute("CREATE TABLE t2 (d interval)")

            cur.execute("INSERT INTO t1(d) VALUES('-infinity'), ('+infinity')")

            for _ in range(10):
                year, month, day = self.random_date()
                cur.execute(
                    "INSERT INTO t1(d) VALUES (%(d)s)",
                    {"d": date(year, month, day)},
                )
                (
                    years,
                    months,
                    weeks,
                    days,
                    hours,
                    minutes,
                    seconds,
                    microseconds,
                ) = self.random_interval()

                cur.execute(
                    "INSERT INTO t2(d) VALUES(make_interval(%(years)s, %(months)s, %(weeks)s, %(days)s, %(hours)s, %(minutes)s, %(seconds)s))",
                    {
                        "years": years,
                        "months": months,
                        "weeks": weeks,
                        "days": days,
                        "hours": hours,
                        "minutes": minutes,
                        "seconds": seconds + microseconds / 1_000_000,
                    },
                )

            for row in cur.execute(
                "SELECT t1.d + t2.d = date_plus_interval(t1.d, t2.d) check FROM t1, t2"
            ):
                assert row["check"]

            for row in cur.execute(
                "SELECT t1.d - t2.d = date_minus_interval(t1.d, t2.d) check FROM t1, t2"
            ):
                assert row["check"]

    def test_query_date(self):
        self.execute_file("sql/datetime/query_date.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d date)")
            cur.execute("INSERT INTO t(d) VALUES ('+infinity'), ('-infinity')")

            for _ in range(100):
                year, month, day = self.random_date()
                cur.execute(
                    "INSERT INTO t(d) VALUES(%(d)s)",
                    {
                        "d": date(
                            year,
                            month,
                            day,
                        ),
                    },
                )

            for row in cur.execute("SELECT d = query_date(d) check FROM t"):
                assert row["check"]

    def test_query_time(self):
        self.execute_file("sql/datetime/query_time.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d time)")

            for _ in range(100):
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES(%(d)s)",
                    {
                        "d": time(hour, minute, second, microsecond),
                    },
                )

            for row in cur.execute("SELECT d = query_time(d) check FROM t"):
                assert row["check"]

    def test_query_timestamp(self):
        self.execute_file("sql/datetime/query_timestamp.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d timestamp)")

            for _ in range(100):
                year, month, day = self.random_date()
                hour, minute, second, microsecond = self.random_time()
                cur.execute(
                    "INSERT INTO t(d) VALUES(%(d)s)",
                    {
                        "d": datetime(
                            year, month, day, hour, minute, second, microsecond
                        ),
                    },
                )

            for row in cur.execute("SELECT d = query_timestamp(d) check FROM t"):
                assert row["check"]

    def test_query_interval(self):
        self.execute_file("sql/datetime/query_interval.sql")

        with self.conn.cursor() as cur:
            cur.execute("CREATE TABLE t (d interval)")

            for _ in range(100):
                (
                    years,
                    months,
                    weeks,
                    days,
                    hours,
                    minutes,
                    seconds,
                    microseconds,
                ) = self.random_interval()

                cur.execute(
                    "INSERT INTO t(d) VALUES(make_interval(%(years)s, %(months)s, %(weeks)s, %(days)s, %(hours)s, %(minutes)s, %(seconds)s))",
                    {
                        "years": years,
                        "months": months,
                        "weeks": weeks,
                        "days": days,
                        "hours": hours,
                        "minutes": minutes,
                        "seconds": seconds + microseconds / 1_000_000,
                    },
                )

            for row in cur.execute("SELECT d = query_interval(d) check FROM t"):
                assert row["check"]
