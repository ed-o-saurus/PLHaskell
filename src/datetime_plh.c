// This is a "procedural language" extension of PostgreSQL
// allowing the execution of code in Haskell within SQL code.
//
// Copyright (C) 2026 Edward F. Behn, Jr.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include "datetime_plh.h"

#define USE_ISO_DATES 1
#define INTSTYLE_POSTGRES 0

bool date_read(DateADT *date, char *buf) __attribute__((visibility("hidden")));
bool date_read(DateADT *date, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int tzp;
  int dtype;
  int nf;
  int dterr;
  char *field[MAXDATEFIELDS];
  int ftype[MAXDATEFIELDS];
  char workbuf[MAXDATELEN + 1];
  DateTimeErrorExtra extra;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);
  if (dterr == 0)
    dterr = DecodeDateTime(field, ftype, nf, &dtype, tm, &fsec, &tzp, &extra);

  if (dterr != 0)
    return false;

  switch (dtype) {
  case DTK_DATE:
    break;

  case DTK_EPOCH:
    GetEpochTime(tm);
    break;

  case DTK_LATE:
    DATE_NOEND(*date);
    return true;

  case DTK_EARLY:
    DATE_NOBEGIN(*date);
    return true;

  default:
    return false;
  }

  /* Prevent overflow in Julian-day routines */
  if (!IS_VALID_JULIAN(tm->tm_year, tm->tm_mon, tm->tm_mday))
    return false;

  *date = date2j(tm->tm_year, tm->tm_mon, tm->tm_mday) - POSTGRES_EPOCH_JDATE;

  /* Now check for just-out-of-range dates */
  if (!IS_VALID_DATE(*date))
    return false;

  return true;
}

void date_show(DateADT date, char *buf) __attribute__((visibility("hidden")));
void date_show(DateADT date, char *buf) {
  struct pg_tm tt, *tm = &tt;

  j2date(date + POSTGRES_EPOCH_JDATE, &(tm->tm_year), &(tm->tm_mon),
         &(tm->tm_mday));

  EncodeDateOnly(tm, USE_ISO_DATES, buf);
}

bool time_read(TimeADT *time, char *buf) __attribute__((visibility("hidden")));
bool time_read(TimeADT *time, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int nf;
  int dterr;
  char workbuf[MAXDATELEN + 1];
  char *field[MAXDATEFIELDS];
  int dtype;
  int ftype[MAXDATEFIELDS];
  DateTimeErrorExtra extra;
  bool has_tz = false;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);

  if (dterr == 0)
    dterr = DecodeTimeOnly(field, ftype, nf, &dtype, tm, &fsec, NULL, &extra);
  if (dterr != 0)
    return false;

  for (int i = 0; i < nf; i++)
    if (ftype[i] == DTK_TZ) {
      has_tz = true;
      break;
    }

  if (has_tz)
    return false;

  tm2time(tm, fsec, time);

  return true;
}

void time_show(TimeADT time, char *buf) __attribute__((visibility("hidden")));
void time_show(TimeADT time, char *buf) {
  struct pg_tm tt, *tm = &tt;
  fsec_t fsec;

  time2tm(time, tm, &fsec);
  EncodeTimeOnly(tm, fsec, false, 0, USE_ISO_DATES, buf);
}

bool timestamp_read(Timestamp *timestamp, char *buf)
    __attribute__((visibility("hidden")));
bool timestamp_read(Timestamp *timestamp, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int tz;
  int dtype;
  int nf;
  int dterr;
  char *field[MAXDATEFIELDS];
  int ftype[MAXDATEFIELDS];
  char workbuf[MAXDATELEN + MAXDATEFIELDS];
  DateTimeErrorExtra extra;
  bool has_tz = false;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);

  if (dterr == 0)
    dterr = DecodeDateTime(field, ftype, nf, &dtype, tm, &fsec, &tz, &extra);
  if (dterr != 0)
    return false;

  switch (dtype) {
  case DTK_DATE:
    for (int i = 0; i < nf; i++)
      if (ftype[i] == DTK_TZ) {
        has_tz = true;
        break;
      }

    if (has_tz)
      return false;

    if (tm2timestamp(tm, fsec, NULL, timestamp) != 0)
      return false;

    break;

  case DTK_EPOCH:
    GetEpochTime(tm);
    tm2timestamp(tm, 0, NULL, timestamp);
    break;

  case DTK_LATE:
    TIMESTAMP_NOEND(*timestamp);
    break;

  case DTK_EARLY:
    TIMESTAMP_NOBEGIN(*timestamp);
    break;

  default:
    return false;
  }

  return true;
}

void timestamp_show(Timestamp timestamp, char *buf)
    __attribute__((visibility("hidden")));
void timestamp_show(Timestamp timestamp, char *buf) {
  struct pg_tm tt, *tm = &tt;
  fsec_t fsec;

  if (timestamp2tm(timestamp, NULL, tm, &fsec, NULL, NULL) == 0)
    EncodeDateTime(tm, fsec, false, 0, NULL, USE_ISO_DATES, buf);
  else
    ereport(ERROR, (errcode(ERRCODE_DATETIME_VALUE_OUT_OF_RANGE),
                    errmsg("timestamp out of range")));
}

bool interval_read(Interval *interval, char *buf)
    __attribute__((visibility("hidden")));
bool interval_read(Interval *interval, char *buf) {
  struct pg_itm_in tt, *itm_in = &tt;
  int dtype;
  int nf;
  int range;
  int dterr;
  char *field[MAXDATEFIELDS];
  int ftype[MAXDATEFIELDS];
  char workbuf[256];

  itm_in->tm_year = 0;
  itm_in->tm_mon = 0;
  itm_in->tm_mday = 0;
  itm_in->tm_usec = 0;

  range = INTERVAL_FULL_RANGE;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);
  if (dterr == 0)
    dterr = DecodeInterval(field, ftype, nf, range, &dtype, itm_in);

  /* if those functions think it's a bad format, try ISO8601 style */
  if (dterr == DTERR_BAD_FORMAT)
    dterr = DecodeISO8601Interval(buf, &dtype, itm_in);

  if (dterr != 0)
    return false;

  switch (dtype) {
  case DTK_DELTA:
    if (itmin2interval(itm_in, interval) != 0)
      return false;

    break;

  case DTK_LATE:
  case DTK_EARLY:
    return false;

  default:
    return false;
  }

  return true;
}

void interval_show(Interval *interval, char *buf)
    __attribute__((visibility("hidden")));
void interval_show(Interval *interval, char *buf) {
  struct pg_itm tt, *itm = &tt;

  interval2itm(*interval, itm);
  EncodeInterval(itm, INTSTYLE_POSTGRES, buf);
}
