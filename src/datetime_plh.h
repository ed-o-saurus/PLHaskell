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

#ifndef __PLH_DATETIME_H
#define __PLH_DATETIME_H

#include "plhaskell.h"

#include "utils/date.h"
#include "utils/datetime.h"
#include "utils/timestamp.h"

bool date_read(DateADT *date, char *buf);
void date_show(DateADT date, char *buf);

bool time_read(TimeADT *time, char *buf);
void time_show(TimeADT time, char *buf);

bool timestamp_read(Timestamp *timestamp, char *buf);
void timestamp_show(Timestamp timestamp, char *buf);

bool interval_read(Interval *interval, char *buf);
void interval_show(Interval *interval, char *buf);

#endif // __PLH_DATETIME_H
