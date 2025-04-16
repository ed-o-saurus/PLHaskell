// This is a "procedural language" extension of PostgreSQL
// allowing the execution of code in Haskell within SQL code.
//
// Copyright (C) 2024 Edward F. Behn, Jr.
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

#ifndef __PLH_ERROR_H
#define __PLH_ERROR_H

#include "plhaskell.h"

// Report a message or error
void plhaskell_report(int elevel, char *msg);

void bad_multi_dim_array();
void expected_type(Oid type_oid);
void expected_composite();
void expected_array();
void expected_type_in_query(TypeInfo *p_type_info);
void incorrect_length(TypeInfo *p_type_info);
void higher_dim_arrays();
void unknown_compiler_error();
void error_func_sig(char *func_sig);
void language_error(int elevel, char *msg);

Datum handler(char *msg);

#endif // __PLH_ERROR_H
