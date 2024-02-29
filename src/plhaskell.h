// This is a "procedural language" extension of PostgreSQL
// allowing the execution of code in Haskell within SQL code.
//
// Copyright (C) 2023 Edward F. Behn, Jr.
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

#ifndef __PLHASKELL
#define __PLHASKELL

#include "postgres.h"

// If version 16 of higher
#if PG_VERSION_NUM >= 160000
#include "varatt.h"
#endif

#include "funcapi.h"
#include "executor/spi.h"

#include "HsFFI.h"

#define VOID_TYPE      0
#define BASE_TYPE      1
#define COMPOSITE_TYPE 2

// Represents a value passed or returned by a function
struct ValueInfo
{
    bool is_null;
    uint16 value_type; // VOID_TYPE, BASE_TYPE, or COMPOSITE_TYPE
    union {
        struct { // BASE
            Oid type_oid; // OID of the type
            Datum value; // Value of base type
        };

        struct { // COMPOSITE
            int16 count; // Number of fields of the composite
            int16 natts; // Number of attributes
            int16 *attnums; // Attribute numbers of the members
            TupleDesc tupdesc; // Tuple Descriptor
            struct ValueInfo **fields; // Fields of the composite type
        };
    };
};

// Represents the information about a function call
struct CallInfo
{
    char *func_name; // Name of function
    char *mod_file_name; // Temporary file where code is stored
    bool trusted; // Is the language the trusted version?
    int16 nargs; // Number of arguments
    struct ValueInfo **args; // Arguments
    struct ValueInfo *result; // Returned result
    bool return_set; // Does the function return a set of values?
    bool spi_read_only; // Use read-only mode on internal queries
    void (*function)(void); // Pointer to the function to be called to read Args and populate Result
    HsStablePtr list; // Stable pointer to list of results

    struct CallInfo *prev; // Used to link list of all active CallInfos
    struct CallInfo *next;
};

// Report a message or error
void plhaskell_report(int elevel, char *msg);

struct ValueInfo *new_value_info(Oid typeoid);
void delete_value_info(struct ValueInfo *p_value_info);

// Functions for SPI queries
int run_query(const char *command, int nargs, Oid *argtypes, Datum *values, bool *is_nulls);
void get_header_field(struct SPITupleTable *tuptable, char *header, int fnumber);
void get_oids(struct SPITupleTable *tuptable, Oid *oids);
void fill_value_info(struct SPITupleTable *tuptable, struct ValueInfo *p_value_info, uint64 row_number, int fnumber);
void free_tuptable(struct SPITupleTable *tuptable);

#endif
