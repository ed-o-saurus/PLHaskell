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
#include "funcapi.h"

#include "HsFFI.h"

#define VOID_TYPE      0
#define BASE_TYPE      1
#define COMPOSITE_TYPE 2

// Maximum memory the Haskell RTS is allowed to use (in MiB)
#define MAX_MEMORY 128

// Represents a value passed or returned by a function
struct ValueInfo
{
    bool is_null;
    int type; // VOID_TYPE, BASE_TYPE, or COMPOSITE_TYPE,
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
    short nargs; // Number of arguments
    struct ValueInfo **args; // Arguments
    struct ValueInfo *result; // Returned result
    bool return_set; // Does the function return a set of values?
    bool more_results; // Are there more results to return from a set?
    void (*function)(void); // Pointer to the function to be called to read Args and populate Result
    HsStablePtr list; // Stable pointer to list of results
};

// Report a message or error
void plhaskell_report(int32 elevel, char *msg);

#endif
