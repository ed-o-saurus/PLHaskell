// This is a "procedural language" extension of PostgreSQL
// allowing the execution of code in Haskell within SQL code.
//
// Copyright (C) 2022 Edward F. Behn, Jr.
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

// Maximum memory the Haskell RTS is allowed to use (in bytes)
#define MAX_MEMORY 0x8000000 // 128 MiB

// Represents a value passed or returned by a function
struct ValueInfo
{
    bool isNull;
    int Type; // VOID_TYPE, BASE_TYPE, COMPOSITE_TYPE,
    union {
        struct { // BASE
            Oid TypeOid; // OID of the type
            bool ByVal; // Is the type passed by value?
            Datum Value; // value or pointer depending on ByVal
        };

        struct { // COMPOSITE
            int16 Count; // Number of fields of the composite
            int16 natts; // Number of attributes
            int16* attnums; // Attribute numbers of the members
            TupleDesc tupdesc; // Tuple Descriptor
            struct ValueInfo** Fields; // Fields of the composite type
        };
    };
};

// Represents the information about a function call
struct CallInfo
{
    char* FuncName; // Name of function
    char* ModFileName; // Temporary file where code is stored
    short nargs; // Number of arguments
    struct ValueInfo** Args; // Arguments
    struct ValueInfo* Result; // Returned result
    bool ReturnSet; // Does the function return a set of values?
    bool MoreResults; // Are there more results to return from a set?
    HsStablePtr Function; // Stable pointer to the function to be called to read Args and populate Result
    HsStablePtr List; // Stable pointer to list of results
};

// Report a message or error
void Report(int32 elevel, char* msg);

#endif
