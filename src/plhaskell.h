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

#ifndef __PLHASKELL_H
#define __PLHASKELL_H

#include "postgres.h"

// If version 16 of higher
#if PG_VERSION_NUM >= 160000
#include "varatt.h"
#endif

#include "access/tupdesc.h"
#include "funcapi.h"

#include "HsFFI.h"

#define USE_ISO_DATES 1
#define INTSTYLE_POSTGRES 0

#define VOID_TYPE 0
#define BASE_TYPE 1
#define COMPOSITE_TYPE 2
#define ARRAY_TYPE 3

// Represents a value passed or returned by a function
typedef struct TypeInfo {
  uint16 value_type; // VOID_TYPE, BASE_TYPE, COMPOSITE_TYPE, or ARRAY_TYPE
  Oid type_oid;      // OID of the type
  int16 type_len;
  bool type_byval;
  char type_align;

  union {
    struct // COMPOSITE
    {
      int16 count;              // Number of fields of the composite
      int16 natts;              // Number of attributes
      int16 *attnums;           // Attribute numbers of the members
      TupleDesc tupdesc;        // Tuple Descriptor
      struct TypeInfo **fields; // Fields of the composite type
    };

    // ARRAY
    struct TypeInfo *element; // Element type of array
  };

  char *nspname; // Schema and type name of composite or element of array
  char *typname;
} TypeInfo;

// Represents the information about a function call
typedef struct CallInfo {
  char *func_name;     // Name of function
  char *mod_file_name; // Temporary file where code is stored
  bool trusted;        // Is the language the trusted version?
  int16 nargs;         // Number of arguments
  TypeInfo **args;     // Arguments
  TypeInfo *result;    // Returned result
  bool return_set;     // Does the function return a set of values?
  bool spi_read_only;  // Use read-only mode on internal queries
  bool atomic;         // Is this an atomic transaction?

  union {
    Datum (*function)(
        NullableDatum *args,
        bool *isnull); // Pointer to the function to be called to return result
    HsStablePtr list;  // Stable pointer to list of results
  };

  struct CallInfo *prev; // Used to link list of all active CallInfos
  struct CallInfo *next;
} CallInfo;

TypeInfo *new_type_info(Oid type_oid);
void delete_type_info(TypeInfo *p_type_info);

void read_composite(TypeInfo *p_type_info, Datum composite_datum,
                    Datum *field_values, bool *field_is_nulls);
Datum write_composite(TypeInfo *p_type_info, Datum *field_values,
                      bool *field_is_nulls);

Oid get_oid(bool array, char *nspname, char *typname);
Oid find_oid(bool array, char *typname);

Datum detoast_datum(Datum datum);

Datum call_func(Oid functionId, int16 nargs, NullableDatum *args, bool *isnull);
MemoryContext alloc_set_context_create_small_temp(MemoryContext parent);

CallInfo *get_current_p_call_info(void);

#endif // __PLHASKELL_H
