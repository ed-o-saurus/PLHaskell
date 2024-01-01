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

#ifndef __PLHASKELL
#define __PLHASKELL

#include "postgres.h"
#include "funcapi.h"
#include "executor/spi.h"

#include "HsFFI.h"

#define VOID_TYPE      0
#define BASE_TYPE      1
#define COMPOSITE_TYPE 2
#define ARRAY_TYPE     3

// Represents a value passed or returned by a function
struct TypeInfo
{
    uint16 value_type; // VOID_TYPE, BASE_TYPE, or COMPOSITE_TYPE
    Oid type_oid; // OID of the type
    int16 type_len;
    bool type_byval;
    char type_align;

    union
    {
        struct // COMPOSITE
        {
            int16 count; // Number of fields of the composite
            int16 natts; // Number of attributes
            int16 *attnums; // Attribute numbers of the members
            TupleDesc tupdesc; // Tuple Descriptor
            struct TypeInfo **fields; // Fields of the composite type
        };

        // ARRAY
        struct TypeInfo *element; // Element type of array
    };

    char *nspname; // Schema and type name
    char *typname;
};

// Represents the information about a function call
struct CallInfo
{
    char *func_name; // Name of function
    char *mod_file_name; // Temporary file where code is stored
    bool trusted; // Is the language the trusted version?
    int16 nargs; // Number of arguments
    struct TypeInfo **args; // Arguments
    struct TypeInfo *result; // Returned result
    bool return_set; // Does the function return a set of values?
    bool spi_read_only; // Use read-only mode on internal queries
    bool atomic; // Is this an atomic transaction?

    union
    {
        Datum (*function)(NullableDatum *args, bool *isnull); // Pointer to the function to be called to read Args and populate Result
        HsStablePtr list; // Stable pointer to list of results
    };

    struct CallInfo *prev; // Used to link list of all active CallInfos
    struct CallInfo *next;
};

// Report a message or error
void plhaskell_report(int elevel, char *msg);

struct TypeInfo *new_type_info(Oid type_oid);
void delete_type_info(struct TypeInfo *p_type_info);

void read_composite(struct TypeInfo *p_type_info, Datum composite_datum, Datum *field_values, bool *field_is_nulls);
Datum write_composite(struct TypeInfo *p_type_info, Datum *field_values, bool *field_is_nulls);

Datum write_array(struct TypeInfo *pTypeInfo, Datum *elems, bool *nulls, int ndims, int *dims, int *lbs);
ArrayType *get_array_type(Datum datum);
int get_ndim(ArrayType *array);
int* get_lbs_ptr(ArrayType *array);
int* get_dims_ptr(ArrayType *array);
void get_array_elems(struct TypeInfo *pTypeInfo, ArrayType *array, int nelems, Datum* elems, bool* nulls);

// Functions for SPI queries
int run_query(const char *command, int nargs, Oid *argtypes, Datum *values, bool *is_nulls);
void get_header_field(struct SPITupleTable *tuptable, char *header, int fnumber);
void get_oids(struct SPITupleTable *tuptable, Oid *oids);
Datum get_tuple_datum(struct SPITupleTable *tuptable, uint64 row_number, int fnumber, bool *is_null);
void free_tuptable(struct SPITupleTable *tuptable);
Oid get_composite_oid(char *nspname, char *typname);
Oid find_composite_oid(char *typname);

Datum detoast_datum(Datum datum);
Datum datum_SPI_copy(struct TypeInfo *p_type_info, Datum datum);

void commit_rollback(bool commit, bool chain);

#endif
