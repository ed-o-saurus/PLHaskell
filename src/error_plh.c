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

#include "error_plh.h"

#include "utils/syscache.h"

// Used by Haskell
void plhaskell_report(int elevel, char *msg)
    __attribute__((visibility("hidden")));
void plhaskell_report(int elevel, char *msg) {
  ereport(elevel, errmsg_internal("%s", msg));
}

void bad_multi_dim_array() __attribute__((visibility("hidden")));
void bad_multi_dim_array() {
  ereport(ERROR, errmsg("Multidimensional arrays must have sub-arrays with "
                        "matching dimensions."));
}

void expected_type(Oid type_oid) __attribute__((visibility("hidden")));
void expected_type(Oid type_oid) {
  bool is_null;

  HeapTuple typtup = SearchSysCache1(TYPEOID, ObjectIdGetDatum(type_oid));
  if (!HeapTupleIsValid(typtup))
    ereport(ERROR,
            errmsg_internal("cache lookup failed for type %u", type_oid));

  Datum typname =
      SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typname, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_type.typname is NULL"));

  ereport(ERROR,
          errmsg("Expected type \"%s\" in query", DatumGetCString(typname)));
}

void expected_composite() __attribute__((visibility("hidden")));
void expected_composite() { ereport(ERROR, errmsg("Expected composite type")); }

void expected_array() __attribute__((visibility("hidden")));
void expected_array() { ereport(ERROR, errmsg("Expected array type")); }

void expected_type_in_query(TypeInfo *p_type_info)
    __attribute__((visibility("hidden")));
void expected_type_in_query(TypeInfo *p_type_info) {
  ereport(ERROR, errmsg("Expected type \"%s.%s\" in query",
                        p_type_info->nspname, p_type_info->typname));
}

void incorrect_length(TypeInfo *p_type_info)
    __attribute__((visibility("hidden")));
void incorrect_length(TypeInfo *p_type_info) {
  ereport(ERROR, errmsg("Type \"%s.%s\" incorrect length", p_type_info->nspname,
                        p_type_info->typname));
}

void higher_dim_arrays() __attribute__((visibility("hidden")));
void higher_dim_arrays() {
  ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
          errmsg("PL/Haskell does not support more than 6D arrays"));
}

void unknown_compiler_error() __attribute__((visibility("hidden")));
void unknown_compiler_error() {
  ereport(ERROR, errmsg("PL/Haskell : Unknown Compiler Error"));
}

void error_func_sig(char *func_sig) __attribute__((visibility("hidden")));
void error_func_sig(char *func_sig) {
  ereport(ERROR, errmsg("Expected Signature : %s", func_sig));
}

void language_error(int elevel, char *msg)
    __attribute__((visibility("hidden")));
void language_error(int elevel, char *msg) {
  char *filename_location;
  extern CallInfo *current_p_call_info;

  // Strip trailing new-line if necessary
  if (strlen(msg) > 1 && msg[strlen(msg) - 1] == '\n')
    msg[strlen(msg) - 1] = '\0';

  filename_location = strstr(msg, current_p_call_info->mod_file_name);

  if (!filename_location)
    ereport(elevel, errmsg_internal("%s", msg));
  else {
    int filename_length = strlen(current_p_call_info->mod_file_name);
    *filename_location = '\0';

    if (filename_location[filename_length] == ':' &&
        isdigit(filename_location[filename_length + 1])) {
      char *i;
      for (i = filename_location + filename_length + 1; isdigit(*i); i++)
        ;

      // Reduce line number by one to account for added line of code in file
      ereport(elevel,
              errmsg_internal("%s%s:%d%s", msg, current_p_call_info->func_name,
                              atoi(filename_location + filename_length + 1) - 1,
                              i));
    } else
      ereport(elevel,
              errmsg_internal("%s%s%s", msg, current_p_call_info->func_name,
                              filename_location + filename_length));
  }
}

Datum handler(char *msg) __attribute__((visibility("hidden")));
Datum handler(char *msg) {
  extern CallInfo *current_p_call_info;

  ereport(ERROR,
          errmsg("PL/Haskell: %s: %s", current_p_call_info->func_name, msg));

  PG_RETURN_VOID();
}
