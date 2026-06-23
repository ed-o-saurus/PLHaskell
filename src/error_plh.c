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

#include "catalog/pg_namespace.h"
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

void expected_type(Oid type_oid, TypeInfo *pTypeInfo)
    __attribute__((visibility("hidden")));
void expected_type(Oid type_oid, TypeInfo *pTypeInfo) {
  char received_name[2 * MAXNAMLEN + 6], excpected_name[2 * MAXNAMLEN + 6];
  HeapTuple typtup, nsptup;
  Datum typnamespace, typname, nspname;
  bool is_null;

  typtup = SearchSysCache1(TYPEOID, ObjectIdGetDatum(type_oid));
  if (!HeapTupleIsValid(typtup))
    ereport(FATAL,
            errmsg_internal("cache lookup failed for type %u", type_oid));

  typnamespace =
      SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typnamespace, &is_null);
  if (is_null)
    ereport(FATAL, errmsg_internal("pg_type.typnamespace is NULL"));

  typname = SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typname, &is_null);
  if (is_null)
    ereport(FATAL, errmsg_internal("pg_type.typname is NULL"));

  if (DatumGetObjectId(typnamespace) == PG_CATALOG_NAMESPACE) {
    sprintf(received_name, "\"%s\"", DatumGetCString(typname));
  } else {
    nsptup = SearchSysCache1(NAMESPACEOID, DatumGetObjectId(typnamespace));
    if (!HeapTupleIsValid(nsptup))
      ereport(FATAL, errmsg_internal("cache lookup failed for namespace %u",
                                     DatumGetObjectId(typnamespace)));

    nspname = SysCacheGetAttr(NAMESPACEOID, nsptup, Anum_pg_namespace_nspname,
                              &is_null);
    if (is_null)
      ereport(FATAL, errmsg_internal("pg_namespace.nspname is NULL"));

    sprintf(received_name, "\"%s\".\"%s\"", DatumGetCString(nspname),
            DatumGetCString(typname));
  }

  typtup = SearchSysCache1(TYPEOID, ObjectIdGetDatum(pTypeInfo->type_oid));
  if (!HeapTupleIsValid(typtup))
    ereport(FATAL,
            errmsg_internal("cache lookup failed for type %u", type_oid));

  typnamespace =
      SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typnamespace, &is_null);
  if (is_null)
    ereport(FATAL, errmsg_internal("pg_type.typnamespace is NULL"));

  typname = SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typname, &is_null);
  if (is_null)
    ereport(FATAL, errmsg_internal("pg_type.typname is NULL"));

  if (DatumGetObjectId(typnamespace) == PG_CATALOG_NAMESPACE) {
    sprintf(excpected_name, "\"%s\"", DatumGetCString(typname));
  } else {
    nsptup = SearchSysCache1(NAMESPACEOID, DatumGetObjectId(typnamespace));
    if (!HeapTupleIsValid(nsptup))
      ereport(FATAL, errmsg_internal("cache lookup failed for namespace %u",
                                     DatumGetObjectId(typnamespace)));

    nspname = SysCacheGetAttr(NAMESPACEOID, nsptup, Anum_pg_namespace_nspname,
                              &is_null);
    if (is_null)
      ereport(FATAL, errmsg_internal("pg_namespace.nspname is NULL"));

    sprintf(excpected_name, "\"%s\".\"%s\"", DatumGetCString(nspname),
            DatumGetCString(typname));
  }

  ereport(ERROR, errmsg("Query expected %s but received %s.", excpected_name,
                        received_name));
}

void expected(uint16 value_type, TypeInfo *pTypeInfo)
    __attribute__((visibility("hidden")));
void expected(uint16 value_type, TypeInfo *pTypeInfo) {
  char *received_type = "", *expected_type = "";

  switch (value_type) {
  case BASE_TYPE:
    received_type = "Base";
    break;
  case COMPOSITE_TYPE:
    received_type = "Compisite";
    break;
  case ARRAY_TYPE:
    received_type = "Array";
    break;
  case RANGE_TYPE:
    received_type = "Range";
    break;
  case MULTIRANGE_TYPE:
    received_type = "MultiRange";
    break;
  }

  switch (pTypeInfo->value_type) {
  case BASE_TYPE:
    expected_type = "Base";
    break;
  case COMPOSITE_TYPE:
    expected_type = "Compisite";
    break;
  case ARRAY_TYPE:
    expected_type = "Array";
    break;
  case RANGE_TYPE:
    expected_type = "Range";
    break;
  case MULTIRANGE_TYPE:
    expected_type = "MultiRange";
    break;
  }

  ereport(ERROR, errmsg("Query expected %s type but received %s type.",
                        expected_type, received_type));
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

Datum null_bound() __attribute__((visibility("hidden")));
Datum null_bound() {
  ereport(ERROR, errcode(ERRCODE_NULL_VALUE_NOT_ALLOWED),
          errmsg("NULL(Nothing) bounds are not supported"));
}

Datum handler(char *msg) __attribute__((visibility("hidden")));
Datum handler(char *msg) {
  extern CallInfo *current_p_call_info;

  ereport(ERROR,
          errmsg("PL/Haskell: %s: %s", current_p_call_info->func_name, msg));

  PG_RETURN_VOID();
}
