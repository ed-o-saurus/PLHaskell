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

#include "spi_plh.h"

int run_query(const char *command, int nargs, Oid *argtypes, Datum *values,
              bool *is_nulls) __attribute__((visibility("hidden")));
int run_query(const char *command, int nargs, Oid *argtypes, Datum *values,
              bool *is_nulls) {
  int spi_code;
  char nulls[nargs + 1];

  for (int i = 0; i < nargs; i++)
    nulls[i] = is_nulls[i] ? 'n' : ' ';

  // This is just to stop the compiler from complaining.
  nulls[nargs] = '\0';

  spi_code = SPI_execute_with_args(command, nargs, argtypes, values, nulls,
                                   get_current_p_call_info()->spi_read_only, 0);
  if (spi_code < 0)
    ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

  return spi_code;
}

void get_header_field(struct SPITupleTable *tuptable, char *header, int fnumber)
    __attribute__((visibility("hidden")));
void get_header_field(struct SPITupleTable *tuptable, char *header,
                      int fnumber) {
  char *name = SPI_fname(tuptable->tupdesc, fnumber);
  strcpy(header, name);
  pfree(name);
}

// Get OID of types of columns from SPI query
void get_oids(struct SPITupleTable *tuptable, Oid *oids)
    __attribute__((visibility("hidden")));
void get_oids(struct SPITupleTable *tuptable, Oid *oids) {
  for (int i = 0; i < tuptable->tupdesc->natts; i++)
    oids[i] = SPI_gettypeid(tuptable->tupdesc, i + 1);
}

Datum get_tuple_datum(struct SPITupleTable *tuptable, uint64 row_number,
                      int fnumber, bool *is_null)
    __attribute__((visibility("hidden")));
Datum get_tuple_datum(struct SPITupleTable *tuptable, uint64 row_number,
                      int fnumber, bool *is_null) {
  return SPI_getbinval(tuptable->vals[row_number], tuptable->tupdesc, fnumber,
                       is_null);
}

Datum datum_SPI_copy(struct TypeInfo *p_type_info, Datum datum)
    __attribute__((visibility("hidden")));
Datum datum_SPI_copy(struct TypeInfo *p_type_info, Datum datum) {
  if (p_type_info->type_byval)
    return datum;

  Pointer dest, src = DatumGetPointer(datum);

  if (p_type_info->type_len < 0) {
    // varlena
    int16 len = VARSIZE_ANY_EXHDR(datum) + VARHDRSZ;
    dest = SPI_palloc(len);
    memcpy(dest, src, len);
  } else {
    dest = SPI_palloc(p_type_info->type_len);
    memcpy(dest, src, p_type_info->type_len);
  }
  pfree(src);

  return PointerGetDatum(dest);
}

void commit_rollback(bool commit, bool chain)
    __attribute__((visibility("hidden")));
void commit_rollback(bool commit, bool chain) {
  if (commit) {
    if (chain)
      SPI_commit_and_chain();
    else
      SPI_commit();
  } else {
    if (chain)
      SPI_rollback_and_chain();
    else
      SPI_rollback();
  }
}
