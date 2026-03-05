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

#include "array_plh.h"
#include "spi_plh.h"

Datum write_array(TypeInfo *pTypeInfo, Datum *elems, bool *nulls, int ndims,
                  int *dims, int *lbs) __attribute__((visibility("hidden")));
Datum write_array(TypeInfo *pTypeInfo, Datum *elems, bool *nulls, int ndims,
                  int *dims, int *lbs) {
  Pointer dest, src;

  src = (Pointer)construct_md_array(
      elems, nulls, ndims, dims, lbs, pTypeInfo->element->type_oid,
      pTypeInfo->element->type_len, pTypeInfo->element->type_byval,
      pTypeInfo->element->type_align);
  int16 len = VARSIZE_ANY_EXHDR(src) + VARHDRSZ;

  dest = SPI_palloc(len);
  memcpy(dest, src, len);
  pfree(src);

  return PointerGetDatum(dest);
}

void get_array_elems(TypeInfo *pTypeInfo, ArrayType *array, int nelems,
                     Datum *elems, bool *nulls)
    __attribute__((visibility("hidden")));
void get_array_elems(TypeInfo *pTypeInfo, ArrayType *array, int nelems,
                     Datum *elems, bool *nulls) {
  Datum *elems_;
  bool *nulls_;
  int nelems_;

  deconstruct_array(array, pTypeInfo->element->type_oid,
                    pTypeInfo->element->type_len,
                    pTypeInfo->element->type_byval,
                    pTypeInfo->element->type_align, &elems_, &nulls_, &nelems_);
  if (nelems != nelems_)
    ereport(ERROR, errmsg_internal("Element count mismatch"));

  memcpy(elems, elems_, nelems * sizeof(Datum));
  memcpy(nulls, nulls_, nelems * sizeof(bool));

  pfree(elems_);
  pfree(nulls_);
}
