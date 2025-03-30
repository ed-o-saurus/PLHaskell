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

#ifndef __PLH_ARRAY_H
#define __PLH_ARRAY_H

#include "plhaskell.h"

#include "utils/array.h"

Datum write_array(struct TypeInfo *pTypeInfo, Datum *elems, bool *nulls,
                  int ndims, int *dims, int *lbs);
ArrayType *get_array_type(Datum datum);
int get_ndim(ArrayType *array);
int *get_lbs_ptr(ArrayType *array);
int *get_dims_ptr(ArrayType *array);
void get_array_elems(struct TypeInfo *pTypeInfo, ArrayType *array, int nelems,
                     Datum *elems, bool *nulls);

#endif // __PLH_ARRAY_H
