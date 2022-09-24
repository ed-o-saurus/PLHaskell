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

#include <template-hsc.h>

#define hsc_transfer(expr, name, sig) { \
    hsc_printf("do {"); \
    hsc_printf("pFunc <- (liftIO . newStablePtr) (%s); ", #expr); \
    hsc_printf("runStmt (\"%s <- Foreign.StablePtr.deRefStablePtr ( Foreign.StablePtr.castPtrToStablePtr ( Foreign.Ptr.wordPtrToPtr \" ++ (show . castStablePtrToPtr) pFunc ++ \")) :: Prelude.IO (%s)\"); ", #name, #sig); \
    hsc_printf("(liftIO . freeStablePtr) pFunc;"); \
    hsc_printf("}"); \
}
