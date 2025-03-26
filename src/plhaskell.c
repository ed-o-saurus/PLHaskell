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

#include "plhaskell.h"
#include "PLHaskell_stub.h"

#include "Rts.h"

#include "catalog/namespace.h"
#include "catalog/pg_attribute_d.h"
#include "catalog/pg_class_d.h"
#include "catalog/pg_language_d.h"
#include "catalog/pg_namespace.h"
#include "catalog/pg_proc_d.h"
#include "catalog/pg_tablespace_d.h"
#include "catalog/pg_type_d.h"
#include "storage/ipc.h"
#include "utils/guc.h"
#include "utils/syscache.h"
#include "utils/typcache.h"

#define ARRAY_SUBSCRIPT_HANDLER_NAME "array_subscript_handler"

extern char pkglib_path[];

static void build_call_info(struct CallInfo *p_call_info, Oid func_oid,
                            bool return_set, bool atomic);
static void build_type_info(struct TypeInfo *p_type_info, Oid type_oid,
                            bool set_schema_name);

static void destroy_call_info(void *arg);

static void enter(void);
static void gcDoneHook(const struct GCDetails_ *stats);

static int rts_msg_fn(int elevel, const char *s, va_list ap);

#if __GLASGOW_HASKELL__ >= 902
static int rts_debug_msg_fn(const char *s, va_list ap);
#else
static void rts_debug_msg_fn(const char *s, va_list ap);
#endif

static void rts_fatal_msg_fn(const char *s, va_list ap);

static void mod_exit(int code, Datum arg);

static struct CallInfo *current_p_call_info = NULL;
static struct CallInfo *first_p_call_info =
    NULL; // Points to list of all active CallInfos

static int plhaskell_max_memory;
static Oid array_subscript_handler_oid;

PG_MODULE_MAGIC;

// Main handler
PG_FUNCTION_INFO_V1(plhaskell_call_handler);
Datum plhaskell_call_handler(PG_FUNCTION_ARGS) {
  struct CallInfo *p_call_info;
  Oid func_oid = fcinfo->flinfo->fn_oid; // OID of the function being handled
  HeapTuple proctup;
  Datum proretset;
  bool is_null;
  int spi_code;
  bool nonatomic;
  struct CallInfo *prev_p_call_info;
  Datum ret_val;

  proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(func_oid));
  if (!HeapTupleIsValid(proctup))
    ereport(ERROR,
            errmsg_internal("cache lookup failed for function %u", func_oid));

  proretset =
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proretset, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.proretset is NULL"));

  ReleaseSysCache(proctup);

  nonatomic = fcinfo->context && IsA(fcinfo->context, CallContext) &&
              !castNode(CallContext, fcinfo->context)->atomic;

  // If we're returning a set
  if (DatumGetBool(proretset)) {
    FuncCallContext *funcctx;
    ReturnSetInfo *rsi = (ReturnSetInfo *)fcinfo->resultinfo;

    if (fcinfo->flinfo->fn_extra == NULL) {
      MemoryContext old_context;
      MemoryContextCallback *cb;

      if (!(rsi->allowedModes & SFRM_ValuePerCall))
        ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                errmsg("Bad return mode"));

      rsi->returnMode = SFRM_ValuePerCall;

      funcctx = init_MultiFuncCall(fcinfo);
      old_context = MemoryContextSwitchTo(funcctx->multi_call_memory_ctx);

      cb = palloc(sizeof(MemoryContextCallback));

      // Allocate, setup, and Build CallInfo
      p_call_info = funcctx->user_fctx = palloc0(sizeof(struct CallInfo));

      // Register callback to free List and Iterator and delete temp module file
      cb->func = destroy_call_info;
      cb->arg = p_call_info;
      MemoryContextRegisterResetCallback(funcctx->multi_call_memory_ctx, cb);

      build_call_info(p_call_info, func_oid, true, !nonatomic);

      spi_code = SPI_connect_ext(p_call_info->atomic ? 0 : SPI_OPT_NONATOMIC);
      if (spi_code < 0)
        ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

      // Setup the iterator and list
      prev_p_call_info = current_p_call_info;
      current_p_call_info = p_call_info;
      mk_list(p_call_info, fcinfo->args);
      current_p_call_info = prev_p_call_info;

      spi_code = SPI_finish();
      if (spi_code < 0)
        ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

      MemoryContextSwitchTo(old_context);
    }

    funcctx = per_MultiFuncCall(fcinfo);
    p_call_info = funcctx->user_fctx;

    spi_code = SPI_connect_ext(p_call_info->atomic ? 0 : SPI_OPT_NONATOMIC);
    if (spi_code < 0)
      ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

    // Iterate the list
    prev_p_call_info = current_p_call_info;
    current_p_call_info = p_call_info;
    ret_val = iterate(p_call_info, &fcinfo->isnull);
    current_p_call_info = prev_p_call_info;

    spi_code = SPI_finish();
    if (spi_code < 0)
      ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

    if (p_call_info->list) // Is there another result?
      rsi->isDone = ExprMultipleResult;
    else {
      end_MultiFuncCall(fcinfo, funcctx);
      rsi->isDone = ExprEndResult;
    }

    return ret_val;
  } else {
    if (fcinfo->flinfo->fn_extra == NULL) {
      MemoryContext old_context =
          MemoryContextSwitchTo(fcinfo->flinfo->fn_mcxt);
      MemoryContextCallback *cb = palloc(sizeof(MemoryContextCallback));

      // Allocate, setup, and Build CallInfo
      p_call_info = fcinfo->flinfo->fn_extra = palloc0(sizeof(struct CallInfo));

      // Register callback to free function and delete temp module file
      cb->func = destroy_call_info;
      cb->arg = p_call_info;
      MemoryContextRegisterResetCallback(fcinfo->flinfo->fn_mcxt, cb);

      build_call_info(p_call_info, func_oid, false, !nonatomic);

      // Make the function
      prev_p_call_info = current_p_call_info;
      current_p_call_info = p_call_info;
      mk_function(p_call_info);
      current_p_call_info = prev_p_call_info;

      MemoryContextSwitchTo(old_context);
    }

    p_call_info = fcinfo->flinfo->fn_extra;

    spi_code = SPI_connect_ext(p_call_info->atomic ? 0 : SPI_OPT_NONATOMIC);
    if (spi_code < 0)
      ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

    // Run the function
    prev_p_call_info = current_p_call_info;
    current_p_call_info = p_call_info;
    ret_val = (*p_call_info->function)(fcinfo->args, &fcinfo->isnull);
    current_p_call_info = prev_p_call_info;

    spi_code = SPI_finish();
    if (spi_code < 0)
      ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

    return ret_val;
  }
}

// Called when PL/Haskell function is created
PG_FUNCTION_INFO_V1(plhaskell_validator);
Datum plhaskell_validator(PG_FUNCTION_ARGS) {
  struct CallInfo *p_call_info;
  Oid func_oid = PG_GETARG_OID(0);
  MemoryContextCallback *cb;
  HeapTuple proctup;
  Datum proretset;
  bool is_null;
  struct CallInfo *prev_p_call_info;

  if (!CheckFunctionValidatorAccess(fcinfo->flinfo->fn_oid, func_oid))
    PG_RETURN_VOID();

  if (!check_function_bodies)
    PG_RETURN_VOID();

  proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(func_oid));
  if (!HeapTupleIsValid(proctup))
    ereport(ERROR,
            errmsg_internal("cache lookup failed for function %u", func_oid));

  proretset =
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proretset, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.proretset is NULL"));

  ReleaseSysCache(proctup);

  cb = palloc(sizeof(MemoryContextCallback));

  // Allocate, setup, and Build CallInfo
  p_call_info = palloc0(sizeof(struct CallInfo));

  // Register callback to delete temp module file
  cb->func = destroy_call_info;
  cb->arg = p_call_info;
  MemoryContextRegisterResetCallback(CurrentMemoryContext, cb);

  build_call_info(p_call_info, func_oid, DatumGetBool(proretset), false);

  // Raise error if the function's signature is incorrect
  prev_p_call_info = current_p_call_info;
  current_p_call_info = p_call_info;
  check_signature(p_call_info);
  current_p_call_info = prev_p_call_info;

  PG_RETURN_VOID();
}

// Called when PL/Haskell DO block is run
PG_FUNCTION_INFO_V1(plhaskell_inline_handler);
Datum plhaskell_inline_handler(PG_FUNCTION_ARGS) {
  InlineCodeBlock *codeblock = (InlineCodeBlock *)PG_GETARG_POINTER(0);
  MemoryContextCallback *cb = palloc(sizeof(MemoryContextCallback));
  struct CallInfo *p_call_info = palloc0(sizeof(struct CallInfo));
  char tempdirpath[MAXPGPATH];
  FILE *modfile;
  int spi_code;
  struct CallInfo *prev_p_call_info;
  bool is_null; // dummy variable

  // Register callback to free List and Iterator and delete temp module file
  cb->func = destroy_call_info;
  cb->arg = p_call_info;
  MemoryContextRegisterResetCallback(CurrentMemoryContext, cb);

  p_call_info->return_set = false;
  p_call_info->atomic = codeblock->atomic;
  p_call_info->trusted = codeblock->langIsTrusted;
  p_call_info->spi_read_only = false;
  p_call_info->nargs = 0;

  p_call_info->result = palloc0(sizeof(struct TypeInfo));
  build_type_info(p_call_info->result, VOIDOID, false);

  p_call_info->func_name = palloc(3);
  strcpy(p_call_info->func_name, "_'");

  TempTablespacePath(tempdirpath, DEFAULTTABLESPACE_OID);
  p_call_info->mod_file_name = palloc0(MAXPGPATH);
  snprintf(p_call_info->mod_file_name, MAXPGPATH, "%s/ModXXXXXX.hs",
           tempdirpath);
  modfile = fdopen(mkstemps(p_call_info->mod_file_name, 3), "w");
  if (!modfile)
    ereport(ERROR, errmsg_internal("Unable to create temporary file (%s)",
                                   p_call_info->mod_file_name));

  fprintf(modfile, "module PGmodule (_') where\n");
  fputs(codeblock->source_text, modfile);

  fclose(modfile);

  // Add p_call_info to the list of all active CallInfos
  if (first_p_call_info) {
    p_call_info->next = first_p_call_info;
    first_p_call_info->prev = p_call_info;
  }

  first_p_call_info = p_call_info;

  spi_code = SPI_connect_ext(p_call_info->atomic ? 0 : SPI_OPT_NONATOMIC);
  if (spi_code < 0)
    ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

  // Make and call the function
  prev_p_call_info = current_p_call_info;
  current_p_call_info = p_call_info;
  mk_function(p_call_info);
  (*p_call_info->function)(NULL, &is_null);
  current_p_call_info = prev_p_call_info;

  spi_code = SPI_finish();
  if (spi_code < 0)
    ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(spi_code)));

  PG_RETURN_VOID();
}

// Fill CallInfo struct
static void build_call_info(struct CallInfo *p_call_info, Oid func_oid,
                            bool return_set, bool atomic) {
  HeapTuple proctup, lantup;
  Datum provariadic, prokind, prorettype, proargtypes, prosrc, proname,
      provolatile, proparallel, prolang, lanpltrusted;
  ArrayType *proargtypes_arr;
  Oid *argtypes;
  bool is_null;
  text *src;
  char tempdirpath[MAXPGPATH];
  FILE *modfile;

  p_call_info->return_set = return_set;
  p_call_info->atomic = atomic;

  proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(func_oid));
  if (!HeapTupleIsValid(proctup))
    ereport(ERROR,
            errmsg_internal("cache lookup failed for function %u", func_oid));

  prolang = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prolang, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.prolang is NULL"));

  lantup = SearchSysCache1(LANGOID, prolang);

  lanpltrusted =
      SysCacheGetAttr(LANGOID, lantup, Anum_pg_language_lanpltrusted, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_language.lanpltrusted is NULL"));

  p_call_info->trusted = DatumGetBool(lanpltrusted);

  ReleaseSysCache(lantup);

  provariadic =
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_provariadic, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.provariadic is NULL"));

  if (DatumGetObjectId(provariadic) != 0)
    ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
            errmsg("PL/Haskell : Variadic types not allowed"));

  prokind = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prokind, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.prokind is NULL"));

  if (DatumGetChar(prokind) != PROKIND_FUNCTION)
    ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
            errmsg("PL/Haskell : Only normal function allowed"));

  provolatile =
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_provolatile, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.provolatile is NULL"));

  p_call_info->spi_read_only =
      DatumGetChar(provolatile) != PROVOLATILE_VOLATILE;

  p_call_info->nargs = DatumGetInt16(
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_pronargs, &is_null));
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.pronargs is NULL"));

  p_call_info->args = palloc(p_call_info->nargs * sizeof(struct TypeInfo *));

  SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proargmodes, &is_null);
  if (!is_null)
    ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
            errmsg("PL/Haskell : Only IN arguments allowed"));

  prorettype =
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prorettype, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.prorettype is NULL"));

  proparallel =
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proparallel, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.proparallel is NULL"));

  if (DatumGetChar(proparallel) != PROPARALLEL_UNSAFE)
    ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
            errmsg("PL/Haksell : Function must be parallel unsafe"));

  p_call_info->result = palloc0(sizeof(struct TypeInfo));
  build_type_info(p_call_info->result, prorettype, false);

  proargtypes =
      SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proargtypes, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.proargtypes is NULL"));

  proargtypes_arr = DatumGetArrayTypeP(proargtypes);

  if (ARR_NDIM(proargtypes_arr) != 1)
    ereport(ERROR, errmsg_internal("pg_proc.proargtypes has %d dimensions",
                                   ARR_NDIM(proargtypes_arr)));

  if (ARR_LBOUND(proargtypes_arr)[0] != 0 ||
      ARR_DIMS(proargtypes_arr)[0] != p_call_info->nargs)
    ereport(ERROR, errmsg_internal("pg_proc.proargtypes has unexpected size"));

  if (ARR_NULLBITMAP(proargtypes_arr) != NULL)
    ereport(ERROR, errmsg_internal("pg_proc.proargtypes has NULL element"));

  argtypes = (Oid *)ARR_DATA_PTR(proargtypes_arr);
  for (int16 i = 0; i < p_call_info->nargs; i++) {
    p_call_info->args[i] = palloc0(sizeof(struct TypeInfo));
    build_type_info(p_call_info->args[i], argtypes[i], false);
  }

  proname = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proname, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.proname is NULL"));

  p_call_info->func_name = palloc(NAMEDATALEN);
  memcpy(p_call_info->func_name, DatumGetName(proname)->data, NAMEDATALEN);

  // Fill temp file with function source
  prosrc = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prosrc, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.prosrc is NULL"));

  src = DatumGetTextPP(prosrc);

  TempTablespacePath(tempdirpath, DEFAULTTABLESPACE_OID);
  p_call_info->mod_file_name = palloc0(MAXPGPATH);
  snprintf(p_call_info->mod_file_name, MAXPGPATH, "%s/ModXXXXXX.hs",
           tempdirpath);
  modfile = fdopen(mkstemps(p_call_info->mod_file_name, 3), "w");
  if (!modfile)
    ereport(ERROR, errmsg_internal("Unable to create temporary file (%s)",
                                   p_call_info->mod_file_name));

  fprintf(modfile, "module PGmodule (%s) where\n", p_call_info->func_name);
  fwrite(VARDATA_ANY(src), VARSIZE_ANY_EXHDR(src), 1, modfile);
  fclose(modfile);

  ReleaseSysCache(proctup);

  // Add p_call_info to the list of all active CallInfos
  if (first_p_call_info) {
    p_call_info->next = first_p_call_info;
    first_p_call_info->prev = p_call_info;
  }

  first_p_call_info = p_call_info;
}

// Fill TypeInfo struct
static void build_type_info(struct TypeInfo *p_type_info, Oid type_oid,
                            bool set_schema_name) {
  HeapTuple typtup, reltup, atttup, nsptup;
  Datum typtype, typname, typlen, typbyval, typalign, typbasetype, typrelid,
      typnamespace, typsubscript, typelem;
  Datum relnatts;
  Datum atttypid;
  Datum nspname;
  char *type_name;
  bool is_null;
  Oid class_oid, attr_oid;

  p_type_info->type_oid = type_oid;

  if (type_oid == VOIDOID) {
    p_type_info->value_type = VOID_TYPE;
    return;
  }

  typtup = SearchSysCache1(TYPEOID, ObjectIdGetDatum(type_oid));
  if (!HeapTupleIsValid(typtup))
    ereport(ERROR,
            errmsg_internal("cache lookup failed for type %u", type_oid));

  typlen = SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typlen, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_type.typlen is NULL"));

  typbyval = SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typbyval, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_type.typbyval is NULL"));

  typalign = SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typalign, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_type.typalign is NULL"));

  p_type_info->type_len = DatumGetInt16(typlen);
  p_type_info->type_byval = DatumGetBool(typbyval);
  p_type_info->type_align = DatumGetChar(typalign);

  typname = SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typname, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_type.typname is NULL"));

  type_name = DatumGetCString(typname);

  typtype = SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typtype, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_type.typtype is NULL"));

  switch (DatumGetChar(typtype)) {
  case TYPTYPE_BASE:
    typsubscript =
        SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typsubscript, &is_null);
    if (is_null)
      ereport(ERROR, errmsg_internal("pg_type.typsubscript is NULL"));

    if (DatumGetObjectId(typsubscript) ==
        array_subscript_handler_oid) // if type is an array type
    {
      p_type_info->value_type = ARRAY_TYPE;

      typelem =
          SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typelem, &is_null);
      if (is_null)
        ereport(ERROR, errmsg_internal("pg_type.typelem is NULL"));

      p_type_info->element = palloc0(sizeof(struct TypeInfo));
      build_type_info(p_type_info->element, DatumGetObjectId(typelem),
                      set_schema_name);
    } else {
      if (!type_available(type_oid))
        ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                errmsg("PL/Haskell does not support type %s", type_name));

      p_type_info->value_type = BASE_TYPE;
    }
    break;
  case TYPTYPE_COMPOSITE:
    p_type_info->value_type = COMPOSITE_TYPE;
    p_type_info->count = 0;
    p_type_info->attnums = palloc(0);
    p_type_info->fields = palloc(0);

    typrelid =
        SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typrelid, &is_null);
    if (is_null)
      ereport(ERROR, errmsg_internal("pg_type.typrelid is NULL"));

    class_oid = ObjectIdGetDatum(typrelid);

    reltup = SearchSysCache1(RELOID, ObjectIdGetDatum(class_oid));
    if (!HeapTupleIsValid(reltup))
      ereport(ERROR,
              errmsg_internal("cache lookup failed for class %u", class_oid));

    relnatts =
        SysCacheGetAttr(RELOID, reltup, Anum_pg_class_relnatts, &is_null);
    if (is_null)
      ereport(ERROR, errmsg_internal("pg_class.relnatts is NULL"));

    p_type_info->natts = DatumGetInt16(relnatts);

    // Loop over all composite type attributes
    for (int16 j = 0; j < p_type_info->natts; j++) {
      atttup = SearchSysCache2(ATTNUM, ObjectIdGetDatum(class_oid),
                               Int16GetDatum(j + 1));
      if (!HeapTupleIsValid(atttup))
        ereport(ERROR, errmsg_internal(
                           "cache lookup failed for attribute %u attnum %d",
                           class_oid, j + 1));

      atttypid =
          SysCacheGetAttr(ATTNUM, atttup, Anum_pg_attribute_atttypid, &is_null);
      if (is_null)
        ereport(ERROR, errmsg_internal("pg_attribute.atttypid is NULL"));

      // If the attribute is not dropped
      if ((attr_oid = DatumGetObjectId(atttypid))) {
        p_type_info->count++;

        if (p_type_info->count > 63)
          ereport(ERROR, errcode(ERRCODE_TOO_MANY_COLUMNS),
                  errmsg("PL/Haskell : Tuple size too large (%s)", type_name));

        p_type_info->attnums =
            repalloc(p_type_info->attnums, p_type_info->count * sizeof(int16));
        p_type_info->attnums[p_type_info->count - 1] = j + 1;

        p_type_info->fields =
            repalloc(p_type_info->fields,
                     p_type_info->count * sizeof(struct TypeInfo *));
        p_type_info->fields[p_type_info->count - 1] =
            palloc0(sizeof(struct TypeInfo));
        build_type_info(p_type_info->fields[p_type_info->count - 1], attr_oid,
                        set_schema_name);
      }

      ReleaseSysCache(atttup);
    }

    ReleaseSysCache(reltup);

    p_type_info->tupdesc = lookup_rowtype_tupdesc_copy(type_oid, -1);
    BlessTupleDesc(p_type_info->tupdesc);

    break;
  case TYPTYPE_DOMAIN:
    typbasetype =
        SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typbasetype, &is_null);
    if (is_null)
      ereport(ERROR, errmsg_internal("pg_type.typbasetype is NULL"));

    build_type_info(p_type_info, DatumGetObjectId(typbasetype),
                    set_schema_name);

    break;
  case TYPTYPE_ENUM:
    ereport(
        ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
        errmsg("PL/Haskell does not support enumerated types (%s)", type_name));
    break;
#ifdef TYPTYPE_MULTIRANGE
  case TYPTYPE_MULTIRANGE:
#endif
  case TYPTYPE_RANGE:
    ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
            errmsg("PL/Haskell does not support range types (%s)", type_name));
    break;
  case TYPTYPE_PSEUDO:
    ereport(ERROR, errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
            errmsg("PL/Haskell does not support type %s", type_name));
    break;
  default:
    ereport(ERROR, errmsg_internal("pg_type.typtype is invalid : %c",
                                   DatumGetChar(typtype)));
  }

  if (set_schema_name) {
    typnamespace =
        SysCacheGetAttr(TYPEOID, typtup, Anum_pg_type_typnamespace, &is_null);

    nsptup = SearchSysCache1(NAMESPACEOID, DatumGetObjectId(typnamespace));
    if (!HeapTupleIsValid(nsptup))
      ereport(ERROR, errmsg_internal("cache lookup failed for namespace %u",
                                     DatumGetObjectId(typnamespace)));

    nspname = SysCacheGetAttr(NAMESPACEOID, nsptup, Anum_pg_namespace_nspname,
                              &is_null);
    if (is_null)
      ereport(ERROR, errmsg_internal("pg_namespace.nspname is NULL"));

    p_type_info->nspname = palloc(NAMEDATALEN);
    strcpy(p_type_info->nspname, DatumGetCString(nspname));

    ReleaseSysCache(nsptup);

    p_type_info->typname = palloc(NAMEDATALEN);
    strcpy(p_type_info->typname, type_name);
  }

  ReleaseSysCache(typtup);
}

// Delete temp module file and free function and List if necessary
static void destroy_call_info(void *arg) {
  struct CallInfo *p_call_info = arg;

  if (p_call_info->return_set) {
    if (p_call_info->list) {
      hs_free_stable_ptr(p_call_info->list);
      p_call_info->list = NULL;
    }
  } else {
    if (p_call_info->function) {
      hs_free_fun_ptr((HsFunPtr)(p_call_info->function));
      p_call_info->function = NULL;
    }
  }

  if (p_call_info->mod_file_name) {
    unlink(p_call_info->mod_file_name);
    pfree(p_call_info->mod_file_name);
    p_call_info->mod_file_name = NULL;
  }

  if (first_p_call_info == p_call_info)
    first_p_call_info = p_call_info->next;

  if (p_call_info->prev)
    p_call_info->prev->next = p_call_info->next;

  if (p_call_info->next)
    p_call_info->next->prev = p_call_info->prev;
}

// Get the datums that represent a composie values
void read_composite(struct TypeInfo *p_type_info, Datum composite_datum,
                    Datum *field_values, bool *field_is_nulls)
    __attribute__((visibility("hidden")));
void read_composite(struct TypeInfo *p_type_info, Datum composite_datum,
                    Datum *field_values, bool *field_is_nulls) {
  HeapTupleHeader tuple = DatumGetHeapTupleHeader(composite_datum);
  HeapTupleData tmptup;

  tmptup.t_len = HeapTupleHeaderGetDatumLength(tuple);
  ItemPointerSetInvalid(&(tmptup.t_self));
  tmptup.t_tableOid = InvalidOid;
  tmptup.t_data = tuple;

  for (int16 j = 0; j < p_type_info->count; j++)
    field_values[j] = heap_getattr(&tmptup, p_type_info->attnums[j],
                                   p_type_info->tupdesc, field_is_nulls + j);
}

// Get the datum that represents a composite value
Datum write_composite(struct TypeInfo *p_type_info, Datum *field_values,
                      bool *field_is_nulls)
    __attribute__((visibility("hidden")));
Datum write_composite(struct TypeInfo *p_type_info, Datum *field_values,
                      bool *field_is_nulls) {
  Datum *values = palloc(p_type_info->natts * sizeof(Datum));
  bool *is_nulls = palloc(p_type_info->natts * sizeof(bool));
  HeapTupleHeader ret_val;

  for (int16 j = 0; j < p_type_info->count; j++) {
    int16 attnum = p_type_info->attnums[j];
    values[attnum - 1] = field_values[j];
    is_nulls[attnum - 1] = field_is_nulls[j];
  }

  ret_val =
      SPI_returntuple(heap_form_tuple(p_type_info->tupdesc, values, is_nulls),
                      p_type_info->tupdesc);
  if (ret_val == NULL)
    ereport(ERROR, errmsg_internal("%s", SPI_result_code_string(SPI_result)));

  return PointerGetDatum(ret_val);
}

Datum write_array(struct TypeInfo *pTypeInfo, Datum *elems, bool *nulls,
                  int ndims, int *dims, int *lbs)
    __attribute__((visibility("hidden")));
Datum write_array(struct TypeInfo *pTypeInfo, Datum *elems, bool *nulls,
                  int ndims, int *dims, int *lbs) {
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

ArrayType *get_array_type(Datum datum) __attribute__((visibility("hidden")));
ArrayType *get_array_type(Datum datum) { return DatumGetArrayTypeP(datum); }

int get_ndim(ArrayType *array) __attribute__((visibility("hidden")));
int get_ndim(ArrayType *array) { return ARR_NDIM(array); }

int *get_lbs_ptr(ArrayType *array) __attribute__((visibility("hidden")));
int *get_lbs_ptr(ArrayType *array) { return ARR_LBOUND(array); }

int *get_dims_ptr(ArrayType *array) __attribute__((visibility("hidden")));
int *get_dims_ptr(ArrayType *array) { return ARR_DIMS(array); }

void get_array_elems(struct TypeInfo *pTypeInfo, ArrayType *array, int nelems,
                     Datum *elems, bool *nulls)
    __attribute__((visibility("hidden")));
void get_array_elems(struct TypeInfo *pTypeInfo, ArrayType *array, int nelems,
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

// Called when the module is loaded
static void enter(void) __attribute__((constructor));
static void enter(void) {
  Oid arg_oid = INTERNALOID;
  oidvector *parameterTypes;
  HeapTuple proctup;
  Datum procoid;
  bool is_null;
  static int argc = 3;
  static char *argv[] = {
      "PLHaskell", "+RTS", // Configuration for the RTS
      "-K0", // No limit on stack size. It's handled by gcDoneHook.
      NULL};
  static char **pargv = argv;
  RtsConfig conf = defaultRtsConfig;
  char tempdirpath[MAXPGPATH];

  on_proc_exit(mod_exit, (Datum)0);

  parameterTypes =
      (oidvector *)palloc0(offsetof(oidvector, values) + sizeof(Oid));
  SET_VARSIZE(parameterTypes, offsetof(oidvector, values) + sizeof(Oid));
  parameterTypes->ndim = 1;
  parameterTypes->dataoffset = 0;
  parameterTypes->elemtype = OIDOID;
  parameterTypes->dim1 = 1;
  parameterTypes->lbound1 = 0;
  memcpy(parameterTypes->values, &arg_oid, sizeof(Oid));

  proctup = SearchSysCache3(
      PROCNAMEARGSNSP, CStringGetDatum(ARRAY_SUBSCRIPT_HANDLER_NAME),
      PointerGetDatum(parameterTypes), ObjectIdGetDatum(PG_CATALOG_NAMESPACE));

  if (!HeapTupleIsValid(proctup))
    ereport(ERROR, errmsg_internal("cache lookup failed for proc %s",
                                   ARRAY_SUBSCRIPT_HANDLER_NAME));

  procoid =
      SysCacheGetAttr(PROCNAMEARGSNSP, proctup, Anum_pg_proc_oid, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_proc.oid is NULL"));

  array_subscript_handler_oid = DatumGetObjectId(procoid);

  ReleaseSysCache(proctup);
  pfree(parameterTypes);

  TempTablespacePath(tempdirpath, DEFAULTTABLESPACE_OID);
  MakePGDirectory(tempdirpath);
  setenv("TMPDIR", tempdirpath, true);

  DefineCustomIntVariable(
      "plhaskell.max_memory", gettext_noop("Maximum memory for PL/Haskell"),
      gettext_noop("This is the maximum memory that can be used by the Haskell "
                   "runtime system before a FATAL error."),
      &plhaskell_max_memory,
      131072, // 131072 kB = 128 MB
      0, MAX_KILOBYTES, PGC_SUSET, GUC_UNIT_KB, NULL, NULL, NULL);

  fatalInternalErrorFn = rts_fatal_msg_fn;
  debugMsgFn = rts_debug_msg_fn;
  errorMsgFn = rts_fatal_msg_fn;

  conf.rts_opts_enabled = RtsOptsAll;
  conf.gcDoneHook =
      gcDoneHook; // Called on every garbage collections to monitor memory usage
  hs_init_ghc(&argc, &pargv, conf);
}

// Called on every garbage collection to monitor memory usage
static void gcDoneHook(const struct GCDetails_ *stats) {
  if (plhaskell_max_memory &&
      stats->mem_in_use_bytes > (uint64_t)0x400 * plhaskell_max_memory)
    ereport(FATAL, errcode(ERRCODE_OUT_OF_MEMORY),
            errmsg("Haskell RTS exceeded maximum memory. (%d kB)",
                   plhaskell_max_memory));
}

// Used by Haskell
void plhaskell_report(int elevel, char *msg)
    __attribute__((visibility("hidden")));
void plhaskell_report(int elevel, char *msg) {
  ereport(elevel, errmsg_internal("%s", msg));
}

static int rts_msg_fn(int elevel, const char *s, va_list ap) {
  char *buf;
  int len;
  va_list apc;
  va_copy(apc, ap);

  len = vsnprintf(NULL, 0, s, ap);
  buf = palloc(len);
  vsnprintf(buf, len, s, apc);

  language_error(elevel, buf);
  pfree(buf);

  return len;
}

#if __GLASGOW_HASKELL__ >= 902
static int rts_debug_msg_fn(const char *s, va_list ap) {
  return rts_msg_fn(DEBUG1, s, ap);
}
#else
static void rts_debug_msg_fn(const char *s, va_list ap) {
  rts_msg_fn(DEBUG1, s, ap);
}
#endif

static void rts_fatal_msg_fn(const char *s, va_list ap) {
  rts_msg_fn(FATAL, s, ap);
}

// Functions to handle TypeInfo for SPI querying
struct TypeInfo *new_type_info(Oid type_oid)
    __attribute__((visibility("hidden")));
struct TypeInfo *new_type_info(Oid type_oid) {
  struct TypeInfo *p_type_info = palloc0(sizeof(struct TypeInfo));
  build_type_info(p_type_info, type_oid, true);
  return p_type_info;
}

// Recusively free TypeInfo
void delete_type_info(struct TypeInfo *p_type_info)
    __attribute__((visibility("hidden")));
void delete_type_info(struct TypeInfo *p_type_info) {
  switch (p_type_info->value_type) {
  case COMPOSITE_TYPE:
    pfree(p_type_info->attnums);
    pfree(p_type_info->tupdesc);

    for (int16 i = 0; i < p_type_info->count; i++)
      delete_type_info(p_type_info->fields[i]);

    pfree(p_type_info->fields);

    break;
  case ARRAY_TYPE:
    delete_type_info(p_type_info->element);

    break;
  }

  if (p_type_info->nspname)
    pfree(p_type_info->nspname);

  if (p_type_info->typname)
    pfree(p_type_info->typname);

  pfree(p_type_info);
}

// Execute an SPI query
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
                                   current_p_call_info->spi_read_only, 0);
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

void free_tuptable(struct SPITupleTable *tuptable)
    __attribute__((visibility("hidden")));
void free_tuptable(struct SPITupleTable *tuptable) {
  SPI_freetuptable(tuptable);
}

// Delete temp files from all active CallInfos
static void mod_exit(int _code, Datum arg) {
  for (struct CallInfo *p_call_info = first_p_call_info; p_call_info;
       p_call_info = p_call_info->next)
    if (p_call_info->return_set) {
      if (p_call_info->list) {
        hs_free_stable_ptr(p_call_info->list);
        p_call_info->list = NULL;
      }
    } else {
      if (p_call_info->function) {
        hs_free_fun_ptr((HsFunPtr)(p_call_info->function));
        p_call_info->function = NULL;
      }
    }

  hs_exit();

  for (struct CallInfo *p_call_info = first_p_call_info; p_call_info;
       p_call_info = p_call_info->next)
    if (p_call_info->mod_file_name) {
      unlink(p_call_info->mod_file_name);
      pfree(p_call_info->mod_file_name);
      p_call_info->mod_file_name = NULL;
    }
}

PG_FUNCTION_INFO_V1(ghc_version);
Datum ghc_version(PG_FUNCTION_ARGS) { PG_RETURN_INT32(__GLASGOW_HASKELL__); }

Oid get_oid(bool array, char *nspname, char *typname)
    __attribute__((visibility("hidden")));
Oid get_oid(bool array, char *nspname, char *typname) {
  bool is_null;
  HeapTuple nsptup, typtup;
  Datum nspoid, ret_val;

  nsptup = SearchSysCache1(NAMESPACENAME, CStringGetDatum(nspname));
  if (!HeapTupleIsValid(nsptup))
    ereport(ERROR, errcode(ERRCODE_INVALID_SCHEMA_NAME),
            errmsg("schema \"%s\" does not exist", nspname));

  nspoid =
      SysCacheGetAttr(NAMESPACENAME, nsptup, Anum_pg_namespace_oid, &is_null);
  if (is_null)
    ereport(ERROR, errmsg_internal("pg_namespace.oid is NULL"));

  typtup = SearchSysCache2(TYPENAMENSP, CStringGetDatum(typname), nspoid);
  if (!HeapTupleIsValid(typtup))
    ereport(ERROR, errcode(ERRCODE_UNDEFINED_OBJECT),
            errmsg("type \"%s.%s\" does not exist", nspname, typname));

  if (array) {
    ret_val =
        SysCacheGetAttr(TYPENAMENSP, typtup, Anum_pg_type_typarray, &is_null);
    if (is_null)
      ereport(ERROR, errmsg_internal("pg_type.typarray is NULL"));

    if (ret_val == InvalidOid)
      ereport(ERROR, errcode(ERRCODE_UNDEFINED_OBJECT),
              errmsg("type \"%s.%s[]\" does not exist", nspname, typname));
  } else {
    ret_val = SysCacheGetAttr(TYPENAMENSP, typtup, Anum_pg_type_oid, &is_null);
    if (is_null)
      ereport(ERROR, errmsg_internal("pg_type.oid is NULL"));
  }

  ReleaseSysCache(typtup);
  ReleaseSysCache(nsptup);

  return DatumGetObjectId(ret_val);
}

Oid find_oid(bool array, char *typname) __attribute__((visibility("hidden")));
Oid find_oid(bool array, char *typname) {
  bool is_null;
  HeapTuple typtup;
  Datum ret_val;
  ListCell *nslc;

  List *namespacelist = fetch_search_path(true);
  foreach (nslc, namespacelist) {
    typtup = SearchSysCache2(TYPENAMENSP, CStringGetDatum(typname),
                             ObjectIdGetDatum(nslc->oid_value));
    if (HeapTupleIsValid(typtup)) {
      if (array) {
        ret_val = SysCacheGetAttr(TYPENAMENSP, typtup, Anum_pg_type_typarray,
                                  &is_null);
        if (is_null)
          ereport(ERROR, errmsg_internal("pg_type.typarray is NULL"));

        if (ret_val == InvalidOid)
          ereport(ERROR, errcode(ERRCODE_UNDEFINED_OBJECT),
                  errmsg("type \"%s[]\" does not exist", typname));
      } else {
        ret_val =
            SysCacheGetAttr(TYPENAMENSP, typtup, Anum_pg_type_oid, &is_null);
        if (is_null)
          ereport(ERROR, errmsg_internal("pg_type.oid is NULL"));
      }

      ReleaseSysCache(typtup);

      return DatumGetObjectId(ret_val);
    }
  }

  ereport(ERROR, errcode(ERRCODE_UNDEFINED_OBJECT),
          errmsg("type \"%s\" does not exist", typname));
  return InvalidOid; // Never reached
}

Datum detoast_datum(Datum datum) __attribute__((visibility("hidden")));
Datum detoast_datum(Datum datum) {
  return (Datum)pg_detoast_datum((struct varlena *)((Pointer)datum));
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

void expected_type_in_query(struct TypeInfo *p_type_info)
    __attribute__((visibility("hidden")));
void expected_type_in_query(struct TypeInfo *p_type_info) {
  ereport(ERROR, errmsg("Expected type \"%s.%s\" in query",
                        p_type_info->nspname, p_type_info->typname));
}

void incorrect_length(struct TypeInfo *p_type_info)
    __attribute__((visibility("hidden")));
void incorrect_length(struct TypeInfo *p_type_info) {
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
  ereport(ERROR,
          errmsg("PL/Haskell: %s: %s", current_p_call_info->func_name, msg));

  PG_RETURN_VOID();
}

bool date_read(DateADT *date, char *buf) __attribute__((visibility("hidden")));
bool date_read(DateADT *date, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int tzp;
  int dtype;
  int nf;
  int dterr;
  char *field[MAXDATEFIELDS];
  int ftype[MAXDATEFIELDS];
  char workbuf[MAXDATELEN + 1];
  DateTimeErrorExtra extra;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);
  if (dterr == 0)
    dterr = DecodeDateTime(field, ftype, nf, &dtype, tm, &fsec, &tzp, &extra);

  if (dterr != 0)
    return false;

  switch (dtype) {
  case DTK_DATE:
    break;

  case DTK_EPOCH:
    GetEpochTime(tm);
    break;

  case DTK_LATE:
    DATE_NOEND(*date);
    return true;

  case DTK_EARLY:
    DATE_NOBEGIN(*date);
    return true;

  default:
    return false;
  }

  /* Prevent overflow in Julian-day routines */
  if (!IS_VALID_JULIAN(tm->tm_year, tm->tm_mon, tm->tm_mday))
    return false;

  *date = date2j(tm->tm_year, tm->tm_mon, tm->tm_mday) - POSTGRES_EPOCH_JDATE;

  /* Now check for just-out-of-range dates */
  if (!IS_VALID_DATE(*date))
    return false;

  return true;
}

void date_show(DateADT date, char *buf) __attribute__((visibility("hidden")));
void date_show(DateADT date, char *buf) {
  struct pg_tm tt, *tm = &tt;

  j2date(date + POSTGRES_EPOCH_JDATE, &(tm->tm_year), &(tm->tm_mon),
         &(tm->tm_mday));

  EncodeDateOnly(tm, USE_ISO_DATES, buf);
}

bool time_read(TimeADT *time, char *buf) __attribute__((visibility("hidden")));
bool time_read(TimeADT *time, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int nf;
  int dterr;
  char workbuf[MAXDATELEN + 1];
  char *field[MAXDATEFIELDS];
  int dtype;
  int ftype[MAXDATEFIELDS];
  DateTimeErrorExtra extra;
  bool has_tz = false;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);

  if (dterr == 0)
    dterr = DecodeTimeOnly(field, ftype, nf, &dtype, tm, &fsec, NULL, &extra);
  if (dterr != 0)
    return false;

  for (int i = 0; i < nf; i++)
    if (ftype[i] == DTK_TZ) {
      has_tz = true;
      break;
    }

  if (has_tz)
    return false;

  tm2time(tm, fsec, time);

  return true;
}

void time_show(TimeADT time, char *buf) __attribute__((visibility("hidden")));
void time_show(TimeADT time, char *buf) {
  struct pg_tm tt, *tm = &tt;
  fsec_t fsec;

  time2tm(time, tm, &fsec);
  EncodeTimeOnly(tm, fsec, false, 0, USE_ISO_DATES, buf);
}

bool timetz_read(TimeTzADT *timetz, char *buf)
    __attribute__((visibility("hidden")));
bool timetz_read(TimeTzADT *timetz, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int tz;
  int nf;
  int dterr;
  char workbuf[MAXDATELEN + 1];
  char *field[MAXDATEFIELDS];
  int dtype;
  int ftype[MAXDATEFIELDS];
  DateTimeErrorExtra extra;
  bool has_tz = false;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);

  for (int i = 0; i < nf; i++)
    if (ftype[i] == DTK_TZ) {
      has_tz = true;
      break;
    }

  if (!has_tz)
    return false;

  if (dterr == 0)
    dterr = DecodeTimeOnly(field, ftype, nf, &dtype, tm, &fsec, &tz, &extra);
  if (dterr != 0)
    return false;

  tm2timetz(tm, fsec, tz, timetz);

  return true;
}

void timetz_show(TimeTzADT *timetz, char *buf)
    __attribute__((visibility("hidden")));
void timetz_show(TimeTzADT *timetz, char *buf) {
  struct pg_tm tt, *tm = &tt;
  fsec_t fsec;
  int tz;

  timetz2tm(timetz, tm, &fsec, &tz);
  EncodeTimeOnly(tm, fsec, true, tz, USE_ISO_DATES, buf);
}

bool timestamp_read(Timestamp *timestamp, char *buf)
    __attribute__((visibility("hidden")));
bool timestamp_read(Timestamp *timestamp, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int tz;
  int dtype;
  int nf;
  int dterr;
  char *field[MAXDATEFIELDS];
  int ftype[MAXDATEFIELDS];
  char workbuf[MAXDATELEN + MAXDATEFIELDS];
  DateTimeErrorExtra extra;
  bool has_tz = false;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);

  if (dterr == 0)
    dterr = DecodeDateTime(field, ftype, nf, &dtype, tm, &fsec, &tz, &extra);
  if (dterr != 0)
    return false;

  switch (dtype) {
  case DTK_DATE:
    for (int i = 0; i < nf; i++)
      if (ftype[i] == DTK_TZ) {
        has_tz = true;
        break;
      }

    if (has_tz)
      return false;

    if (tm2timestamp(tm, fsec, NULL, timestamp) != 0)
      return false;

    break;

  case DTK_EPOCH:
    GetEpochTime(tm);
    tm2timestamp(tm, 0, NULL, timestamp);
    break;

  case DTK_LATE:
    TIMESTAMP_NOEND(*timestamp);
    break;

  case DTK_EARLY:
    TIMESTAMP_NOBEGIN(*timestamp);
    break;

  default:
    return false;
  }

  return true;
}

void timestamp_show(Timestamp timestamp, char *buf)
    __attribute__((visibility("hidden")));
void timestamp_show(Timestamp timestamp, char *buf) {
  struct pg_tm tt, *tm = &tt;
  fsec_t fsec;

  if (timestamp2tm(timestamp, NULL, tm, &fsec, NULL, NULL) == 0)
    EncodeDateTime(tm, fsec, false, 0, NULL, USE_ISO_DATES, buf);
  else
    ereport(ERROR, (errcode(ERRCODE_DATETIME_VALUE_OUT_OF_RANGE),
                    errmsg("timestamp out of range")));
}

bool timestamptz_read(TimestampTz *timestamptz, char *buf)
    __attribute__((visibility("hidden")));
bool timestamptz_read(TimestampTz *timestamptz, char *buf) {
  fsec_t fsec;
  struct pg_tm tt, *tm = &tt;
  int tz;
  int dtype;
  int nf;
  int dterr;
  char *field[MAXDATEFIELDS];
  int ftype[MAXDATEFIELDS];
  char workbuf[MAXDATELEN + MAXDATEFIELDS];
  DateTimeErrorExtra extra;
  bool has_tz = false;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);
  if (dterr == 0)
    dterr = DecodeDateTime(field, ftype, nf, &dtype, tm, &fsec, &tz, &extra);

  if (dterr != 0) {
    return false;
  }

  switch (dtype) {
  case DTK_DATE:
    for (int i = 0; i < nf; i++)
      if (ftype[i] == DTK_TZ) {
        has_tz = true;
        break;
      }

    if (!has_tz)
      return false;

    if (tm2timestamp(tm, fsec, &tz, timestamptz) != 0)
      return false;

    break;

  case DTK_EPOCH:
    GetEpochTime(tm);
    tm2timestamp(tm, 0, NULL, timestamptz);
    break;

  case DTK_LATE:
    TIMESTAMP_NOEND(*timestamptz);
    break;

  case DTK_EARLY:
    TIMESTAMP_NOBEGIN(*timestamptz);
    break;

  default:
    return false;
  }

  return true;
}

void timestamptz_show(TimestampTz timestamptz, char *buf)
    __attribute__((visibility("hidden")));
void timestamptz_show(TimestampTz timestamptz, char *buf) {
  int tz;
  struct pg_tm tt, *tm = &tt;
  fsec_t fsec;
  const char *tzn;
  pg_tz *attimezone = pg_tzset("GMT");

  if (timestamp2tm(timestamptz, &tz, tm, &fsec, &tzn, attimezone) == 0)
    EncodeDateTime(tm, fsec, true, tz, tzn, USE_ISO_DATES, buf);
  else
    ereport(ERROR, (errcode(ERRCODE_DATETIME_VALUE_OUT_OF_RANGE),
                    errmsg("timestamp out of range")));
}

bool interval_read(Interval *interval, char *buf)
    __attribute__((visibility("hidden")));
bool interval_read(Interval *interval, char *buf) {
  struct pg_itm_in tt, *itm_in = &tt;
  int dtype;
  int nf;
  int range;
  int dterr;
  char *field[MAXDATEFIELDS];
  int ftype[MAXDATEFIELDS];
  char workbuf[256];

  itm_in->tm_year = 0;
  itm_in->tm_mon = 0;
  itm_in->tm_mday = 0;
  itm_in->tm_usec = 0;

  range = INTERVAL_FULL_RANGE;

  dterr = ParseDateTime(buf, workbuf, sizeof(workbuf), field, ftype,
                        MAXDATEFIELDS, &nf);
  if (dterr == 0)
    dterr = DecodeInterval(field, ftype, nf, range, &dtype, itm_in);

  /* if those functions think it's a bad format, try ISO8601 style */
  if (dterr == DTERR_BAD_FORMAT)
    dterr = DecodeISO8601Interval(buf, &dtype, itm_in);

  if (dterr != 0)
    return false;

  switch (dtype) {
  case DTK_DELTA:
    if (itmin2interval(itm_in, interval) != 0)
      return false;

    break;

  case DTK_LATE:
  case DTK_EARLY:
    return false;

  default:
    return false;
  }

  return true;
}

void interval_show(Interval *interval, char *buf)
    __attribute__((visibility("hidden")));
void interval_show(Interval *interval, char *buf) {
  struct pg_itm tt, *itm = &tt;

  interval2itm(*interval, itm);
  EncodeInterval(itm, INTSTYLE_POSTGRES, buf);
}
