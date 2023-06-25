// This is a "procedural language" extension of PostgreSQL
// allowing the execution of code in Haskell within SQL code.
//
// Copyright (C) 2023 Edward F. Behn, Jr.
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

#include "postgres.h"
#include "catalog/pg_attribute_d.h"
#include "catalog/pg_class_d.h"
#include "catalog/pg_language_d.h"
#include "catalog/pg_proc_d.h"
#include "catalog/pg_tablespace_d.h"
#include "catalog/pg_type_d.h"
#include "utils/guc.h"
#include "utils/syscache.h"
#include "utils/typcache.h"
#include "storage/ipc.h"

extern char pkglib_path[];

static void build_call_info(struct CallInfo *p_call_info, Oid funcoid, bool return_set);
static void build_value_info(struct ValueInfo *p_value_info, Oid typeoid);

static void destroy_call_info(void *arg);

static void write_value_info(struct ValueInfo *p_value_info, Datum value, bool is_null);
static Datum read_value_info(struct ValueInfo *p_value_info, bool *is_null);

static void enter(void);
static void gcDoneHook(const struct GCDetails_ *stats);

static void call_function(void *p_call_info);

static int rts_msg_fn(int elevel, const char *s, va_list ap);

#if __GLASGOW_HASKELL__ >= 902
static int rts_debug_msg_fn(const char *s, va_list ap);
#else
static void rts_debug_msg_fn(const char *s, va_list ap);
#endif

static void rts_fatal_msg_fn(const char *s, va_list ap);

static void unlink_all(int code, Datum arg);

void call_wrapper(void (*func)(void*), struct CallInfo *p_call_info);

static struct CallInfo *current_p_call_info = NULL;
static struct CallInfo *first_p_call_info = NULL; // Points to list of all active CallInfos

static int plhaskell_max_memory;

PG_MODULE_MAGIC;

// Main handler
PG_FUNCTION_INFO_V1(plhaskell_call_handler);
Datum plhaskell_call_handler(PG_FUNCTION_ARGS)
{
    struct CallInfo *p_call_info;
    Oid funcoid = fcinfo->flinfo->fn_oid; // OID of the function being handled
    HeapTuple proctup;
    Datum proretset;
    bool is_null;
    int spi_code;

    proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(funcoid));
    if(!HeapTupleIsValid(proctup))
        ereport(ERROR, errmsg("Cache lookup failed for function %u", funcoid));

    proretset = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proretset, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.proretset is NULL"));

    ReleaseSysCache(proctup);

    // If we're returning a set
    if(DatumGetBool(proretset))
    {
        FuncCallContext *funcctx;
        ReturnSetInfo *rsi = (ReturnSetInfo*)fcinfo->resultinfo;

        if(fcinfo->flinfo->fn_extra == NULL)
        {
            MemoryContext old_context;
            MemoryContextCallback *cb;

            if(!(rsi->allowedModes & SFRM_ValuePerCall))
                ereport(ERROR, errmsg("Bad return mode"));

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

            build_call_info(p_call_info, funcoid, true);

            // Write the argument to their ValueInfo structs
            for(int16 i=0; i<p_call_info->nargs; i++)
                write_value_info(p_call_info->args[i], fcinfo->args[i].value, fcinfo->args[i].isnull);

            spi_code = SPI_connect();
            if(spi_code < 0)
                ereport(ERROR, errmsg("%s", SPI_result_code_string(spi_code)));

            // Setup the iterator and list
            call_wrapper(mk_iterator, p_call_info);

            spi_code = SPI_finish();
            if(spi_code < 0)
                ereport(ERROR, errmsg("%s", SPI_result_code_string(spi_code)));

            MemoryContextSwitchTo(old_context);
        }

        funcctx = per_MultiFuncCall(fcinfo);
        p_call_info = funcctx->user_fctx;

        spi_code = SPI_connect();
        if(spi_code < 0)
            ereport(ERROR, errmsg("%s", SPI_result_code_string(spi_code)));

        // Run the function
        call_wrapper(call_function, p_call_info);

        spi_code = SPI_finish();
        if(spi_code < 0)
            ereport(ERROR, errmsg("%s", SPI_result_code_string(spi_code)));

        if(p_call_info->list) // Is there another result?
        {
            rsi->isDone = ExprMultipleResult;
            return read_value_info(p_call_info->result, &fcinfo->isnull); // Get the next result
        }
        else
        {
            end_MultiFuncCall(fcinfo, funcctx);
            rsi->isDone = ExprEndResult;

            fcinfo->isnull = true;
            return (Datum)0;
        }
    }
    else
    {
        if(fcinfo->flinfo->fn_extra == NULL)
        {
            MemoryContext old_context = MemoryContextSwitchTo(fcinfo->flinfo->fn_mcxt);
            MemoryContextCallback *cb = palloc(sizeof(MemoryContextCallback));

            // Allocate, setup, and Build CallInfo
            p_call_info = fcinfo->flinfo->fn_extra = palloc0(sizeof(struct CallInfo));

            // Register callback to free function and delete temp module file
            cb->func = destroy_call_info;
            cb->arg = p_call_info;
            MemoryContextRegisterResetCallback(fcinfo->flinfo->fn_mcxt, cb);

            build_call_info(p_call_info, funcoid, false);

            // Make the function
            call_wrapper(mk_function, p_call_info);

            MemoryContextSwitchTo(old_context);
        }

        p_call_info = fcinfo->flinfo->fn_extra;

        // Write the argument to their ValueInfo structs
        for(int16 i=0; i<p_call_info->nargs; i++)
            write_value_info(p_call_info->args[i], fcinfo->args[i].value, fcinfo->args[i].isnull);

        spi_code = SPI_connect();
        if(spi_code < 0)
            ereport(ERROR, errmsg("%s", SPI_result_code_string(spi_code)));

        // Run the function
        call_wrapper(call_function, p_call_info);

        spi_code = SPI_finish();
        if(spi_code < 0)
            ereport(ERROR, errmsg("%s", SPI_result_code_string(spi_code)));

        return read_value_info(p_call_info->result, &fcinfo->isnull);
    }
}

// Called when PL/Haskell function is created
PG_FUNCTION_INFO_V1(plhaskell_validator);
Datum plhaskell_validator(PG_FUNCTION_ARGS)
{
    struct CallInfo *p_call_info;
    Oid funcoid = PG_GETARG_OID(0);
    MemoryContextCallback *cb;

    HeapTuple proctup;
    Datum proretset;
    bool is_null;

    if (!CheckFunctionValidatorAccess(fcinfo->flinfo->fn_oid, funcoid))
        PG_RETURN_VOID();

    if (!check_function_bodies)
        PG_RETURN_VOID();

    proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(funcoid));
    if(!HeapTupleIsValid(proctup))
        ereport(ERROR, errmsg("Cache lookup failed for function %u", funcoid));

    proretset = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proretset, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.proretset is NULL"));

    ReleaseSysCache(proctup);

    cb = palloc(sizeof(MemoryContextCallback));

    // Allocate, setup, and Build CallInfo
    p_call_info = palloc0(sizeof(struct CallInfo));

    // Register callback to delete temp module file
    cb->func = destroy_call_info;
    cb->arg = p_call_info;
    MemoryContextRegisterResetCallback(CurrentMemoryContext, cb);

    build_call_info(p_call_info, funcoid, DatumGetBool(proretset));

    // Raise error if the function's signature is incorrect
    call_wrapper(check_signature, p_call_info);

    PG_RETURN_VOID();
}

// Fill CallInfo struct
static void build_call_info(struct CallInfo *p_call_info, Oid funcoid, bool return_set)
{
    HeapTuple proctup, lantup;
    Datum provariadic, prokind, prorettype, proargtypes, prosrc, proname, provolatile, proparallel, prolang, lanpltrusted;
    ArrayType *proargtypes_arr;
    Oid *argtypes;
    bool is_null;
    text *src;
    char tempdirpath[MAXPGPATH];
    FILE *modfile;

    p_call_info->return_set = return_set;

    proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(funcoid));
    if(!HeapTupleIsValid(proctup))
        ereport(ERROR, errmsg("Cache lookup failed for function %u", funcoid));

    prolang = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prolang, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.prolang is NULL"));

    lantup = SearchSysCache1(LANGOID, prolang);

    lanpltrusted = SysCacheGetAttr(LANGOID, lantup, Anum_pg_language_lanpltrusted, &is_null);
    if(is_null)
           ereport(ERROR, errmsg("pg_language.lanpltrusted is NULL"));

    p_call_info->trusted = DatumGetBool(lanpltrusted);

    ReleaseSysCache(lantup);

    provariadic = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_provariadic, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.provariadic is NULL"));

    if(DatumGetObjectId(provariadic) != 0)
        ereport(ERROR, errmsg("PL/Haskell : Variadic types not allowed"));

    prokind = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prokind, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.prokind is NULL"));

    if(DatumGetChar(prokind) != PROKIND_FUNCTION)
        ereport(ERROR, errmsg("PL/Haskell : Only normal function allowed"));

    provolatile = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_provolatile, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.provolatile is NULL"));

    p_call_info->spi_read_only = DatumGetChar(provolatile) != PROVOLATILE_VOLATILE;

    p_call_info->nargs = DatumGetInt16(SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_pronargs, &is_null));
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.pronargs is NULL"));

    p_call_info->args = palloc(p_call_info->nargs * sizeof(struct ValueInfo*));

    SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proargmodes, &is_null);
    if(!is_null)
        ereport(ERROR, errmsg("PL/Haskell : Only IN arguments allowed"));

    prorettype = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prorettype, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.prorettype is NULL"));

    proparallel = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proparallel, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.proparallel is NULL"));

    if(DatumGetChar(proparallel) != PROPARALLEL_UNSAFE)
        ereport(ERROR, errmsg("PL/Haksell : Function must be parallel unsafe"));

    p_call_info->result = palloc(sizeof(struct ValueInfo));
    build_value_info(p_call_info->result, prorettype);

    proargtypes = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proargtypes, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.proargtypes is NULL"));

    proargtypes_arr = DatumGetArrayTypeP(proargtypes);

    if(ARR_NDIM(proargtypes_arr) != 1)
        ereport(ERROR, errmsg("pg_proc.proargtypes has %d dimensions", ARR_NDIM(proargtypes_arr)));

    if(ARR_LBOUND(proargtypes_arr)[0] != 0 || ARR_DIMS(proargtypes_arr)[0] != p_call_info->nargs)
        ereport(ERROR, errmsg("pg_proc.proargtypes has unexpected size"));

    if(ARR_NULLBITMAP(proargtypes_arr) != NULL)
        ereport(ERROR, errmsg("pg_proc.proargtypes has NULL element"));

    argtypes = (Oid*)ARR_DATA_PTR(proargtypes_arr);
    for(int16 i=0; i<p_call_info->nargs; i++)
    {
        p_call_info->args[i] = palloc(sizeof(struct ValueInfo));
        build_value_info(p_call_info->args[i], argtypes[i]);
    }

    proname = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proname, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.proname is NULL"));

    p_call_info->func_name = palloc(NAMEDATALEN);
    memcpy(p_call_info->func_name, DatumGetName(proname)->data, NAMEDATALEN);

    // Fill temp file with function source
    prosrc = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prosrc, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_proc.prosrc is NULL"));

    src = DatumGetTextPP(prosrc);

    TempTablespacePath(tempdirpath, DEFAULTTABLESPACE_OID);
    p_call_info->mod_file_name = palloc0(MAXPGPATH);
    snprintf(p_call_info->mod_file_name, MAXPGPATH, "%s/ModXXXXXX.hs", tempdirpath);
    modfile = fdopen(mkstemps(p_call_info->mod_file_name, 3), "w");
    if(!modfile)
        ereport(ERROR, errmsg("Unable to create temporary file (%s)", p_call_info->mod_file_name));

    fprintf(modfile, "module PGmodule (%s) where\n", p_call_info->func_name);
    fwrite(VARDATA_ANY(src), VARSIZE_ANY_EXHDR(src), 1, modfile);
    fclose(modfile);

    ReleaseSysCache(proctup);

    // Add p_call_info to the list of all active CallInfos
    if(first_p_call_info)
    {
        p_call_info->next = first_p_call_info;
        first_p_call_info->prev = p_call_info;
    }

    first_p_call_info = p_call_info;
}

// Fill ValueInfo struct
static void build_value_info(struct ValueInfo *p_value_info, Oid type_oid)
{
    HeapTuple typetup, reltup, atttup;
    Datum typtype, typname, typbasetype, typrelid;
    Datum relnatts;
    Datum atttypid;
    char *type_name;
    bool is_null;
    Oid classoid, attroid;

    if(type_oid == VOIDOID)
    {
        p_value_info->type = VOID_TYPE;
        return;
    }

    typetup = SearchSysCache1(TYPEOID, ObjectIdGetDatum(type_oid));
    if(!HeapTupleIsValid(typetup))
        ereport(ERROR, errmsg("cache lookup failed for type %u", type_oid));

    typname = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typname, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_type.typname is NULL"));

    type_name = DatumGetCString(typname);

    typtype = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typtype, &is_null);
    if(is_null)
        ereport(ERROR, errmsg("pg_type.typtype is NULL"));

    switch(DatumGetChar(typtype))
    {
    case TYPTYPE_BASE :
        if(!type_available(type_oid))
            ereport(ERROR, errmsg("PL/Haskell does not support type %s", type_name));

        p_value_info->type_oid = type_oid;
        p_value_info->type = BASE_TYPE;

        break;
    case TYPTYPE_COMPOSITE :
        p_value_info->type = COMPOSITE_TYPE;
        p_value_info->count = 0;
        p_value_info->attnums = palloc(0);
        p_value_info->fields = palloc(0);

        typrelid = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typrelid, &is_null);
        if(is_null)
            ereport(ERROR, errmsg("pg_type.typrelid is NULL"));

        classoid = ObjectIdGetDatum(typrelid);

        reltup = SearchSysCache1(RELOID, ObjectIdGetDatum(classoid));
        if(!HeapTupleIsValid(reltup))
            ereport(ERROR, errmsg("cache lookup failed for class %u", classoid));

        relnatts = SysCacheGetAttr(RELOID, reltup, Anum_pg_class_relnatts, &is_null);
        if(is_null)
            ereport(ERROR, errmsg("pg_class.relnatts is NULL"));

        p_value_info->natts = DatumGetInt16(relnatts);

        // Loop over all composite type attributes
        for(int16 i=0; i<p_value_info->natts; i++)
        {
            atttup = SearchSysCache2(ATTNUM, ObjectIdGetDatum(classoid), Int16GetDatum(i+1));
            if(!HeapTupleIsValid(atttup))
                ereport(ERROR, errmsg("cache lookup failed for attribute %u attnum %d", classoid, i+1));

            atttypid = SysCacheGetAttr(ATTNUM, atttup, Anum_pg_attribute_atttypid, &is_null);
            if(is_null)
                ereport(ERROR, errmsg("pg_attribute.atttypid is NULL"));

            // If the attribute is not dropped
            if((attroid = DatumGetObjectId(atttypid)))
            {
                p_value_info->count++;

                if(p_value_info->count > 63)
                    ereport(ERROR, errmsg("PL/Haskell : Tuple size too large (%s)", type_name));

                p_value_info->attnums = repalloc(p_value_info->attnums, p_value_info->count * sizeof(int16));
                p_value_info->attnums[p_value_info->count-1] = i+1;

                p_value_info->fields = repalloc(p_value_info->fields, p_value_info->count * sizeof(struct ValueInfo*));
                p_value_info->fields[p_value_info->count-1] = palloc(sizeof(struct ValueInfo));
                build_value_info(p_value_info->fields[p_value_info->count-1], attroid);
            }

            ReleaseSysCache(atttup);
        }

        ReleaseSysCache(reltup);

        p_value_info->tupdesc = lookup_rowtype_tupdesc_copy(type_oid, -1);
        BlessTupleDesc(p_value_info->tupdesc);

        break;
    case TYPTYPE_DOMAIN :
        typbasetype = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typbasetype, &is_null);
        if(is_null)
            ereport(ERROR, errmsg("pg_type.typbasetype is NULL"));

        build_value_info(p_value_info, DatumGetObjectId(typbasetype));

        break;
    case TYPTYPE_ENUM :
        ereport(ERROR, errmsg("PL/Haskell does not support enumerated types (%s)", type_name));
        break;
#ifdef TYPTYPE_MULTIRANGE
    case TYPTYPE_MULTIRANGE :
#endif
    case TYPTYPE_RANGE :
        ereport(ERROR, errmsg("PL/Haskell does not support range types (%s)", type_name));
        break;
    case TYPTYPE_PSEUDO :
        ereport(ERROR, errmsg("PL/Haskell does not support type %s", type_name));
        break;
    default :
        ereport(ERROR, errmsg("pg_type.typtype is invalid : %c", DatumGetChar(typtype)));
    }

    ReleaseSysCache(typetup);
}

// Delete temp module file and free function and List if necessary
static void destroy_call_info(void *arg)
{
    struct CallInfo *p_call_info = arg;

    if(p_call_info->mod_file_name)
        unlink(p_call_info->mod_file_name);

    if(p_call_info->list)
        hs_free_stable_ptr(p_call_info->list);

    if(p_call_info->function)
        hs_free_fun_ptr(p_call_info->function);

    if(first_p_call_info == p_call_info)
        first_p_call_info = p_call_info->next;

    if(p_call_info->prev)
        p_call_info->prev->next = p_call_info->next;

    if(p_call_info->next)
        p_call_info->next->prev = p_call_info->prev;
}

// Populate the value and is_null fields of p_value_info
static void write_value_info(struct ValueInfo *p_value_info, Datum value, bool is_null)
{
    HeapTupleHeader tuple;
    HeapTupleData tmptup;

    if((p_value_info->is_null=is_null))
        return;

    switch(p_value_info->type)
    {
    case BASE_TYPE :
        p_value_info->value = value;
        break;
    case COMPOSITE_TYPE :
        // If p_value_info is a tuple, populate the fields recursively.
        tuple = DatumGetHeapTupleHeader(value);

        tmptup.t_len = HeapTupleHeaderGetDatumLength(tuple);
        ItemPointerSetInvalid(&(tmptup.t_self));
        tmptup.t_tableOid = InvalidOid;
        tmptup.t_data = tuple;

        for(int16 i=0; i<p_value_info->count; i++)
        {
            Datum value = heap_getattr(&tmptup, p_value_info->attnums[i], p_value_info->tupdesc, &is_null);
            write_value_info(p_value_info->fields[i], value, is_null);
        }

        break;
    default :
        ereport(ERROR, errmsg("Bad Type : %d", p_value_info->type));
    }
}

// Get the value from p_value_info
static Datum read_value_info(struct ValueInfo *p_value_info, bool *is_null)
{
    Datum *values;
    bool *is_nulls;

    if((*is_null=p_value_info->is_null))
        return (Datum)0;

    switch(p_value_info->type)
    {
    case VOID_TYPE :
        return (Datum)0;
    case BASE_TYPE :
        return p_value_info->value;
    case COMPOSITE_TYPE :
        // If p_value_info is a tuple, get the fields' values recursively.
        values  = palloc(p_value_info->natts * sizeof(Datum));
        is_nulls = palloc(p_value_info->natts * sizeof(bool));

        for(int16 i=0; i<p_value_info->count; i++)
        {
            int16 attnum = p_value_info->attnums[i];
            values[attnum-1] = read_value_info(p_value_info->fields[i], is_nulls+(attnum-1));
        }

        return HeapTupleGetDatum(heap_form_tuple(p_value_info->tupdesc, values, is_nulls));
    default :
        ereport(ERROR, errmsg("Bad Type : %d", p_value_info->type));
    }
}

// Called when the module is loaded
static void enter(void) __attribute__((constructor));
static void enter(void)
{
    char GHC_PackagePath[MAXPGPATH+18];
    static char *argv[] = {"PLHaskell", "+RTS", "--install-signal-handlers=no", "-V0", "-RTS"}; // Configuration for the RTS
    static int argc = sizeof(argv) / sizeof(char*);
    static char **argv_rts = argv;
    RtsConfig conf = defaultRtsConfig;
    char tempdirpath[MAXPGPATH];

    on_proc_exit(unlink_all, (Datum)0);

    // Add the directory containing PGutils to the GHC Package Path
    snprintf(GHC_PackagePath, MAXPGPATH+18, "%s/plhaskell_pkg_db:", pkglib_path); // Note the colon
    setenv("GHC_PACKAGE_PATH", GHC_PackagePath, true);

    TempTablespacePath(tempdirpath, DEFAULTTABLESPACE_OID);
    MakePGDirectory(tempdirpath);
    setenv("TMPDIR", tempdirpath, true);

    DefineCustomIntVariable("plhaskell.max_memory",
                             gettext_noop("Maximum memory for PL/Haskell"),
                             gettext_noop("This is the maximum memory that can be used by the Haskell runtime system before a FATAL error."),
                             &plhaskell_max_memory,
                             131072, // 131072 kB = 128 MB
                             0, MAX_KILOBYTES,
                             PGC_USERSET,
                             GUC_UNIT_KB,
                             NULL, NULL, NULL);

    fatalInternalErrorFn = rts_fatal_msg_fn;
    debugMsgFn           = rts_debug_msg_fn;
    errorMsgFn           = rts_fatal_msg_fn;

    conf.rts_opts_enabled = RtsOptsAll;
    conf.gcDoneHook = gcDoneHook; // Called on every garbage collections to monitor memory usage
    hs_init_ghc(&argc, &argv_rts, conf);
}

// Called on every garbage collection to monitor memory usage
static void gcDoneHook(const struct GCDetails_ *stats)
{
    if(stats->mem_in_use_bytes > (uint64_t)0x400 * plhaskell_max_memory)
        ereport(FATAL, errmsg("Haskell RTS exceeded maximum memory. (%d kB)", plhaskell_max_memory));
}

// Used by Haskell
void plhaskell_report(int elevel, char *msg) __attribute__((visibility ("hidden")));
void plhaskell_report(int elevel, char *msg)
{
    char *filename_location;

    // Strip trailing new-line if necessary
    if(strlen(msg)>1 && msg[strlen(msg)-1] == '\n')
        msg[strlen(msg)-1] = '\0';

    filename_location = strstr(msg, current_p_call_info->mod_file_name);

    if(!filename_location)
        ereport(elevel, errmsg("%s", msg));
    else
    {
        int filename_length = strlen(current_p_call_info->mod_file_name);
        *filename_location = '\0';

        if(filename_location[filename_length]==':' && isdigit(filename_location[filename_length+1]))
        {
            char *i;
            for(i=filename_location+filename_length+1; isdigit(*i); i++);

            // Reduce line number by one to account for added line of code in file
            ereport(elevel, errmsg("%s%s:%d%s", msg, current_p_call_info->func_name, atoi(filename_location+filename_length+1)-1, i));
        }
        else
            ereport(elevel, errmsg("%s%s%s", msg, current_p_call_info->func_name, filename_location+filename_length));
    }
}

static void call_function(void *p_call_info)
{
    (*((struct CallInfo*)p_call_info)->function)();
}

static int rts_msg_fn(int elevel, const char *s, va_list ap)
{
    char *buf;
    int len;
    va_list apc;
    va_copy(apc, ap);

    len = vsnprintf(NULL, 0, s, ap);
    buf = palloc(len);
    vsnprintf(buf, len, s, apc);
    plhaskell_report(elevel, buf);
    pfree(buf);

    return len;
}

#if __GLASGOW_HASKELL__ >= 902
static int rts_debug_msg_fn(const char *s, va_list ap)
{
    return rts_msg_fn(DEBUG1, s, ap);
}
#else
static void rts_debug_msg_fn(const char *s, va_list ap)
{
    rts_msg_fn(DEBUG1, s, ap);
}
#endif

static void rts_fatal_msg_fn(const char *s, va_list ap)
{
    rts_msg_fn(FATAL, s, ap);
}

// Functions to handle ValueInfo for SPI querying
struct ValueInfo *new_value_info(Oid typeoid) __attribute__((visibility ("hidden")));
struct ValueInfo *new_value_info(Oid typeoid)
{
    struct ValueInfo *p_value_info = palloc(sizeof(struct ValueInfo));
    build_value_info(p_value_info, typeoid);
    return p_value_info;
}

// Recusively free ValueInfo
void delete_value_info(struct ValueInfo *p_value_info) __attribute__((visibility ("hidden")));
void delete_value_info(struct ValueInfo *p_value_info)
{
    if(p_value_info->type == COMPOSITE_TYPE)
    {
        pfree(p_value_info->attnums);
        pfree(p_value_info->tupdesc);

        for(int16 i=0; i<p_value_info->count; i++)
            delete_value_info(p_value_info->fields[i]);

        pfree(p_value_info->fields);
    }

    pfree(p_value_info);
}

// Execute an SPI query
int run_query(const char *command, int nargs, Oid *argtypes, Datum *values, bool *is_nulls) __attribute__((visibility ("hidden")));
int run_query(const char *command, int nargs, Oid *argtypes, Datum *values, bool *is_nulls)
{
    int spi_code;
    char nulls[nargs+1];

    for(int i=0; i<nargs; i++)
        nulls[i] = is_nulls[i]?'n':' ';

    // This is just to stop the compiler from complaining.
    nulls[nargs] = '\0';

    spi_code = SPI_execute_with_args(command, nargs, argtypes, values, nulls, current_p_call_info->spi_read_only, 0);
    if(spi_code < 0)
        ereport(ERROR, errmsg("%s", SPI_result_code_string(spi_code)));

    return spi_code;
}

void get_header_field(struct SPITupleTable *tuptable, char *header, int fnumber) __attribute__((visibility ("hidden")));
void get_header_field(struct SPITupleTable *tuptable, char *header, int fnumber)
{
    char *name = SPI_fname(tuptable->tupdesc, fnumber);
    strcpy(header, name);
    pfree(name);
}

// Get OID of types of columns from SPI query
void get_oids(struct SPITupleTable *tuptable, Oid *oids) __attribute__((visibility ("hidden")));
void get_oids(struct SPITupleTable *tuptable, Oid *oids)
{
    for(int i=0; i<tuptable->tupdesc->natts; i++)
        oids[i] = SPI_gettypeid(tuptable->tupdesc, i+1);
}

// Get datum/is_null from SPI result and use it to populate ValueInfo struct
void fill_value_info(struct SPITupleTable *tuptable, struct ValueInfo *p_value_info, uint64 row_number, int fnumber) __attribute__((visibility ("hidden")));
void fill_value_info(struct SPITupleTable *tuptable, struct ValueInfo *p_value_info, uint64 row_number, int fnumber)
{
    Datum value;
    bool is_null;

    value = SPI_getbinval(tuptable->vals[row_number], tuptable->tupdesc, fnumber, &is_null);
    write_value_info(p_value_info, value, is_null);
}

void free_tuptable(struct SPITupleTable *tuptable) __attribute__((visibility ("hidden")));
void free_tuptable(struct SPITupleTable *tuptable)
{
    SPI_freetuptable(tuptable);
}

// Delete temp files from all active CallInfos
static void unlink_all(int code, Datum arg)
{
    for(struct CallInfo *p_call_info = first_p_call_info; p_call_info; p_call_info = p_call_info->next)
        if(p_call_info->mod_file_name)
            unlink(p_call_info->mod_file_name);
}

PG_FUNCTION_INFO_V1(ghc_version);
Datum ghc_version(PG_FUNCTION_ARGS)
{
    PG_RETURN_INT32(hint_ghc_version());
}

void call_wrapper(void (*func)(void*), struct CallInfo *p_call_info)
{
    struct CallInfo *prev_p_call_info = current_p_call_info;
    current_p_call_info = p_call_info;
    (*func)(p_call_info); // Call the function
    current_p_call_info = prev_p_call_info;
}
