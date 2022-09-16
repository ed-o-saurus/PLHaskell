#include "plhaskell.h"
#include "PLHaskell_stub.h"

#include "Rts.h"

#include <unistd.h>

#include "postgres.h"
#include "catalog/pg_attribute_d.h"
#include "catalog/pg_class_d.h"
#include "catalog/pg_proc_d.h"
#include "catalog/pg_type_d.h"
#include "utils/guc.h"
#include "utils/syscache.h"
#include "utils/typcache.h"

extern char pkglib_path[];

void BuildCallInfo(struct CallInfo* pCallInfo, Oid funcoid, bool ReturnSet);
void BuildValueInfo(struct ValueInfo* pValueInfo, Oid typeoid);

void DestroyCallInfo(void* arg);

void WriteValueInfo(struct ValueInfo* pValueInfo, Datum value, bool isNull);
Datum ReadValueInfo(struct ValueInfo* pValueInfo, bool* isNull);

static void Enter(void);
void gcDoneHook(const struct GCDetails_ *stats);
void exit_function(void);

int stderr_pipe_fn;

PG_MODULE_MAGIC;

// Main handler
PG_FUNCTION_INFO_V1(plhaskell_call_handler);
Datum plhaskell_call_handler(PG_FUNCTION_ARGS)
{
    struct CallInfo* pCallInfo;
    Oid funcoid = fcinfo->flinfo->fn_oid; // OID of the function being handled
    HeapTuple proctup;
    Datum proretset;
    bool isNull;

    proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(funcoid));
    if(!HeapTupleIsValid(proctup))
        ereport(ERROR, errmsg("Cache lookup failed for function %u.", funcoid));

    proretset = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proretset, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.proretset is NULL."));

    ReleaseSysCache(proctup);

    // If we're returing a set
    if(DatumGetBool(proretset))
    {
        FuncCallContext *funcctx;

        if(SRF_IS_FIRSTCALL())
        {
            MemoryContext oldcontext;
            MemoryContextCallback *cb;

            funcctx = SRF_FIRSTCALL_INIT();
            oldcontext = MemoryContextSwitchTo(funcctx->multi_call_memory_ctx);

            cb = palloc(sizeof(MemoryContextCallback));

            // Allocate, setup, and Build CallInfo
            pCallInfo = funcctx->user_fctx = palloc0(sizeof(struct CallInfo));

            // Register callback to free List and Iterator and delete temp module file
            cb->func = DestroyCallInfo;
            cb->arg = pCallInfo;
            MemoryContextRegisterResetCallback(funcctx->multi_call_memory_ctx, cb);

            BuildCallInfo(pCallInfo, funcoid, true);

            // Write the argument to their ValueInfo structs
            for(short i=0; i<pCallInfo->nargs; i++)
                WriteValueInfo(pCallInfo->Args[i], fcinfo->args[i].value, fcinfo->args[i].isnull);

            mkIterator(pCallInfo); // Setup the iterator and list

            MemoryContextSwitchTo(oldcontext);
        }

        funcctx = SRF_PERCALL_SETUP();
        pCallInfo = funcctx->user_fctx;

        if(listEmpty(pCallInfo)) // Are there no more values to return?
            SRF_RETURN_DONE(funcctx);
        else
        {
            Datum result;

            runFunction(pCallInfo->Iterator); // Load next value into result ValueInfo struct

            result = ReadValueInfo(pCallInfo->Result, &isNull); // Get the next result

            if(isNull)
                SRF_RETURN_NEXT_NULL(funcctx);
            else
                SRF_RETURN_NEXT(funcctx, result);
        }
    }
    else
    {
        if(fcinfo->flinfo->fn_extra == NULL)
        {
            MemoryContext OldContext = MemoryContextSwitchTo(fcinfo->flinfo->fn_mcxt);
            MemoryContextCallback *cb = palloc(sizeof(MemoryContextCallback));

            // Allocate, setup, and Build CallInfo
            pCallInfo = fcinfo->flinfo->fn_extra = palloc0(sizeof(struct CallInfo));

            // Register callback to free Function and delete temp module file
            cb->func = DestroyCallInfo;
            cb->arg = pCallInfo;
            MemoryContextRegisterResetCallback(fcinfo->flinfo->fn_mcxt, cb);

            BuildCallInfo(pCallInfo, funcoid, false);
            mkFunction(pCallInfo);

            MemoryContextSwitchTo(OldContext);
        }

        pCallInfo = fcinfo->flinfo->fn_extra;

        // Write the argument to their ValueInfo structs
        for(short i=0; i<pCallInfo->nargs; i++)
            WriteValueInfo(pCallInfo->Args[i], fcinfo->args[i].value, fcinfo->args[i].isnull);

        runFunction(pCallInfo->Function); // Run the function

        return ReadValueInfo(pCallInfo->Result, &fcinfo->isnull);
    }
}

// Called when PL/Haskell function is created
PG_FUNCTION_INFO_V1(plhaskell_validator);
Datum plhaskell_validator(PG_FUNCTION_ARGS)
{
    struct CallInfo* pCallInfo;
    Oid funcoid = PG_GETARG_OID(0);
    MemoryContextCallback *cb;

    HeapTuple proctup;
    Datum proretset;
    bool isNull;

    if (!CheckFunctionValidatorAccess(fcinfo->flinfo->fn_oid, funcoid))
        PG_RETURN_VOID();

    if (!check_function_bodies)
        PG_RETURN_VOID();

    proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(funcoid));
    if(!HeapTupleIsValid(proctup))
        ereport(ERROR, errmsg("Cache lookup failed for function %u.", funcoid));

    proretset = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proretset, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.proretset is NULL."));

    ReleaseSysCache(proctup);

    cb = palloc(sizeof(MemoryContextCallback));

    // Allocate, setup, and Build CallInfo
    pCallInfo = palloc0(sizeof(struct CallInfo));

    // Register callback to delete temp module file
    cb->func = DestroyCallInfo;
    cb->arg = pCallInfo;
    MemoryContextRegisterResetCallback(CurrentMemoryContext, cb);

    BuildCallInfo(pCallInfo, funcoid, DatumGetBool(proretset));

    // Raise error if the function's signature is incorrect
    checkSignature(pCallInfo);

    PG_RETURN_VOID();
}

// Fill CallInfo struct
void BuildCallInfo(struct CallInfo* pCallInfo, Oid funcoid, bool ReturnSet)
{
    HeapTuple proctup;
    Datum provariadic, prokind, prorettype, proargtypes, prosrc, proname;
    ArrayType *proargtypes_arr;
    Oid* argtypes;
    bool isNull;
    text* src;
    int modfd;

    pCallInfo->ReturnSet = ReturnSet;

    proctup = SearchSysCache1(PROCOID, ObjectIdGetDatum(funcoid));
    if(!HeapTupleIsValid(proctup))
        ereport(ERROR, errmsg("Cache lookup failed for function %u.", funcoid));

    provariadic = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_provariadic, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.provariadic is NULL."));

    if(DatumGetObjectId(provariadic) != 0)
        ereport(ERROR, errmsg("Variadic types not allowed"));

    prokind = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prokind, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.prokind is NULL."));

    if(DatumGetChar(prokind) != PROKIND_FUNCTION)
        ereport(ERROR, errmsg("Only normal function allowed."));

    pCallInfo->nargs = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_pronargs, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.pronargs is NULL."));

    pCallInfo->Args = palloc(pCallInfo->nargs * sizeof(struct ValueInfo*));

    SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proargmodes, &isNull);
    if(!isNull)
        ereport(ERROR, errmsg("Only IN arguments allowed."));

    prorettype = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prorettype, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.prorettype is NULL."));

    pCallInfo->Result = palloc(sizeof(struct ValueInfo));
    BuildValueInfo(pCallInfo->Result, prorettype);

    proargtypes = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proargtypes, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.proargtypes is NULL."));

    proargtypes_arr = DatumGetArrayTypeP(proargtypes);

    if(ARR_NDIM(proargtypes_arr) != 1)
        ereport(ERROR, errmsg("pg_proc.proargtypes has %d dimensions.", ARR_NDIM(proargtypes_arr)));

    if(ARR_LBOUND(proargtypes_arr)[0] != 0 || ARR_DIMS(proargtypes_arr)[0] != pCallInfo->nargs)
        ereport(ERROR, errmsg("pg_proc.proargtypes has unexpected size"));

    if(ARR_NULLBITMAP(proargtypes_arr) != NULL)
        ereport(ERROR, errmsg("pg_proc.proargtypes has NULL element"));

    argtypes = (Oid*)ARR_DATA_PTR(proargtypes_arr);
    for(int i=0; i<pCallInfo->nargs; i++)
    {
        pCallInfo->Args[i] = palloc(sizeof(struct ValueInfo));
        BuildValueInfo(pCallInfo->Args[i], argtypes[i]);
    }

    proname = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_proname, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.proname is NULL."));

    pCallInfo->FuncName = palloc(NAMEDATALEN);
    memcpy(pCallInfo->FuncName, DatumGetName(proname)->data, NAMEDATALEN);

    // Fill temp file with function source
    prosrc = SysCacheGetAttr(PROCOID, proctup, Anum_pg_proc_prosrc, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_proc.prosrc is NULL."));

    src = DatumGetTextPP(prosrc);

    pCallInfo->ModFileName = palloc(18);
    memcpy(pCallInfo->ModFileName, "/tmp/ModXXXXXX.hs", 18);

    modfd = mkstemps(pCallInfo->ModFileName, 3);

    if(write(modfd, "module PGmodule (", 17) < 0)
    {
        close(modfd);
        ereport(ERROR, errmsg("Unable to write to module file."));
    }

    if(write(modfd, pCallInfo->FuncName, strlen(pCallInfo->FuncName)) < 0)
    {
        close(modfd);
        ereport(ERROR, errmsg("Unable to write to module file."));
    }

    if(write(modfd, ") where\n", 8) < 0)
    {
        close(modfd);
        ereport(ERROR, errmsg("Unable to write to module file."));
    }

    if(write(modfd, VARDATA_ANY(src), VARSIZE_ANY_EXHDR(src)) < 0)
    {
        close(modfd);
        ereport(ERROR, errmsg("Unable to write to module file."));
    }

    close(modfd);

    ReleaseSysCache(proctup);
}

// Fill ValueInfo struct
void BuildValueInfo(struct ValueInfo* pValueInfo, Oid TypeOid)
{
    HeapTuple typetup, reltup, atttup;
    Datum typtype, typname, typbasetype, typrelid, typbyval;
    Datum relnatts;
    Datum atttypid;
    char* type_name;
    bool isNull;
    Oid classoid, attroid;

    if(TypeOid == VOIDOID)
    {
        pValueInfo->Type = VOID_TYPE;
        return;
    }

    typetup = SearchSysCache1(TYPEOID, ObjectIdGetDatum(TypeOid));
    if(!HeapTupleIsValid(typetup))
        ereport(ERROR, errmsg("cache lookup failed for type %u.", TypeOid));

    typname = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typname, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_type.typname is NULL."));

    type_name = DatumGetCString(typname);

    typtype = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typtype, &isNull);
    if(isNull)
        ereport(ERROR, errmsg("pg_type.typtype is NULL."));

    switch(DatumGetChar(typtype))
    {
    case TYPTYPE_BASE :
        if(!TypeAvailable(TypeOid))
            ereport(ERROR, errmsg("PL/Haskell does not support type %s.", type_name));

        pValueInfo->TypeOid = TypeOid;
        pValueInfo->Type = BASE_TYPE;

        typbyval = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typbyval, &isNull);
        if(isNull)
            ereport(ERROR, errmsg("pg_type.typbyval is NULL."));

        pValueInfo->ByVal = DatumGetBool(typbyval);

        break;
    case TYPTYPE_COMPOSITE :
        pValueInfo->Type = COMPOSITE_TYPE;
        pValueInfo->Count = 0;
        pValueInfo->attnums = palloc(0);
        pValueInfo->Fields = palloc(0);

        typrelid = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typrelid, &isNull);
        if(isNull)
            ereport(ERROR, errmsg("pg_type.typrelid is NULL."));

        classoid = ObjectIdGetDatum(typrelid);

        reltup = SearchSysCache1(RELOID, ObjectIdGetDatum(classoid));
        if(!HeapTupleIsValid(reltup))
            ereport(ERROR, errmsg("cache lookup failed for class %u.", classoid));

        relnatts = SysCacheGetAttr(RELOID, reltup, Anum_pg_class_relnatts, &isNull);
        if(isNull)
            ereport(ERROR, errmsg("pg_class.relnatts is NULL."));

        pValueInfo->natts = DatumGetInt16(relnatts);

        // Loop over all composite type attributes
        for(int16 i=0; i<pValueInfo->natts; i++)
        {
            atttup = SearchSysCache2(ATTNUM, ObjectIdGetDatum(classoid), Int16GetDatum(i+1));
            if(!HeapTupleIsValid(atttup))
                ereport(ERROR, errmsg("cache lookup failed for attribute %u attnum %d.", classoid, i+1));

            atttypid = SysCacheGetAttr(ATTNUM, atttup, Anum_pg_attribute_atttypid, &isNull);
            if(isNull)
                ereport(ERROR, errmsg("pg_attribute.atttypid is NULL."));

            // If the attribute is not dropped
            if((attroid = DatumGetObjectId(atttypid)))
            {
                pValueInfo->Count++;

                pValueInfo->attnums = repalloc(pValueInfo->attnums, pValueInfo->Count * sizeof(int16));
                pValueInfo->attnums[pValueInfo->Count-1] = i+1;

                pValueInfo->Fields = repalloc(pValueInfo->Fields, pValueInfo->Count * sizeof(struct ValueInfo*));
                pValueInfo->Fields[pValueInfo->Count-1] = palloc(sizeof(struct ValueInfo));
                BuildValueInfo(pValueInfo->Fields[pValueInfo->Count-1], attroid);
            }

            ReleaseSysCache(atttup);
        }

        ReleaseSysCache(reltup);

        pValueInfo->tupdesc = lookup_rowtype_tupdesc_copy(TypeOid, -1);
        BlessTupleDesc(pValueInfo->tupdesc);

        break;
    case TYPTYPE_DOMAIN :
        typbasetype = SysCacheGetAttr(TYPEOID, typetup, Anum_pg_type_typbasetype, &isNull);
        if(isNull)
            ereport(ERROR, errmsg("pg_type.typbasetype is NULL."));

        BuildValueInfo(pValueInfo, DatumGetObjectId(typbasetype));

        break;
    case TYPTYPE_ENUM :
        ereport(ERROR, errmsg("PL/Haskell does not support enumerated types. (%s)", type_name));
        break;
#ifdef TYPTYPE_MULTIRANGE
    case TYPTYPE_MULTIRANGE :
#endif
    case TYPTYPE_RANGE :
        ereport(ERROR, errmsg("PL/Haskell does not support range types. (%s)", type_name));
        break;
    case TYPTYPE_PSEUDO :
        ereport(ERROR, errmsg("PL/Haskell does not support type %s.", type_name));
        break;
    default :
        ereport(ERROR, errmsg("pg_type.typtype is invalid : %c", DatumGetChar(typtype)));
    }

    ReleaseSysCache(typetup);
}

// Delete temp module file and free Function or List and Iterator
void DestroyCallInfo(void* arg)
{
    struct CallInfo *pCallInfo = arg;

    if(pCallInfo->ModFileName)
        unlink(pCallInfo->ModFileName);

    if(pCallInfo->ReturnSet)
    {
        if(pCallInfo->List)
            hs_free_stable_ptr(pCallInfo->List);

        if(pCallInfo->Iterator)
            hs_free_stable_ptr(pCallInfo->Iterator);
    }
    else
        if(pCallInfo->Function)
            hs_free_stable_ptr(pCallInfo->Function);
}

// Populate the value and isNull fields of pValueInfo
void WriteValueInfo(struct ValueInfo* pValueInfo, Datum value, bool isNull)
{
    HeapTupleHeader tuple;
    Oid tupType;
    int32 tupTypmod;
    TupleDesc tupDesc;
    HeapTupleData tmptup;

    if((pValueInfo->isNull=isNull))
        return;

    switch(pValueInfo->Type)
    {
    case VOID_TYPE :
        ereport(ERROR, errmsg("Cannot write void type"));
        break;
    case BASE_TYPE :
        pValueInfo->Value = value;
        break;
    case COMPOSITE_TYPE :
        // If pValueInfo is a tuple, populate the fields recursively.
        tuple = DatumGetHeapTupleHeader(value);

        tupType = HeapTupleHeaderGetTypeId(tuple);
        tupTypmod = HeapTupleHeaderGetTypMod(tuple);
        tupDesc = lookup_rowtype_tupdesc(tupType, tupTypmod);

        tmptup.t_len = HeapTupleHeaderGetDatumLength(tuple);
        ItemPointerSetInvalid(&(tmptup.t_self));
        tmptup.t_tableOid = InvalidOid;
        tmptup.t_data = tuple;

        for(int16 i=0; i<pValueInfo->Count; i++)
        {
            Datum value = heap_getattr(&tmptup, pValueInfo->attnums[i], tupDesc, &isNull);
            WriteValueInfo(pValueInfo->Fields[i], value, isNull);
        }

        ReleaseTupleDesc(tupDesc);

        break;
    default :
        ereport(ERROR, errmsg("Bad Type : %d", pValueInfo->Type));
    }
}

// Get the value from pValueInfo
Datum ReadValueInfo(struct ValueInfo* pValueInfo, bool* isNull)
{
    Datum *values;
    bool *isNulls;

    if((*isNull=pValueInfo->isNull))
        return (Datum)0;

    switch(pValueInfo->Type)
    {
    case VOID_TYPE :
        return (Datum)0;
    case BASE_TYPE :
        return pValueInfo->Value;
    case COMPOSITE_TYPE :
        // If pValueInfo is a tuple, get the fields' values recursively.
        values  = palloc(pValueInfo->natts * sizeof(Datum));
        isNulls = palloc(pValueInfo->natts * sizeof(bool));

        for(int i = 0; i < pValueInfo->Count; i++)
        {
            int16 attnum = pValueInfo->attnums[i];
            values[attnum-1] = ReadValueInfo(pValueInfo->Fields[i], isNulls+(attnum-1));
        }

        return HeapTupleGetDatum(heap_form_tuple(pValueInfo->tupdesc, values, isNulls));
    default :
        ereport(ERROR, errmsg("Bad Type : %d", pValueInfo->Type));
    }
}

// Called when the module is loaded
static void Enter(void) __attribute__((constructor));
static void Enter(void)
{
    int new_fn;
    int stderr_pipefd[2];
    char GHC_PackagePath[MAXPGPATH+18];
    static char *argv[] = {"PLHaskell", "+RTS", "--install-signal-handlers=no", "-RTS"}; // Configuration for the RTS
    static int argc = sizeof(argv) / sizeof(char*);
    static char **argv_rts = argv;
    RtsConfig conf = defaultRtsConfig;

    // Add the directory containing PGutil to the GHC Package Path
    sprintf(GHC_PackagePath, "%s/plhaskell_pkg_db:", pkglib_path); // Note the colon
    setenv("GHC_PACKAGE_PATH", GHC_PackagePath, true);

    conf.rts_opts_enabled = RtsOptsAll;
    conf.gcDoneHook = gcDoneHook; // Called on every garbage collections to monitor memory usage
    hs_init_ghc(&argc, &argv_rts, conf);

    // Redirect stderr to pipe
    if(pipe2(stderr_pipefd, O_NONBLOCK) < 0)
        ereport(FATAL, errmsg("Unable to open pipe"));

    stderr_pipe_fn = stderr_pipefd[0];

    fflush(stderr);

    dup2(stderr_pipefd[1], STDERR_FILENO);
    close(stderr_pipefd[1]);

    atexit(exit_function); // Used to collect error information if the RTS terminates the process. 
}

// Called on every garbage collections to monitor memory usage
void gcDoneHook(const struct GCDetails_ *stats)
{
    if(stats->mem_in_use_bytes > MAX_MEMORY)
        ereport(FATAL, errmsg("Haskell RTS exceeded maximum memory. (%d bytes)", MAX_MEMORY));
}

// Called if the RTS terminates the process
void exit_function(void)
{
    char buf[4097];
    int len = read(stderr_pipe_fn, buf, 4096); // Read stderr into buf
    if(len > 0)
    {
        buf[len] = '\0';
        if(buf[len-1] == '\n')
            buf[len-1] = '\0';

        ereport(FATAL, errmsg("%s", buf)); // Use the ereport mechanism to report the terminating error. 
    }
}

// Used by Haskell
void Report(int32 elevel, char* msg)
{
    ereport(elevel, errmsg("%s", msg));
}
