#ifndef __PLHASKELL
#define __PLHASKELL

#include "postgres.h"
#include "funcapi.h"

#include "HsFFI.h"

#define VOID_TYPE      0
#define BASE_TYPE      1
#define COMPOSITE_TYPE 2

// Maximum memory the Haskell RTS is allowed to use (in bytes)
#define MAX_MEMORY 0x8000000 // 128 MiB

// Represents a value passed or returned by a function
struct ValueInfo
{
    bool isNull;
    int Type; // VOID_TYPE, BASE_TYPE, COMPOSITE_TYPE,
    union {
        struct { // BASE
            Oid TypeOid; // OID of the type
            bool ByVal; // Is the type passed by value?
            Datum Value; // value or pointer depeding on ByVal
        };

        struct { // COMPOSITE
            int16 Count; // Number of fields of the composite
            int16 natts; // Number of attributes
            int16* attnums; // Atribute numbers of the members
            TupleDesc tupdesc; // Tuple Descriptor
            struct ValueInfo** Fields; // Fields of the composite type
        };
    };
};

// Represents the information about a function call
struct CallInfo
{
    char* FuncName; // Name of function
    char* ModFileName; // Temporary file where code is stored
    short nargs; // Number of arguments
    struct ValueInfo** Args; // Arguments
    struct ValueInfo* Result; // Returned result
    bool ReturnSet; // Does the function return a set of values?

    union
    {
        // Stable pointer to the function to be called to read Args and populate Result
        // Used if ReturnSet is false
        HsStablePtr Function;

        // Used if ReturnSet if true
        struct {
            HsStablePtr List; // Stable pointer to list of reults
            HsStablePtr Iterator; // Stable pointer to function to iterator through list

        };
    };
};

// Report a message or error
void Report(int32 elevel, char* msg);

#endif
