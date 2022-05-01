#include <template-hsc.h>

#define hsc_transfer(expr, name, sig) { \
    hsc_printf("do {"); \
    hsc_printf("pFunc <- (liftIO . newStablePtr) (%s); ", #expr); \
    hsc_printf("runStmt (\"%s <- Foreign.StablePtr.deRefStablePtr ( Foreign.StablePtr.castPtrToStablePtr ( Foreign.Ptr.wordPtrToPtr \" ++ (show . castStablePtrToPtr) pFunc ++ \")) :: Prelude.IO (%s)\"); ", #name, #sig); \
    hsc_printf("(liftIO . freeStablePtr) pFunc;"); \
    hsc_printf("}"); \
}

