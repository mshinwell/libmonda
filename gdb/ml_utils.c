#ifndef ML_UTILS_H
#define ML_UTILS_H

#include "gdb_ocaml_support.h"

#define Val_ptr(x) caml_copy_nativeint((intptr_t)x)
#define Ptr_val(x) ((void*)(intptr_t)Nativeint_val(x))

#define Val_target(x) caml_copy_nativeint((intptr_t)x)
#define Target_val(x) ((CORE_ADDR)(intptr_t)Nativeint_val(x))

#endif /*!ML_UTILS_H*/
