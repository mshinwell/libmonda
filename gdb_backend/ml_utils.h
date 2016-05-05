#ifndef ML_UTILS_H
#define ML_UTILS_H

#include <caml/alloc.h>
#include <caml/mlvalues.h>

#include "gdb_ocaml_support.h"

/* Host process pointers */
#define Val_ptr(x) caml_copy_int64((intptr_t)x)
#define Ptr_val(x) ((void*)(intptr_t)Int64_val(x))

/* Inferior process pointers */
#define TARGET_SIZE (sizeof(CORE_ADDR))
#define Val_target(x) caml_copy_int64((intptr_t)x)
#define Target_val(x) ((CORE_ADDR)(intptr_t)Int64_val(x))

#endif /*!ML_UTILS_H*/
