#ifndef fastplyr_cpp_
#define fastplyr_cpp_

#include <cpp11.hpp>
#include <Rinternals.h>

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#ifndef VECTOR_PTR_RO
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))
#endif

#endif
