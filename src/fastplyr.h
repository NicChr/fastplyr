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

#ifndef INTEGER64_PTR
#define INTEGER64_PTR(x) ((long long*) REAL(x))
#endif

inline cpp11::function fp_group_id = cpp11::package("fastplyr")["group_id"];

namespace rlang {
SEXP eval_tidy(SEXP expr, SEXP data, SEXP env);
SEXP as_data_pronoun(SEXP x);
SEXP new_data_mask(SEXP bottom, SEXP top);
SEXP str_as_symbol(SEXP str);
SEXP sym_as_character(SEXP sym);
SEXP quo_get_expr(SEXP quo);
SEXP quo_get_env(SEXP quo);
void env_unbind(SEXP env, SEXP sym);
}


#endif
