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
#define INTEGER64_PTR(x) ((int64_t*) REAL(x))
#endif

#ifndef SHIELD
#define SHIELD Rf_protect
#endif

#ifndef YIELD
#define YIELD Rf_unprotect
#endif

inline cpp11::function fp_group_id = cpp11::package("fastplyr")["group_id"];

inline int df_nrow(SEXP x){
  return Rf_length(Rf_getAttrib(x, R_RowNamesSymbol));
}

inline void set_names(SEXP x, SEXP names){
  names == R_NilValue ? Rf_setAttrib(x, R_NamesSymbol, R_NilValue) : Rf_namesgets(x, names);
}

inline SEXP get_names(SEXP x){
  return Rf_getAttrib(x, R_NamesSymbol);
}

inline SEXP new_vec(SEXPTYPE type, R_xlen_t n){
  return Rf_allocVector(type, n);
}

inline SEXP coerce_vec(SEXP x, SEXPTYPE type){
  return Rf_coerceVector(x, type);
}

SEXP get_list_element(SEXP list, const char *str);
int cpp_n_group_vars(SEXP x);
SEXP cpp_orig_order(SEXP group_id, SEXP group_sizes);

namespace rlang {
SEXP eval_tidy(SEXP expr, SEXP data, SEXP env);
SEXP as_data_pronoun(SEXP x);
SEXP new_data_mask(SEXP bottom, SEXP top);
SEXP as_data_mask(SEXP data);
SEXP str_as_symbol(SEXP str);
SEXP sym_as_character(SEXP sym);
SEXP sym_as_string(SEXP sym);
SEXP quo_get_expr(SEXP quo);
SEXP quo_get_env(SEXP quo);
void env_unbind(SEXP env, SEXP sym);
}


#endif
