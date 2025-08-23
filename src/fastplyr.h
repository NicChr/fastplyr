#ifndef fastplyr_cpp_
#define fastplyr_cpp_

#include <cpp11.hpp>
#include <Rinternals.h>
#include <vector>

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

inline void *safe_memmove(void *dst, const void *src, size_t n){
  return n ? memmove(dst, src, n) : dst;
}

inline void *safe_memcpy(void *dst, const void *src, size_t n){
  return n ? memcpy(dst, src, n) : dst;
}

inline void *safe_memset(void *dst, int val, size_t n){
  return n ? memset(dst, val, n) : dst;
}

SEXP get_list_element(SEXP list, const char *str);
int cpp_n_group_vars(SEXP x);
SEXP cpp_orig_order(SEXP group_id, SEXP group_sizes);
SEXP compact_int_seq_len(int n);
SEXP as_list_call(SEXP expr);
SEXP call_args(SEXP expr);
bool is_fn_call(SEXP expr, SEXP fn, SEXP ns, SEXP rho);
bool call_is_namespaced(SEXP expr);
SEXP get_fun_ns(SEXP x, SEXP rho);
void set_as_vctrs_new_list_of_int(SEXP x);
void set_as_tbl(SEXP x);
SEXP binary_combine(SEXP x, SEXP y);
SEXP get_mask_data_vars(SEXP mask);
SEXP quo_vars(SEXP quos, SEXP mask, bool combine);
bool exists(SEXP sym, SEXP rho);
bool is_data_pronoun_call(SEXP expr, SEXP env);
SEXP data_pronoun_var(SEXP expr, SEXP env);
SEXP get_mask_top_env(SEXP mask);

// Group metadata

SEXP cpp_group_data(SEXP x);
SEXP cpp_group_keys(SEXP x);
SEXP cpp_group_vars(SEXP x);
SEXP cpp_group_rows(SEXP x);
SEXP cpp_group_size(SEXP x);
SEXP cpp_ungroup(SEXP data);
bool cpp_group_by_order_default(SEXP x);
SEXP cpp_group_id(SEXP x);


namespace rlang {
SEXP eval_tidy(SEXP expr, SEXP data, SEXP env);
SEXP as_data_pronoun(SEXP x);
SEXP new_data_mask(SEXP bottom, SEXP top);
SEXP new_quosure(SEXP expr, SEXP env);
SEXP as_data_mask(SEXP data);
SEXP str_as_symbol(SEXP str);
SEXP sym_as_character(SEXP sym);
SEXP sym_as_string(SEXP sym);
SEXP quo_get_expr(SEXP quo);
SEXP quo_get_env(SEXP quo);
void env_unbind(SEXP env, SEXP sym);
}


#endif
