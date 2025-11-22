#ifndef FASTPLYR_H
#define FASTPLYR_H

#include <cheapr_api.h>
#include <vector>

using namespace cheapr;

inline cpp11::function fp_group_id = cpp11::package("fastplyr")["group_id"];

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
SEXP get_mask_data_vars(SEXP mask);
SEXP quo_vars(SEXP quos, SEXP mask, bool combine);
bool exists(SEXP sym, SEXP rho);
bool is_data_pronoun_call(SEXP expr, SEXP env);
SEXP data_pronoun_var(SEXP expr, SEXP env);
SEXP get_mask_top_env(SEXP mask);
SEXP new_bare_data_mask();
SEXP cpp_quos_drop_null(SEXP quos);
SEXP r_deparse(SEXP quo);
SEXP make_named_quos(SEXP quos);

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
