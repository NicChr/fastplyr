#include "fastplyr.h"

namespace rlang {

// *INDENT-OFF*
struct rlang_api_ptrs_t {
  SEXP (*eval_tidy)(SEXP expr, SEXP data, SEXP env);
  SEXP (*as_data_pronoun)(SEXP x);
  SEXP (*new_data_mask)(SEXP bottom, SEXP top);
  SEXP (*as_data_mask)(SEXP data);
  SEXP (*str_as_symbol)(SEXP str);
  SEXP (*sym_as_character)(SEXP sym);
  SEXP (*sym_as_string)(SEXP sym);
  SEXP (*quo_get_expr)(SEXP quo);
  SEXP (*quo_get_env)(SEXP quo);
  void (*env_unbind)(SEXP, SEXP);

  rlang_api_ptrs_t() {
    eval_tidy       = (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rlang", "rlang_eval_tidy");
    as_data_pronoun = (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_as_data_pronoun");
    new_data_mask   = (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_new_data_mask_3.0.0");
    as_data_mask   = (SEXP (*)(SEXP))              R_GetCCallable("rlang", "rlang_as_data_mask_3.0.0");
    str_as_symbol   = (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_str_as_symbol");
    sym_as_character = (SEXP (*)(SEXP))            R_GetCCallable("rlang", "rlang_sym_as_character");
    sym_as_string = (SEXP (*)(SEXP))            R_GetCCallable("rlang", "rlang_sym_as_string");
    quo_get_expr    = (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_expr");
    quo_get_env     = (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_env");
    env_unbind      = (void (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_env_unbind");
  }
};
// *INDENT-ON*

const rlang_api_ptrs_t& rlang_api() {
  static rlang_api_ptrs_t ptrs;
  return ptrs;
}

SEXP eval_tidy(SEXP expr, SEXP data, SEXP env) {
  return rlang_api().eval_tidy(expr, data, env);
}

SEXP as_data_pronoun(SEXP x) {
  return rlang_api().as_data_pronoun(x);
}

SEXP new_data_mask(SEXP bottom, SEXP top) {
  return rlang_api().new_data_mask(bottom, top);
}

SEXP as_data_mask(SEXP data) {
  return rlang_api().as_data_mask(data);
}

SEXP str_as_symbol(SEXP str) {
  return rlang_api().str_as_symbol(str);
}

SEXP sym_as_character(SEXP sym) {
  return rlang_api().sym_as_character(sym);
}
SEXP sym_as_string(SEXP sym) {
  return rlang_api().sym_as_string(sym);
}

SEXP quo_get_expr(SEXP quo) {
  return rlang_api().quo_get_expr(quo);
}

SEXP quo_get_env(SEXP quo) {
  return rlang_api().quo_get_env(quo);
}

void env_unbind(SEXP env, SEXP sym) {
  rlang_api().env_unbind(env, sym);
}

}
