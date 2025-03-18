#include "fastplyr.h"
#include "cheapr_api.h"

// Basically R's get()

[[cpp11::register]]
SEXP cpp_get(SEXP sym, SEXP rho){
  Rf_protect(sym = Rf_coerceVector(sym, SYMSXP));

  if (TYPEOF(rho) != ENVSXP){
    Rf_error("second argument to '%s' must be an environment", __func__);
  }

  SEXP val = Rf_findVar(sym, rho);
  if (val == R_MissingArg){
    Rf_unprotect(1);
    Rf_error("arg `sym` cannot be missing");
  } else if (val == R_UnboundValue){
    Rf_unprotect(1);
    return R_NilValue;
  }
  else if (TYPEOF(val) == PROMSXP){
    Rf_protect(val);
    val = Rf_eval(val, rho);
    Rf_unprotect(1);
  }
  Rf_unprotect(1);
  return val;
}

// Convert call to list of symbols
SEXP as_list_call(SEXP expr) {
  if (TYPEOF(expr) != LANGSXP) {
    Rf_error("`expr` must be a language object");
  }
  int n = Rf_length(expr);
  SEXP result = Rf_protect(Rf_allocVector(VECSXP, n));
  SEXP current = expr;
  for (int i = 0; i < n; i++) {
    SET_VECTOR_ELT(result, i, CAR(current));
    current = CDR(current);
  }
  Rf_unprotect(1);
  return result;
}

// Basic version of rlang::is_call(expr, ns = ns)
bool is_call(SEXP expr, SEXP ns){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }

  SEXP expr_call = CAR(expr);
  if (TYPEOF(expr_call) != LANGSXP) return false;

  SEXP ns_str = Rf_protect(Rf_asChar(ns));
  SEXP call_list = Rf_protect(as_list_call(expr_call));

  if (Rf_length(call_list) != 3){
    Rf_unprotect(2);
    return false;
  }

  SEXP first = Rf_protect(VECTOR_ELT(call_list, 0));
  SEXP second = Rf_protect(VECTOR_ELT(call_list, 1));

  if (TYPEOF(first) != SYMSXP ||
      (STRING_ELT(rlang::sym_as_character(first), 0) != Rf_mkChar("::") &&
      STRING_ELT(rlang::sym_as_character(first), 0) != Rf_mkChar(":::"))){
    Rf_unprotect(4);
    return false;
  }
  if (TYPEOF(second) != SYMSXP){
    Rf_unprotect(4);
    return false;
  }
  bool out = STRING_ELT(rlang::sym_as_character(second), 0) == ns_str;
  Rf_unprotect(4);
  return out;
}

SEXP cpp_fun_ns(SEXP x, SEXP rho){
  int NP = 0;
  if (!Rf_isFunction(x)){
    Rf_protect(x = cpp_get(x, rho)); ++NP;
  }
  if (TYPEOF(x) != CLOSXP){
    Rf_unprotect(NP); return Rf_mkChar("");
  }
  SEXP env = Rf_protect(CLOENV(x)); ++NP;
  if (Rf_isNull(x) || Rf_isNull(env)){
    Rf_unprotect(NP); return Rf_mkChar("");
  } else if (env == R_BaseNamespace){
    Rf_unprotect(NP); return Rf_mkChar("base");
  } else if (R_IsNamespaceEnv(env)) {
    SEXP ns_name = Rf_protect(R_NamespaceEnvSpec(env)); ++NP;
    SEXP names = Rf_protect(Rf_getAttrib(ns_name, R_NamesSymbol)); ++NP;
    SEXP name = Rf_protect(Rf_mkString("name")); ++NP;
    SEXP name_loc = Rf_protect(Rf_match(names, name, NA_INTEGER)); ++NP;

    if (TYPEOF(ns_name) == STRSXP &&
        Rf_length(name) != 0 &&
        INTEGER(name_loc)[0] != NA_INTEGER){
      SEXP result = Rf_protect(STRING_ELT(ns_name, INTEGER(name_loc)[0] - 1)); ++NP;
      Rf_unprotect(NP);
      return result;
    }
    Rf_unprotect(NP); return Rf_mkChar("");
  } else {
    Rf_unprotect(NP); return Rf_mkChar("");
  }
}
[[cpp11::register]]
bool cpp_call_contains_ns(SEXP expr, SEXP ns, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int NP = 0;
  if (is_call(expr, ns)){
    return true;
  }
  bool out = false;

  SEXP ns_str = Rf_protect(Rf_asChar(ns)); ++NP;
  SEXP tree = Rf_protect(as_list_call(expr)); ++NP;
  SEXP branch;
  for (int i = 0; i < Rf_length(tree); ++i){
    branch = Rf_protect(VECTOR_ELT(tree, i)); ++NP;

    // If branch is a call
    if (TYPEOF(branch) == LANGSXP){
      if (cpp_call_contains_ns(branch, ns, rho)){
        out = true;
        break;
      }
    }
    if (TYPEOF(branch) == SYMSXP &&
        cpp_fun_ns(rlang::sym_as_character(branch), rho) == ns_str){
      out = true;
      break;
    }
  }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
SEXP get_mask_top_env(SEXP mask){

  if (TYPEOF(mask) != ENVSXP){
    Rf_error("Object must be a data mask `environment` in %s", __func__);
  }

  SEXP top_env_sym = Rf_protect(Rf_install(".top_env"));
  SEXP top_env = Rf_protect(cpp_get(top_env_sym, mask));
  if (Rf_isNull(top_env)){
    Rf_unprotect(2);
    Rf_error("`.top_env` cannot be found in data mask");
  }
  Rf_unprotect(2);
  return top_env;
}

// Just a wrapper around rlang::eval_tidy
// but only supplying a quosure and data mask

[[cpp11::register]]
SEXP cpp_eval_tidy(SEXP quo, SEXP mask){
  SEXP expr = Rf_protect(rlang::quo_get_expr(quo));
  SEXP rho = Rf_protect(rlang::quo_get_env(quo));
  SEXP result = Rf_protect(rlang::eval_tidy(expr, mask, rho));
  Rf_unprotect(3);
  return result;
}

// Eval a list of quos

[[cpp11::register]]
SEXP cpp_eval_all_tidy(SEXP quos, SEXP mask){
  int NP = 0;
  int n_exprs = Rf_length(quos);
  SEXP expr_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
  if (Rf_isNull(expr_names)){
    Rf_protect(expr_names = Rf_allocVector(STRSXP, n_exprs)); ++NP;
  }
  SEXP top_env = Rf_protect(get_mask_top_env(mask));++NP;

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_exprs)); ++NP;
  SEXP out_names = Rf_protect(Rf_allocVector(STRSXP, n_exprs)); ++NP;

  for (int i = 0; i < n_exprs; ++i){
    SEXP quo = Rf_protect(VECTOR_ELT(quos, i)); ++NP;
    SEXP result = Rf_protect(cpp_eval_tidy(quo, mask)); ++NP;
    SEXP expr_name = Rf_protect(STRING_ELT(expr_names, i)); ++NP;

    if (expr_name != R_BlankString){
      SEXP sym = Rf_protect(Rf_installChar(expr_name)); ++NP;
      Rf_defineVar(sym, result, top_env);
      SET_STRING_ELT(out_names, i, expr_name);
    }
    if (!Rf_isNull(result)){
      SET_VECTOR_ELT(out, i, result);
    }
  }
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_list_tidy(SEXP quos, bool keep_null){
  SEXP env = Rf_protect(R_NewEnv(R_EmptyEnv, false, 0));
  SEXP mask = Rf_protect(rlang::new_data_mask(env, env));
  SEXP top_env = Rf_protect(get_mask_top_env(mask));

  // Add .data pronoun
  SEXP data_pronoun_sym = Rf_protect(Rf_install(".data"));
  SEXP data_pronoun = Rf_protect(rlang::as_data_pronoun(env));
  Rf_defineVar(data_pronoun_sym, data_pronoun, top_env);

  SEXP out = Rf_protect(cpp_eval_all_tidy(quos, mask));

  if (!keep_null){
    Rf_protect(out = cheapr::drop_null(out, false));
    Rf_unprotect(7); return out;
  } else {
    Rf_unprotect(6); return out;
  }
}

[[cpp11::register]]
SEXP cpp_grouped_eval_tidy(SEXP group_data, SEXP data, SEXP quos, bool as_df){
  SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));

  int n_groups = Rf_length(Rf_GetRowNames(group_data));
  int n_quos = Rf_length(quos);

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));

  SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
  const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);

  // Initialise components

  SEXP chunk_locs, chunk, mask, result;

  PROTECT_INDEX index1, index2, index3, index4;
  R_ProtectWithIndex(chunk_locs = R_NilValue, &index1);
  R_ProtectWithIndex(chunk = R_NilValue, &index2);
  R_ProtectWithIndex(mask = R_NilValue, &index3);
  R_ProtectWithIndex(result = R_NilValue, &index4);

  int k = 0; // Keep track of how many quosures we're looping through

  for (int i = 0; i < n_groups; ++i, ++k){
    if (k == n_quos) k = 0;
    R_Reprotect(chunk_locs = VECTOR_ELT(rows, i), index1);
    R_Reprotect(chunk = cheapr::df_select(data, p_quov[k]), index2);
    R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
    R_Reprotect(mask = rlang::as_data_mask(chunk), index3);
    R_Reprotect(result = cpp_eval_all_tidy(quos, mask), index4);
    if (as_df){
      R_Reprotect(result = cheapr::list_as_df(result), index4);
    }
    SET_VECTOR_ELT(out, i, result);
  }

  Rf_unprotect(7);
  return out;
}
// SEXP cpp_grouped_eval_tidy(SEXP group_data, SEXP data, SEXP quos){
//   SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
//
//   int n_groups = Rf_length(Rf_getAttrib(group_data, R_RowNamesSymbol));
//   int n_quos = Rf_length(quos);
//
//   SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));
//
//   SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
//   const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);
//
//   // Initialise components
//
//   SEXP chunk_locs, chunk, mask, result;
//
//   PROTECT_INDEX index1, index2, index3, index4;
//   R_ProtectWithIndex(chunk_locs = R_NilValue, &index1);
//   R_ProtectWithIndex(chunk = R_NilValue, &index2);
//   R_ProtectWithIndex(mask = R_NilValue, &index3);
//   R_ProtectWithIndex(result = R_NilValue, &index4);
//
//   int k = 0; // Keep track of how many quosures we're looping through
//
//   for (int i = 0; i < n_groups; ++i, ++k){
//     if (k == n_quos) k = 0;
//     R_Reprotect(chunk_locs = VECTOR_ELT(rows, i), index1);
//     R_Reprotect(chunk = cheapr::df_select(data, p_quov[k]), index2);
//     R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
//     R_Reprotect(mask = rlang::as_data_mask(chunk), index3);
//     R_Reprotect(result = cpp_eval_all_tidy(quos, mask), index4);
//     SET_VECTOR_ELT(out, i, result);
//   }
//
//   Rf_unprotect(7);
//   return out;
// }

