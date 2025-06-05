#include "fastplyr.h"
#include "cheapr_api.h"
#include <vector>

// Basically R's get()

SEXP cpp_get(SEXP sym, SEXP rho){

  int32_t NP = 0;
  if (TYPEOF(sym) != SYMSXP){
    Rf_protect(sym = Rf_coerceVector(sym, SYMSXP)); ++NP;
  }

  if (TYPEOF(rho) != ENVSXP){
    Rf_error("second argument to '%s' must be an environment", __func__);
  }

  SEXP val = Rf_findVar(sym, rho);
  if (val == R_MissingArg){
    Rf_unprotect(NP);
    Rf_error("arg `sym` cannot be missing");
  } else if (val == R_UnboundValue){
    Rf_unprotect(NP);
    return R_NilValue;
  } else if (TYPEOF(val) == PROMSXP){
    Rf_protect(val);
    val = Rf_eval(val, rho);
    Rf_unprotect(1);
  }
  Rf_unprotect(NP);
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

[[cpp11::register]]
bool is_nested_call(SEXP expr) {
  if (TYPEOF(expr) != LANGSXP) {
    return false;
  }
  SEXP current = CDR(expr);
  while (TYPEOF(current) != NILSXP) {
    SEXP arg = CAR(current);
    if (TYPEOF(arg) == LANGSXP) {
      return true;
    }
    current = CDR(current);
  }
  return false;
}

[[cpp11::register]]
bool call_is_namespaced(SEXP expr){

  int32_t NP = 0;

  if (TYPEOF(expr) != LANGSXP){
    Rf_error("`expr` must be a `call` in %s", __func__);
  }

  SEXP expr_call = CAR(expr);
  if (TYPEOF(expr_call) != LANGSXP) return false;

  SEXP call_list = Rf_protect(as_list_call(expr_call)); ++NP;

  if (Rf_length(call_list) != 3){
    Rf_unprotect(NP);
    return false;
  }

  SEXP first = Rf_protect(VECTOR_ELT(call_list, 0)); ++NP;
  SEXP second = Rf_protect(VECTOR_ELT(call_list, 1)); ++NP;

  if (TYPEOF(first) != SYMSXP ||
      (first != R_DoubleColonSymbol &&
      first != R_TripleColonSymbol)){
    Rf_unprotect(NP);
    return false;
  }
  if (TYPEOF(second) != SYMSXP){
    Rf_unprotect(NP);
    return false;
  }
  Rf_unprotect(NP);
  return true;
}

// Basic version of rlang::is_call(expr, ns = ns)
bool is_ns_call(SEXP expr, SEXP ns){

  int32_t NP = 0;

  if (TYPEOF(ns) != STRSXP){
    Rf_error("`ns` must be a character vector in %s", __func__);
  }

  if (TYPEOF(expr) != LANGSXP){
    return false;
  }

  SEXP expr_call = CAR(expr);
  if (TYPEOF(expr_call) != LANGSXP) return false;

  SEXP ns_str = Rf_protect(Rf_asChar(ns)); ++NP;
  SEXP call_list = Rf_protect(as_list_call(expr_call)); ++NP;

  if (Rf_length(call_list) != 3){
    Rf_unprotect(NP);
    return false;
  }

  SEXP first = Rf_protect(VECTOR_ELT(call_list, 0)); ++NP;
  SEXP second = Rf_protect(VECTOR_ELT(call_list, 1)); ++NP;

  if (TYPEOF(first) != SYMSXP ||
      (first != R_DoubleColonSymbol &&
      first != R_TripleColonSymbol)){
    Rf_unprotect(NP);
    return false;
  }
  if (TYPEOF(second) != SYMSXP){
    Rf_unprotect(NP);
    return false;
  }
  bool out = rlang::sym_as_string(second) == ns_str;
  Rf_unprotect(NP);
  return out;
}

// get the namespace of a function

SEXP get_fun_ns(SEXP x, SEXP rho){
  int32_t NP = 0;
  if (!Rf_isFunction(x)){
    Rf_protect(x = cpp_get(x, rho)); ++NP;
  }
  if (TYPEOF(x) != CLOSXP){
    Rf_unprotect(NP); return R_BlankString;
  }
  SEXP env_call = Rf_protect(Rf_lang2(Rf_install("environment"), x)); ++NP;
  SEXP env = Rf_protect(Rf_eval(env_call, rho)); ++NP;
  // SEXP env = Rf_protect(CLOENV(x)); ++NP;
  if (Rf_isNull(x) || Rf_isNull(env)){
    Rf_unprotect(NP); return R_BlankString;
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
    Rf_unprotect(NP); return R_BlankString;
  } else {
    Rf_unprotect(NP); return R_BlankString;
  }
}

[[cpp11::register]]
SEXP fun_ns(SEXP x, SEXP rho){
  return Rf_ScalarString(get_fun_ns(x, rho));
}

// is this call a call to any function supplied to `fn`?

bool is_call2(SEXP expr, SEXP fn){

  if (TYPEOF(fn) != STRSXP){
    Rf_error("`fn` must be a character vector %s", __func__);
  }

  int32_t NP = 0;

  SEXP fn_sym;
  PROTECT_INDEX fn_sym_idx;
  R_ProtectWithIndex(fn_sym = R_NilValue, &fn_sym_idx); ++NP;

  for (int i = 0; i < Rf_length(fn); ++i){
    R_Reprotect(fn_sym = Rf_installChar(STRING_ELT(fn, i)), fn_sym_idx);

    if (TYPEOF(expr) == LANGSXP && call_is_namespaced(expr)){
      SEXP call_tree = Rf_protect(as_list_call(expr)); ++NP;
      SEXP fn_expr_tree = Rf_protect(as_list_call(VECTOR_ELT(call_tree, 0))); ++NP;
    if (TYPEOF(VECTOR_ELT(fn_expr_tree, 2)) == SYMSXP &&
        VECTOR_ELT(fn_expr_tree, 2) == fn_sym){
      Rf_unprotect(NP);
      return true;
    }
    } else if (TYPEOF(expr) == LANGSXP && TYPEOF(CAR(expr)) == SYMSXP){
      if (CAR(expr) == fn_sym){
        Rf_unprotect(NP);
        return true;
      }
    }
  }
  Rf_unprotect(NP);
  return false;
}

// A function similar to rlang::is_call(expr, fn)
// but it is more aggressive in finding the specified function
// The call need not be namespaced to check that the call contains a function
// in a specified namespace
// for example, is_fn_call(quote(mean()), "mean", "base", globalenv())
// returns true whereas
// rlang::is_call(quote(mean()), "mean", ns = "base") returns false

[[cpp11::register]]
bool cpp_is_fn_call(SEXP expr, SEXP fn, SEXP ns, SEXP rho){
    if (TYPEOF(fn) != STRSXP){
      Rf_error("`fn` must be a character vector in %s", __func__);
    }

    if (!Rf_isNull(ns) && (TYPEOF(ns) != STRSXP || Rf_length(ns) != 1)){
      Rf_error("`ns` must be `NULL` or a character vector of length one in %s", __func__);
    }

    if (TYPEOF(expr) != LANGSXP){
      return false;
    }

    int32_t NP = 0;
    int n_fns = Rf_length(fn);

    if (Rf_isNull(ns)){
      return is_call2(expr, fn);
    } else {
      SEXP ns_char = STRING_ELT(ns, 0);
      SEXP fn_ns, fn_sym;
      PROTECT_INDEX fn_ns_idx, fn_sym_idx;
      R_ProtectWithIndex(fn_ns = R_NilValue, &fn_ns_idx); ++NP;
      R_ProtectWithIndex(fn_sym = R_NilValue, &fn_sym_idx); ++NP;
      bool out = is_call2(expr, fn);
      if (!out){
        Rf_unprotect(NP);
        return out;
      }
      out = false; // Reset
      if (call_is_namespaced(expr)){
        SEXP call_tree = Rf_protect(as_list_call(expr)); ++NP;
        SEXP fn_expr_tree = Rf_protect(as_list_call(VECTOR_ELT(call_tree, 0))); ++NP;
        R_Reprotect(fn_ns = rlang::sym_as_string(VECTOR_ELT(fn_expr_tree, 1)), fn_ns_idx);
        out = fn_ns == ns_char;
      } else {
        for (int i = 0; i < n_fns; ++i){
          R_Reprotect(fn_sym = Rf_installChar(STRING_ELT(fn, i)), fn_sym_idx);
          R_Reprotect(fn_ns = get_fun_ns(fn_sym, rho), fn_ns_idx);
          out = out || (fn_ns == ns_char);
        }
      }
      Rf_unprotect(NP);
      return out;
    }
}

// checks if call is or contains any calls to a namespace
// it doesn't require the function to actually be called via `::`
bool cpp_call_contains_ns(SEXP expr, SEXP ns, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int32_t NP = 0;
  if (is_ns_call(expr, ns)){
    return true;
  }
  bool out = false;

  if (TYPEOF(ns) != STRSXP || Rf_length(ns) != 1){
    Rf_error("`ns` must be a length 1 character vector in %s", __func__);
  }

  SEXP ns_str = Rf_protect(STRING_ELT(ns, 0)); ++NP;
  SEXP tree = Rf_protect(as_list_call(expr)); ++NP;
  SEXP branch;
  for (int i = 0; i < Rf_length(tree); ++i){
    branch = VECTOR_ELT(tree, i);

    // If branch is a call
    if (TYPEOF(branch) == LANGSXP){
      if (cpp_call_contains_ns(branch, ns, rho)){
        out = true;
        break;
      }
    }
    if (TYPEOF(branch) == SYMSXP){
     SEXP branch_name = Rf_protect(rlang::sym_as_character(branch)); ++NP;
     SEXP fun_ns = Rf_protect(get_fun_ns(branch_name, rho)); ++NP;
     if (fun_ns == ns_str){
       out = true;
       break;
     }
    }
  }
  Rf_unprotect(NP);
  return out;
}

bool cpp_call_contains_fn(SEXP expr, SEXP fn, SEXP ns, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int32_t NP = 0;
  if (cpp_is_fn_call(expr, fn, ns, rho)){
    return true;
  }
  bool out = false;

  if (TYPEOF(fn) != STRSXP || Rf_length(fn) != 1){
    Rf_error("`fn` must be a length 1 character vector in %s", __func__);
  }
  if (TYPEOF(ns) != NILSXP && (TYPEOF(ns) != STRSXP || Rf_length(ns) != 1)){
    Rf_error("`ns` must be `NULL` or a length 1 character vector in %s", __func__);
  }
  // SEXP fn_str = Rf_protect(STRING_ELT(fn, 0)); ++NP;
  // SEXP fn_sym = Rf_protect(Rf_installChar(fn_str)); ++NP;
  // SEXP ns_str;
  // if (TYPEOF(ns) == NILSXP){
  //   ns_str = Rf_protect(Rf_allocVector(STRSXP, 1)); ++NP;
  // } else {
  //   ns_str = Rf_protect(STRING_ELT(ns, 0)); ++NP;
  // }
  SEXP tree = Rf_protect(as_list_call(expr)); ++NP;
  SEXP branch;
  for (int i = 0; i < Rf_length(tree); ++i){
    branch = VECTOR_ELT(tree, i);

    // If branch is a call
    if (TYPEOF(branch) == LANGSXP){
      if (cpp_call_contains_fn(branch, fn, ns, rho)){
        out = true;
        break;
      }
    }
    if (cpp_is_fn_call(branch, fn, ns, rho)){
      out = true;
      break;
    }
  }
  // for (int i = 0; i < Rf_length(tree); ++i){
  //   branch = VECTOR_ELT(tree, i);
  //
  //   // If branch is a call
  //   if (TYPEOF(branch) == LANGSXP){
  //     if (cpp_call_contains_fn(branch, fn, ns, rho)){
  //       out = true;
  //       break;
  //     }
  //   }
  //   if (TYPEOF(branch) == SYMSXP){
  //     if (TYPEOF(ns) == NILSXP){
  //       if (branch == fn_sym){
  //         out = true; break;
  //       }
  //     } else {
  //       SEXP branch_name = Rf_protect(rlang::sym_as_character(branch)); ++NP;
  //       SEXP fn_ns = Rf_protect(get_fun_ns(branch_name, rho)); ++NP;
  //       if (branch == fn_sym && std::strcmp(CHAR(fn_ns), CHAR(ns_str)) == 0){
  //         out = true; break;
  //       }
  //     }
  //   }
  // }
  Rf_unprotect(NP);
  return out;
}

bool cpp_any_quo_contains_ns(SEXP quos, SEXP ns){

  if (TYPEOF(quos) != VECSXP){
    Rf_error("`quos` must be a list of quosures in %s", __func__);
  }

  bool out = false;

  SEXP expr, quo_env;
  PROTECT_INDEX expr_idx, quo_env_idx;
  R_ProtectWithIndex(expr = R_NilValue, &expr_idx);
  R_ProtectWithIndex(quo_env = R_NilValue, &quo_env_idx);

  for (int i = 0; i < Rf_length(quos); ++i){
    if (!Rf_inherits(VECTOR_ELT(quos, i), "quosure")){
      Rf_unprotect(2);
      Rf_error("`quos` must be a list of quosures in %s", __func__);
    }
    R_Reprotect(expr = rlang::quo_get_expr(VECTOR_ELT(quos, i)), expr_idx);
    R_Reprotect(quo_env = rlang::quo_get_env(VECTOR_ELT(quos, i)), quo_env_idx);
    if (cpp_call_contains_ns(expr, ns, quo_env)){
      out = true; break;
    }
  }
  Rf_unprotect(2);
  return out;
}

SEXP cpp_unnest_expr(SEXP expr){
  int32_t NP = 0;
  if (Rf_inherits(expr, "quosure")){
    Rf_protect(expr = rlang::quo_get_expr(expr)); ++NP;
  }

  if (TYPEOF(expr) != LANGSXP){
    Rf_unprotect(NP);
    return expr;
  }

  SEXP out = Rf_protect(as_list_call(expr)); ++NP;

  for (int i = 0; i < Rf_length(out); ++i){
    if (TYPEOF(VECTOR_ELT(out, i)) == LANGSXP){
      SET_VECTOR_ELT(out, i, cpp_unnest_expr(VECTOR_ELT(out, i)));
    }
  }
  Rf_unprotect(NP);
  return out;
}

cpp11::writable::strings all_call_names(SEXP expr){

  cpp11::list expr_tree = as_list_call(expr);

  cpp11::writable::strings out;

  cpp11::strings temp;

  for (int i = 1; i < expr_tree.size(); ++i){
    if (TYPEOF(expr_tree[i]) == SYMSXP){
      out.push_back(rlang::sym_as_string(expr_tree[i]));
    } else if (TYPEOF(expr_tree[i]) == LANGSXP){
      temp = all_call_names(expr_tree[i]);
      for (int j = 0; j < temp.size(); ++j){
        out.push_back(temp[j]);
      }
    }
  }
  return out;
}

// Which variables are quosures pointing to?

[[cpp11::register]]
SEXP cpp_quo_data_vars(SEXP quos, SEXP data){

  SEXP list_container = Rf_protect(Rf_allocVector(VECSXP, 2));

  SEXP quo_vars, out;
  PROTECT_INDEX quo_vars_idx, out_idx;
  R_ProtectWithIndex(quo_vars = R_NilValue, &quo_vars_idx);
  R_ProtectWithIndex(out = Rf_allocVector(STRSXP, 0), &out_idx);

  for (int i = 0; i < Rf_length(quos); ++i){
    R_Reprotect(quo_vars = all_call_names(VECTOR_ELT(quos, i)), quo_vars_idx);
    SET_VECTOR_ELT(list_container, 0, out);
    SET_VECTOR_ELT(list_container, 1, quo_vars);
    R_Reprotect(out = cheapr::c(list_container), out_idx);
  }
  SEXP names = Rf_protect(Rf_getAttrib(data, R_NamesSymbol));
  Rf_protect(out = cheapr::intersect(names, out, false));
  Rf_unprotect(5);
  return out;
}


[[cpp11::register]]
SEXP cpp_quos_drop_null(SEXP quos){

  int n = Rf_length(quos);

  SEXP not_null = Rf_protect(Rf_allocVector(LGLSXP, n));
  int *p_not_null = INTEGER(not_null);
  const SEXP *p_quos = VECTOR_PTR_RO(quos);
  int n_null = 0;

  for (int i = 0; i < n; ++i){
    p_not_null[i] = TYPEOF(rlang::quo_get_expr(p_quos[i])) != NILSXP;
    n_null += !p_not_null[i];
  }
  if (n_null == 0){
    Rf_unprotect(1);
    return quos;
  }
  SEXP r_true = Rf_protect(Rf_allocVector(LGLSXP, 1));
  LOGICAL(r_true)[0] = TRUE;
  SEXP not_null_locs = Rf_protect(cheapr::val_find(not_null, r_true, false));
  SEXP out = Rf_protect(cheapr::sset_vec(quos, not_null_locs, false));
  Rf_copyMostAttrib(quos, out);
  SEXP names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol));
  Rf_setAttrib(out, R_NamesSymbol, cheapr::sset_vec(names, not_null_locs, false));
  SEXP cls = Rf_protect(Rf_getAttrib(quos, R_ClassSymbol));
  Rf_classgets(out, cls);
  Rf_unprotect(6);
  return out;
}

// bool quo_is_dplyr_mask_call(SEXP quo){
//   SEXP expr = Rf_protect(rlang::quo_get_expr(quo));
//   SEXP env =  Rf_protect(rlang::quo_get_env(quo));
//   SEXP dplyr_mask_fns = Rf_protect(Rf_allocVector(STRSXP, 7));
//   SET_STRING_ELT(dplyr_mask_fns, 0, Rf_mkChar("n"));
//   SET_STRING_ELT(dplyr_mask_fns, 1, Rf_mkChar("pick"));
//   SET_STRING_ELT(dplyr_mask_fns, 2, Rf_mkChar("cur_group_id"));
//   SET_STRING_ELT(dplyr_mask_fns, 3, Rf_mkChar("cur_group_rows"));
//   SET_STRING_ELT(dplyr_mask_fns, 4, Rf_mkChar("cur_column"));
//   SET_STRING_ELT(dplyr_mask_fns, 5, Rf_mkChar("cur_data"));
//   SET_STRING_ELT(dplyr_mask_fns, 6, Rf_mkChar("cur_data_all"));
//   SEXP dplyr_str = Rf_protect(Rf_mkString("dplyr"));
//   bool out = cpp_is_fn_call(expr, dplyr_mask_fns, dplyr_str, env);
//   Rf_unprotect(4);
//   return out;
// }

bool call_contains_dplyr_mask(SEXP expr, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }

  int32_t NP = 0;

  SEXP dplyr_mask_fns = Rf_protect(Rf_allocVector(STRSXP, 11)); ++NP;
  SET_STRING_ELT(dplyr_mask_fns, 0, Rf_mkChar("n"));
  SET_STRING_ELT(dplyr_mask_fns, 1, Rf_mkChar("pick"));
  SET_STRING_ELT(dplyr_mask_fns, 2, Rf_mkChar("row_number"));
  SET_STRING_ELT(dplyr_mask_fns, 3, Rf_mkChar("cur_group_id"));
  SET_STRING_ELT(dplyr_mask_fns, 4, Rf_mkChar("cur_group_rows"));
  SET_STRING_ELT(dplyr_mask_fns, 5, Rf_mkChar("cur_column"));
  SET_STRING_ELT(dplyr_mask_fns, 6, Rf_mkChar("cur_data"));
  SET_STRING_ELT(dplyr_mask_fns, 7, Rf_mkChar("cur_data_all"));
  SET_STRING_ELT(dplyr_mask_fns, 8, Rf_mkChar("if_any"));
  SET_STRING_ELT(dplyr_mask_fns, 9, Rf_mkChar("if_all"));
  SET_STRING_ELT(dplyr_mask_fns, 10, Rf_mkChar("c_across"));
  SEXP dplyr_str = Rf_protect(Rf_mkString("dplyr")); ++NP;

  if (cpp_is_fn_call(expr, dplyr_mask_fns, dplyr_str, rho)){
    Rf_unprotect(NP);
    return true;
  }

  bool out = false;

  SEXP tree = Rf_protect(as_list_call(expr)); ++NP;
  SEXP branch;
  for (int i = 0; i < Rf_length(tree); ++i){
    branch = VECTOR_ELT(tree, i);

    // If branch is a call
    if (TYPEOF(branch) == LANGSXP){
      if (call_contains_dplyr_mask(branch, rho)){
        out = true;
        break;
      }
    }
    if (TYPEOF(branch) == SYMSXP){
      SEXP branch_name = Rf_protect(rlang::sym_as_character(branch)); ++NP;
      if (cpp_is_fn_call(branch_name, dplyr_mask_fns, dplyr_str, rho)){
        out = true;
        break;
      }
    }
  }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
bool cpp_any_quo_contains_dplyr_mask_call(SEXP quos){

  if (TYPEOF(quos) != VECSXP){
    Rf_error("`quos` must be a list of quosures in %s", __func__);
  }

  bool out = false;

  SEXP expr, quo_env;
  PROTECT_INDEX expr_idx, quo_env_idx;
  R_ProtectWithIndex(expr = R_NilValue, &expr_idx);
  R_ProtectWithIndex(quo_env = R_NilValue, &quo_env_idx);

  for (int i = 0; i < Rf_length(quos); ++i){
    R_Reprotect(expr = rlang::quo_get_expr(VECTOR_ELT(quos, i)), expr_idx);
    R_Reprotect(quo_env = rlang::quo_get_env(VECTOR_ELT(quos, i)), quo_env_idx);
    if (call_contains_dplyr_mask(expr, quo_env)){
      out = true;
      break;
    }
  }
  Rf_unprotect(2);
  return out;
}

SEXP get_mask_top_env(SEXP mask){
  if (TYPEOF(mask) != ENVSXP){
    Rf_error("Object must be a data mask `environment` in %s", __func__);
  }
  return Rf_findVar(Rf_install(".top_env"), mask);
}

// Just a wrapper around rlang::eval_tidy
// but only supplying a quosure and data mask

SEXP cpp_eval_tidy(SEXP quo, SEXP mask){
  SEXP expr = Rf_protect(rlang::quo_get_expr(quo));
  SEXP rho = Rf_protect(rlang::quo_get_env(quo));
  SEXP result = Rf_protect(rlang::eval_tidy(expr, mask, rho));
  Rf_unprotect(3);
  return result;
}

// Eval a list of quos

SEXP cpp_eval_all_tidy(SEXP quos, SEXP mask){
  int32_t NP = 0;
  int n_exprs = Rf_length(quos);
  SEXP expr_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
  if (TYPEOF(expr_names) == NILSXP){
    Rf_protect(expr_names = Rf_allocVector(STRSXP, n_exprs)); ++NP;
  }
  SEXP top_env = Rf_protect(get_mask_top_env(mask));++NP;

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_exprs)); ++NP;
  SEXP out_names = Rf_protect(Rf_allocVector(STRSXP, n_exprs)); ++NP;

  const SEXP *p_quos = VECTOR_PTR_RO(quos);
  const SEXP *p_expr_names = STRING_PTR_RO(expr_names);

  for (int i = 0; i < n_exprs; ++i){
    SEXP result = Rf_protect(cpp_eval_tidy(p_quos[i], mask)); ++NP;
    SEXP expr_name = p_expr_names[i];

    if (expr_name != R_BlankString){
      SEXP sym = Rf_installChar(expr_name);
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

SEXP new_bare_data_mask(){
  SEXP env = Rf_protect(R_NewEnv(R_EmptyEnv, false, 0));
  SEXP mask = Rf_protect(rlang::new_data_mask(env, env));
  SEXP top_env = Rf_protect(get_mask_top_env(mask));

  // Add .data pronoun
  SEXP data_pronoun_sym = Rf_install(".data");
  SEXP data_pronoun = Rf_protect(rlang::as_data_pronoun(env));
  Rf_defineVar(data_pronoun_sym, data_pronoun, top_env);

  Rf_unprotect(4);
  return mask;
}

[[cpp11::register]]
SEXP cpp_list_tidy(SEXP quos){
  SEXP mask = Rf_protect(new_bare_data_mask());
  SEXP out = Rf_protect(cpp_eval_all_tidy(quos, mask));
  Rf_unprotect(2); return out;
}

void set_as_tbl(SEXP x){
  SEXP tbl_class = Rf_protect(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));
  Rf_classgets(x, tbl_class);
  Rf_unprotect(1);
}

void set_as_vctrs_new_list_of_int(SEXP x){
  if (TYPEOF(x) != VECSXP){
    Rf_error("`x` must be a list of integers in %s", __func__);
  }
  SEXP rows_class = Rf_protect(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(rows_class, 0, Rf_mkChar("vctrs_list_of"));
  SET_STRING_ELT(rows_class, 1, Rf_mkChar("vctrs_vctr"));
  SET_STRING_ELT(rows_class, 2, Rf_mkChar("list"));
  Rf_setAttrib(x, Rf_install("ptype"), Rf_allocVector(INTSXP, 0));
  Rf_classgets(x, rows_class);
  Rf_unprotect(1);
}

// A pretty hacky way of recreating `seq_len()`
SEXP compact_int_seq_len(int n){
  SEXP r_n = Rf_protect(Rf_ScalarInteger(n));
  SEXP empty_list = Rf_protect(Rf_allocVector(VECSXP, 0));
  SEXP temp = Rf_protect(cheapr::new_df(empty_list, r_n, false, false));
  SEXP out = Rf_getAttrib(temp, R_RowNamesSymbol);
  Rf_unprotect(3);
  return out;
}


SEXP get_data_GRP(SEXP x){
  return Rf_getAttrib(x, Rf_install("GRP"));
}

[[cpp11::register]]
SEXP cpp_group_data(SEXP x){
  if (Rf_inherits(x, "grouped_df")){
    return Rf_getAttrib(x, Rf_install("groups"));
  } else if (Rf_inherits(x, "data.frame")){

    SEXP groups = Rf_protect(Rf_allocVector(VECSXP, 1));
    SEXP names = Rf_protect(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(names, 0, Rf_mkChar(".rows"));

    // Rows
    SEXP rows = Rf_protect(Rf_allocVector(VECSXP, 1));
    SET_VECTOR_ELT(rows, 0, compact_int_seq_len(df_nrow(x)));
    set_as_vctrs_new_list_of_int(rows);
    SET_VECTOR_ELT(groups, 0, rows);
    Rf_namesgets(groups, names);
    Rf_protect(groups = cheapr::list_as_df(groups));
    set_as_tbl(groups);
    Rf_unprotect(4);
    return groups;
  } else {
    Rf_error("`x` must be a data frame in %s", __func__);
  }
}

[[cpp11::register]]
SEXP cpp_group_keys(SEXP x){

  SEXP out = R_NilValue;

  if (Rf_inherits(x, "grouped_df")){
    SEXP group_data = Rf_protect(cpp_group_data(x));
    SEXP seq = Rf_protect(cheapr::seq_len(Rf_length(group_data) - 1));
    out = Rf_protect(cheapr::df_select(group_data, seq));
  } else {
    SEXP r_nrows = Rf_protect(Rf_ScalarInteger(1));
    SEXP empty_list = Rf_protect(Rf_allocVector(VECSXP, 0));
    out = Rf_protect(cheapr::new_df(empty_list, r_nrows, false, false));
  }
  set_as_tbl(out);
  Rf_unprotect(3);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_vars(SEXP x){
  if (Rf_inherits(x, "grouped_df")){
    SEXP group_keys = Rf_protect(cpp_group_keys(x));
    SEXP out = Rf_getAttrib(group_keys, R_NamesSymbol);
    Rf_unprotect(1);
    return out;
  } else {
    return Rf_allocVector(STRSXP, 0);
  }
  // return Rf_inherits(x, "grouped_df") ? Rf_getAttrib(cpp_group_keys(x), R_NamesSymbol) : Rf_allocVector(STRSXP, 0);
}

[[cpp11::register]]
SEXP cpp_group_rows(SEXP x){
  SEXP group_data = Rf_protect(cpp_group_data(x));
  SEXP out = VECTOR_ELT(group_data, Rf_length(group_data) - 1);
  Rf_unprotect(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_size(SEXP x){

  SEXP out = R_NilValue;

  if (Rf_inherits(x, "fastplyr_grouped_df")){
    SEXP grp = Rf_protect(get_data_GRP(x));
    Rf_protect(out = get_list_element(grp, "group.sizes"));
  } else {
    SEXP group_rows = Rf_protect(cpp_group_rows(x));
    Rf_protect(out = cheapr::lengths(group_rows, false));
  }
  Rf_unprotect(2);
  return out;
}


[[cpp11::register]]
SEXP cpp_ungroup(SEXP data){
  int32_t NP = 0;
  if (Rf_inherits(data, "grouped_df")){
    SEXP out = Rf_protect(Rf_shallow_duplicate(data)); ++NP;
    SEXP groups_sym = Rf_install("groups");
    SEXP grp_sym = Rf_install("GRP");
    Rf_setAttrib(out, groups_sym, R_NilValue);
    Rf_setAttrib(out, grp_sym, R_NilValue);
    SEXP old_class = Rf_protect(Rf_getAttrib(out, R_ClassSymbol)); ++NP;
    SEXP grouped_df_char = Rf_protect(Rf_mkChar("grouped_df")); ++NP;
    SEXP fp_grouped_df_char = Rf_protect(Rf_mkChar("fastplyr_grouped_df")); ++NP;
    SEXP grp_df_char = Rf_protect(Rf_mkChar("GRP_df")); ++NP;
    SEXP remove = Rf_protect(Rf_allocVector(STRSXP, 3)); ++NP;
    SET_STRING_ELT(remove, 0, grouped_df_char);
    SET_STRING_ELT(remove, 1, fp_grouped_df_char);
    SET_STRING_ELT(remove, 2, grp_df_char);

    SEXP new_class = Rf_protect(cheapr::setdiff(old_class, remove, false)); ++NP;
    Rf_classgets(out, new_class);
    Rf_unprotect(NP);
    return out;
  }
  Rf_unprotect(NP);
  return data;
}
// SEXP cpp_ungroup(SEXP data){
//   if (Rf_inherits(data, "grouped_df")){
//     SEXP out = Rf_protect(Rf_shallow_duplicate(data));
//     SEXP groups_sym = Rf_install("groups");
//     SEXP grp_sym = Rf_install("GRP");
//     Rf_setAttrib(out, groups_sym, R_NilValue);
//     Rf_setAttrib(out, grp_sym, R_NilValue);
//     SEXP old_class = Rf_getAttrib(out, R_ClassSymbol);
//     SEXP grouped_df_char = Rf_protect(Rf_mkChar("grouped_df"));
//     SEXP fp_grouped_df_char = Rf_protect(Rf_mkChar("fastplyr_grouped_df"));
//     SEXP grp_df_char = Rf_protect(Rf_mkChar("GRP_df"));
//     SEXP tp_char = Rf_protect(Rf_mkChar("time_tbl_df"));
//     SEXP remove = Rf_protect(Rf_allocVector(STRSXP, 4));
//     SET_STRING_ELT(remove, 0, grouped_df_char);
//     SET_STRING_ELT(remove, 1, fp_grouped_df_char);
//     SET_STRING_ELT(remove, 2, grp_df_char);
//     SET_STRING_ELT(remove, 3, tp_char);
//
//     SEXP new_class = Rf_protect(cheapr::setdiff(old_class, remove, false));
//     Rf_classgets(out, new_class);
//     Rf_unprotect(7);
//     return out;
//   }
//   return data;
// }

// Taken from dplyr::group_indices,
// All credits go to dplyr

[[cpp11::register]]
SEXP cpp_group_indices(SEXP rows, int size) {
  SEXP indices = Rf_protect(Rf_allocVector(INTSXP, size));
  int* __restrict__ p_indices = INTEGER(indices);
  int ng = Rf_length(rows);
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  for (int i = 0; i < ng; ++i) {
    SEXP rows_i = p_rows[i];
    int n_i = Rf_length(rows_i);
    const int* __restrict__ p_rows_i = INTEGER(rows_i);
    for (int j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }
  Rf_unprotect(1);
  return indices;
}

[[cpp11::register]]
bool cpp_group_by_drop_default(SEXP x){
  if (Rf_inherits(x, "grouped_df")){
    SEXP groups = Rf_protect(cpp_group_data(x));
    SEXP drop_sym = Rf_protect(Rf_install(".drop"));
    SEXP out = Rf_getAttrib(groups, drop_sym);
    Rf_unprotect(2);
    return out;
  } else {
    return true;
  }
}

[[cpp11::register]]
bool cpp_group_by_order_default(SEXP x){

  if (!Rf_inherits(x, "data.frame")){
    Rf_error("`x` must be a data frame in %s", __func__);
  }

  int32_t NP = 0;

  bool out = true;

  SEXP ordered_sym = Rf_protect(Rf_install("ordered")); ++NP;

  if (Rf_inherits(x, "grouped_df")){
    SEXP group_data = Rf_protect(cpp_group_data(x)); ++NP;
    SEXP ordered = Rf_protect(Rf_getAttrib(group_data, ordered_sym)); ++NP;
    if (TYPEOF(ordered) == NILSXP){
      out = true;
      Rf_unprotect(NP);
      return out;
    } else if (Rf_length(ordered) == 1){
      out = LOGICAL(ordered)[0];
      Rf_unprotect(NP);
      return out;
    }
  }

  SEXP fp_order_groups_sym = Rf_protect(Rf_install(".fastplyr.order.groups")); ++NP;
  SEXP order_groups = Rf_protect(Rf_GetOption1(fp_order_groups_sym)); ++NP;

  if (TYPEOF(order_groups) != NILSXP){
    if (TYPEOF(order_groups) != LGLSXP || Rf_length(order_groups) != 1){
      Rf_unprotect(NP);
      Rf_error("'.fastplyr.order.groups' option must be either `TRUE` or `FALSE`");
    }
    out = LOGICAL(order_groups)[0];
    if (out == NA_LOGICAL){
      Rf_unprotect(NP);
      Rf_error("'.fastplyr.order.groups' option must be either `TRUE` or `FALSE`");
    }
  }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
int n_group_vars(SEXP x){
  return Rf_length(cpp_group_vars(x));
}

[[cpp11::register]]
SEXP cpp_group_id(SEXP x){
  if (!Rf_inherits(x, "grouped_df") && !Rf_inherits(x, "data.frame")){
    Rf_error("Can only calculate group indices on data frames in %s", __func__);
  }

  if (Rf_inherits(x, "fastplyr_grouped_df")){
    SEXP grp = Rf_protect(get_data_GRP(x));
    SEXP out = Rf_protect(get_list_element(grp, "group.id"));
    Rf_unprotect(2);
    return out;
  }

  int n = df_nrow(x);
  SEXP out;
  if (n_group_vars(x) == 0){
    SEXP r_one = Rf_protect(Rf_ScalarInteger(1));
    out = Rf_protect(cheapr::rep_len(r_one, n));
  } else {
    SEXP group_rows = Rf_protect(cpp_group_rows(x));
    out = Rf_protect(cpp_group_indices(group_rows, n));
  }
  Rf_unprotect(2);
  return out;
}


// unlist `group_data(data)$.rows` quickly

[[cpp11::register]]
SEXP cpp_unlist_group_locs(SEXP x, SEXP group_sizes){
  if (TYPEOF(x) != VECSXP){
    return x;
  }
  int n = Rf_length(x);
  int m = 0, k = 0, out_size = 0;
  const SEXP *p_x = VECTOR_PTR_RO(x);

  if (Rf_isNull(group_sizes)){

    std::vector<const int*> loc_ptrs(n);

    // Figure out unlisted length
    for (int i = 0; i < n; ++i){
      out_size += Rf_length(p_x[i]);
      loc_ptrs[i] = INTEGER_RO(p_x[i]);
    }

    SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
    int* __restrict__ p_out = INTEGER(out);

    for (int i = 0; i < n; k += m, ++i){
      m = Rf_length(p_x[i]);
      if (m != 0){
        memcpy(&p_out[k], &loc_ptrs[i][0], m * sizeof(int));
      }
    }
    Rf_unprotect(1);
    return out;
  } else {
    if (Rf_length(group_sizes) != n){
      Rf_error("`length(x)` must match `length(group_sizes)`");
    }
    const int* __restrict__ p_gs = INTEGER_RO(group_sizes);
    std::vector<const int*> loc_ptrs(n);

    // Figure out unlisted length
    for (int i = 0; i < n; ++i){
      out_size += p_gs[i];
      loc_ptrs[i] = INTEGER_RO(p_x[i]);
    }

    SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
    int* __restrict__ p_out = INTEGER(out);

    for (int i = 0; i < n; k += m, ++i){
      m = p_gs[i];
      if (m != 0){
        memcpy(&p_out[k], &loc_ptrs[i][0], m * sizeof(int));
      }
    }
    Rf_unprotect(1);
    return out;
  }
}

// Are group IDs sorted?
// This function expects no NAs
[[cpp11::register]]
bool cpp_group_id_sorted(SEXP x){
  bool out = true;
  int n = Rf_length(x);
  const int* __restrict__ p_x = INTEGER(x);
  for (int i = 1; i < n; ++i){
    if (p_x[i] < p_x[i - 1]){
      return false;
    }
  }
  return out;
}

// SEXP cpp_df_as_collapse_grp(SEXP x, bool return_order){
//   int32_t NP = 0;
//
//   SEXP out = Rf_protect(Rf_allocVector(VECSXP, 9)); ++NP;
//   SEXP out_names = Rf_protect(Rf_allocVector(STRSXP, 9)); ++NP;
//   SET_STRING_ELT(out_names, 0, Rf_mkChar("N.groups"));
//   SET_STRING_ELT(out_names, 1, Rf_mkChar("group.id"));
//   SET_STRING_ELT(out_names, 2, Rf_mkChar("group.sizes"));
//   SET_STRING_ELT(out_names, 3, Rf_mkChar("groups"));
//   SET_STRING_ELT(out_names, 4, Rf_mkChar("group.vars"));
//   SET_STRING_ELT(out_names, 5, Rf_mkChar("ordered"));
//   SET_STRING_ELT(out_names, 6, Rf_mkChar("order"));
//   SET_STRING_ELT(out_names, 7, Rf_mkChar("group.starts"));
//   SET_STRING_ELT(out_names, 8, Rf_mkChar("call"));
//   Rf_namesgets(out, out_names);
//
//   int n_rows = df_nrow(x);
//
//   SEXP group_data = Rf_protect(cpp_group_data(x)); ++NP;
//   SEXP group_vars = Rf_protect(cpp_group_vars(x)); ++NP;
//   int n_groups = df_nrow(group_data);
//   SEXP group_keys = Rf_protect(cpp_group_keys(x)); ++NP;
//   SEXP group_id = Rf_protect(cpp_group_id(x)); ++NP;
//   SEXP group_rows = Rf_protect(cpp_group_rows(x)); ++NP;
//   SEXP group_sizes = Rf_protect(cheapr::lengths(group_rows, false)); ++NP;
//   int ngvars = n_group_vars(x);
//
//   SEXP group_order = R_NilValue;
//   SEXP sorted = Rf_protect(Rf_allocVector(LGLSXP, 1)); ++NP;
//   LOGICAL(sorted)[0] = NA_LOGICAL;
//
//   if (return_order){
//     if (ngvars == 0){
//       Rf_protect(group_order = VECTOR_ELT(group_rows, 0)); ++NP;
//       LOGICAL(sorted)[0] = TRUE;
//     } else {
//       Rf_protect(group_order = cpp_unlist_group_locs(group_rows, group_sizes)); ++NP;
//     }
//   }
//   SEXP group_ordered = Rf_protect(Rf_allocVector(LGLSXP, 2)); ++NP;
//   LOGICAL(group_ordered)[0] = TRUE;
//   LOGICAL(group_ordered)[1] = LOGICAL(sorted)[0];
//   SEXP group_ordered_names = Rf_protect(Rf_allocVector(STRSXP, 2)); ++NP;
//   SET_STRING_ELT(group_ordered_names, 0, Rf_mkChar("ordered"));
//   SET_STRING_ELT(group_ordered_names, 1, Rf_mkChar("sorted"));
//   Rf_namesgets(group_ordered, group_ordered_names);
//   SEXP group_starts;
//
//     if (ngvars == 0){
//       Rf_protect(group_starts = Rf_ScalarInteger(std::min(1, n_rows))); ++NP;
//     } else {
//       SEXP r_one = Rf_protect(Rf_ScalarInteger(1)); ++NP;
//       Rf_protect(group_starts = cpp11::package("fastplyr")["list_subset"](group_rows, r_one)); ++NP;
//     }
//     SET_VECTOR_ELT(out, 0, Rf_ScalarInteger(n_groups));
//     SET_VECTOR_ELT(out, 1, group_id);
//     SET_VECTOR_ELT(out, 2, group_sizes);
//     if (ngvars != 0){
//       SET_VECTOR_ELT(out, 3, group_keys);
//       SET_VECTOR_ELT(out, 4, group_vars);
//     }
//     SET_VECTOR_ELT(out, 7, group_starts);
//     if (TYPEOF(group_order) != NILSXP){
//       SET_VECTOR_ELT(out, 6, group_order);
//     }
//     SET_VECTOR_ELT(out, 5, group_ordered);
//     Rf_classgets(out, Rf_mkString("GRP"));
//     Rf_unprotect(NP);
//     return out;
// }
// the recycle arg recyles results on a by-group basis
// useful for `reframe()`
//

[[cpp11::register]]
SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos, bool recycle, bool add_groups){
  int32_t NP = 0;
  int n_quos = Rf_length(quos);

  if (n_quos == 0){
    SEXP out = Rf_protect(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(out, 0, Rf_allocVector(VECSXP, 0));
    SET_VECTOR_ELT(out, 1, Rf_allocVector(VECSXP, 0));
    Rf_namesgets(VECTOR_ELT(out, 0), Rf_allocVector(STRSXP, 0));
    Rf_namesgets(VECTOR_ELT(out, 1), Rf_allocVector(STRSXP, 0));
    SEXP out_names = Rf_protect(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(out_names, 0, Rf_mkChar("groups"));
    SET_STRING_ELT(out_names, 1, Rf_mkChar("results"));
    Rf_namesgets(out, out_names);
    Rf_unprotect(2);
    return out;
  }

  bool has_groups = Rf_inherits(data, "grouped_df");
  SEXP group_data = R_NilValue;
  SEXP groups = Rf_protect(cpp_group_keys(data)); ++NP;
  SEXP rows = R_NilValue;
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
  const SEXP *p_quo_name_syms = VECTOR_PTR_RO(quo_name_syms);
  const SEXP *p_exprs = VECTOR_PTR_RO(exprs);
  const SEXP *p_envs = VECTOR_PTR_RO(envs);
  const SEXP *p_rows;

  int n_groups = 1;

  if (has_groups){
    Rf_protect(group_data = cpp_group_data(data)); ++NP;
    n_groups = df_nrow(group_data);

    // Get group locations
    Rf_protect(rows = VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
    p_rows = VECTOR_PTR_RO(rows);
  } else {
    p_rows = VECTOR_PTR_RO(data);
  }

  n_groups = std::max(n_groups, 1);


  // grab the variable names the expressions point to
  SEXP quo_data_vars = Rf_protect(cpp_quo_data_vars(quos, data)); ++NP;
  int chunk_n_cols = Rf_length(quo_data_vars);
  SEXP quo_data_syms = Rf_protect(Rf_allocVector(VECSXP, chunk_n_cols)); ++NP;
  const SEXP *p_quo_data_syms = VECTOR_PTR_RO(quo_data_syms);

  for (int i = 0; i < chunk_n_cols; ++i){
    SET_VECTOR_ELT(quo_data_syms, i, Rf_installChar(STRING_ELT(quo_data_vars, i)));
  }

  // Initialise expressions, environments and symbols of expression names
  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
    if (STRING_ELT(quo_names, i) == R_BlankString){
      SET_VECTOR_ELT(quo_name_syms, i, R_UnboundValue);
    } else {
      SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
    }
  }

  // Initialise components

  SEXP data_subset = Rf_protect(cheapr::df_select(data, quo_data_vars)); ++NP;

  // We will re-use the mask across all groups but add the same
  // vars for each slice in each iteration
  SEXP mask = Rf_protect(new_bare_data_mask()); ++NP;
  SEXP top_env = Rf_protect(get_mask_top_env(mask)); ++NP;

  SEXP chunk_locs, chunk, result, inner_container;

  PROTECT_INDEX chunk_locs_idx, chunk_idx,
  result_idx, inner_container_idx;

  R_ProtectWithIndex(chunk_locs = R_NilValue, &chunk_locs_idx); ++NP;
  R_ProtectWithIndex(chunk = data_subset, &chunk_idx); ++NP;
  R_ProtectWithIndex(result = R_NilValue, &result_idx); ++NP;
  R_ProtectWithIndex(inner_container = R_NilValue, &inner_container_idx); ++NP;


  // outer container will contain lists of expressions for each group
  // Basically for the mask to work nicely we loop through each and group and
  // in each group we loop through the expressions

  // At the end we invert that so that we have a
  // list of results of length `length(quos)`

  SEXP outer_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;

  // Recycling in this context means to recycle the results to a common size
  // on a by-group basis, meaning the recycled sizes may differ between different
  // groups

  int recycled_size;
  int result_size;

  SEXP recycled_sizes;
  PROTECT_INDEX recycled_sizes_idx;
  R_ProtectWithIndex(recycled_sizes = R_NilValue, &recycled_sizes_idx); ++NP;

  SEXP recycled_sizes_container = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  const SEXP *p_recycled_sizes_container = VECTOR_PTR_RO(recycled_sizes_container);
  std::vector<int *> recycled_pointers(n_quos);

  // Some C trickery here.. We repeat out the rows of the group keys at the end
  // but if we recycle we only need to do this once and we only need one
  // result sizes vector, otherwise we need one for each expression
  if (recycle){
    R_Reprotect(recycled_sizes = Rf_allocVector(INTSXP, n_groups), recycled_sizes_idx);
    recycled_pointers[0] = INTEGER(recycled_sizes);
    for (int m = 0; m < n_quos; ++m){
      SET_VECTOR_ELT(recycled_sizes_container, m, recycled_sizes);
      recycled_pointers[m] = recycled_pointers[0];
    }
  } else {
    for (int m = 0; m < n_quos; ++m){
      SET_VECTOR_ELT(recycled_sizes_container, m, Rf_allocVector(INTSXP, n_groups));
      recycled_pointers[m] = INTEGER(VECTOR_ELT(recycled_sizes_container, m));
    }
  }


  for (int i = 0; i < n_groups; ++i){

    recycled_size = -1; // Initialise this to < 0 for later logic to work

    // Filter on the rows relevant to the current group

    if (has_groups){
      chunk_locs = p_rows[i];
      R_Reprotect(chunk = cheapr::df_slice(data_subset, chunk_locs, false), chunk_idx);
    }

    // assign variables to existing data mask
    // essentially list2env()

    for (int l = 0; l < chunk_n_cols; ++l){
      Rf_defineVar(p_quo_data_syms[l], VECTOR_ELT(chunk, l), top_env);
    }

    // inner container will contain the results of our expressions
    // and we assign these inner containers to the bigger outer container
    R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_quos), inner_container_idx);

    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(
        p_exprs[m], mask, p_envs[m]
      ), result_idx);
      if (p_quo_name_syms[m] != R_UnboundValue){
        Rf_defineVar(p_quo_name_syms[m], result, top_env);
      }
      SET_VECTOR_ELT(inner_container, m, result);
      result_size = cheapr::vec_length(result);
      recycled_size = recycle ? (result_size == 0 ? 0 : recycled_size > result_size ? recycled_size : result_size) : result_size;
      recycled_pointers[m][i] = recycled_size;
    }

    // recycle expression results to a common length

    if (recycle){
      for (int m = 0; m < n_quos; ++m){
        SET_VECTOR_ELT(inner_container, m, cheapr::rep_len(VECTOR_ELT(inner_container, m), recycled_size));
      }
    }
    SET_VECTOR_ELT(outer_container, i, inner_container);
  }

  // Combine results

  SEXP results = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  Rf_setAttrib(results, R_NamesSymbol, quo_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);


  // groups container will hold the repeated out rows of the group keys
  SEXP groups_container = R_NilValue;
  if (add_groups){
    Rf_protect(groups_container = Rf_allocVector(VECSXP, n_quos)); ++NP;
    Rf_setAttrib(groups_container, R_NamesSymbol, quo_names);
  }

  SEXP repeated_groups;
  PROTECT_INDEX repeated_groups_idx;
  R_ProtectWithIndex(repeated_groups = R_NilValue, &repeated_groups_idx); ++NP;

  if (add_groups && recycle && n_quos > 0){
    R_Reprotect(repeated_groups = cheapr::rep(groups, p_recycled_sizes_container[0]), repeated_groups_idx);
  }
  for (int m = 0; m < n_quos; ++m){
    R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_groups), inner_container_idx);
    for (int j = 0; j < n_groups; ++j){
      SET_VECTOR_ELT(inner_container, j, VECTOR_ELT(p_outer_container[j], m));
    }
    R_Reprotect(result = cheapr::c(inner_container), result_idx);
    if (add_groups){
      if (!recycle){
        R_Reprotect(repeated_groups = cheapr::rep(groups, p_recycled_sizes_container[m]), repeated_groups_idx);
      }
      SET_VECTOR_ELT(groups_container, m, repeated_groups);
    }
    SET_VECTOR_ELT(results, m, result);
  }
  SEXP out = Rf_protect(Rf_allocVector(VECSXP, 2)); ++NP;
  SET_VECTOR_ELT(out, 0, groups_container);
  SET_VECTOR_ELT(out, 1, results);
  SEXP out_names = Rf_protect(Rf_allocVector(STRSXP, 2)); ++NP;
  SET_STRING_ELT(out_names, 0, Rf_mkChar("groups"));
  SET_STRING_ELT(out_names, 1, Rf_mkChar("results"));
  Rf_namesgets(out, out_names);
  Rf_unprotect(NP);
  return out;
}
// Working, pretty fast but very messy

[[cpp11::register]]
SEXP cpp_grouped_eval_mutate(SEXP data, SEXP quos){
  int32_t NP = 0;
  int n_quos = Rf_length(quos);

  if (n_quos == 0){
    SEXP out = Rf_protect(Rf_allocVector(VECSXP, 0));
    Rf_namesgets(out, Rf_allocVector(STRSXP, 0));
    Rf_unprotect(1);
    return out;
  }

  bool has_groups = Rf_inherits(data, "grouped_df");
  int n_rows = df_nrow(data);
  SEXP group_data = R_NilValue;
  SEXP rows = R_NilValue;
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
  const SEXP *p_quo_name_syms = VECTOR_PTR_RO(quo_name_syms);
  const SEXP *p_exprs = VECTOR_PTR_RO(exprs);
  const SEXP *p_envs = VECTOR_PTR_RO(envs);
  const SEXP *p_rows;

  int n_groups = 1;

  if (has_groups){
    Rf_protect(group_data = cpp_group_data(data)); ++NP;
    n_groups = df_nrow(group_data);

    // Get group locations
    Rf_protect(rows = VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
    p_rows = VECTOR_PTR_RO(rows);
  } else {
    p_rows = VECTOR_PTR_RO(data);
  }

  n_groups = std::max(n_groups, 1);


  // grab the variable names the expressions point to
  SEXP quo_data_vars = Rf_protect(cpp_quo_data_vars(quos, data)); ++NP;
  int chunk_n_cols = Rf_length(quo_data_vars);
  SEXP quo_data_syms = Rf_protect(Rf_allocVector(VECSXP, chunk_n_cols)); ++NP;
  const SEXP *p_quo_data_syms = VECTOR_PTR_RO(quo_data_syms);

  for (int i = 0; i < chunk_n_cols; ++i){
    SET_VECTOR_ELT(quo_data_syms, i, Rf_installChar(STRING_ELT(quo_data_vars, i)));
  }

  // Initialise expressions, environments and symbols of expression names
  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
    if (STRING_ELT(quo_names, i) == R_BlankString){
      SET_VECTOR_ELT(quo_name_syms, i, R_UnboundValue);
    } else {
      SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
    }
  }

  // Initialise components

  SEXP data_subset = Rf_protect(cheapr::df_select(data, quo_data_vars)); ++NP;

  // We will re-use the mask across all groups but add the same
  // vars for each slice in each iteration
  SEXP mask = Rf_protect(new_bare_data_mask()); ++NP;
  SEXP top_env = Rf_protect(get_mask_top_env(mask)); ++NP;

  SEXP chunk_locs, chunk, result, inner_container;

  PROTECT_INDEX chunk_locs_idx, chunk_idx,
  result_idx, inner_container_idx;

  R_ProtectWithIndex(chunk_locs = R_NilValue, &chunk_locs_idx); ++NP;
  R_ProtectWithIndex(chunk = data_subset, &chunk_idx); ++NP;
  R_ProtectWithIndex(result = R_NilValue, &result_idx); ++NP;
  R_ProtectWithIndex(inner_container = R_NilValue, &inner_container_idx); ++NP;


  // outer container will contain lists of expressions for each group
  // Basically for the mask to work nicely we loop through each and group and
  // in each group we loop through the expressions

  // At the end we invert that so that we have a
  // list of results of length `length(quos)`

  SEXP outer_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;

  int chunk_size;

  for (int i = 0; i < n_groups; ++i){

    // Filter on the rows relevant to the current group

    if (has_groups){
      chunk_locs = p_rows[i];
      R_Reprotect(chunk = cheapr::df_slice(data_subset, chunk_locs, false), chunk_idx);
      chunk_size = Rf_length(chunk_locs);
    } else {
     chunk_size = n_rows;
    }


    // assign variables to existing data mask
    // essentially list2env()

    for (int l = 0; l < chunk_n_cols; ++l){
      Rf_defineVar(p_quo_data_syms[l], VECTOR_ELT(chunk, l), top_env);
    }

    // inner container will contain the results of our expressions
    // and we assign these inner containers to the bigger outer container
    R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_quos), inner_container_idx);

    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(
        p_exprs[m], mask, p_envs[m]
      ), result_idx);
      R_Reprotect(result = cheapr::rep_len(result, chunk_size), result_idx);
      if (p_quo_name_syms[m] != R_UnboundValue){
        Rf_defineVar(p_quo_name_syms[m], result, top_env);
      }
      SET_VECTOR_ELT(inner_container, m, result);
    }
    SET_VECTOR_ELT(outer_container, i, inner_container);
  }

  // Combine results

  SEXP results = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  Rf_setAttrib(results, R_NamesSymbol, quo_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);

  for (int m = 0; m < n_quos; ++m){
    R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_groups), inner_container_idx);
    for (int j = 0; j < n_groups; ++j){
      SET_VECTOR_ELT(inner_container, j, VECTOR_ELT(p_outer_container[j], m));
    }
    SET_VECTOR_ELT(results, m, cheapr::c(inner_container));
  }

  SEXP group_id = R_NilValue;
  SEXP group_sizes = R_NilValue;
  SEXP order = R_NilValue;
  SEXP is_already_ordered = R_NilValue;
  SEXP sorted_sym = R_NilValue;
  SEXP grp_sym = Rf_protect(Rf_install(".GRP")); ++NP;
  SEXP grp = Rf_protect(Rf_getAttrib(quos, grp_sym)); ++NP;

  // Initialise
  SEXP results_without_null;
  PROTECT_INDEX results_without_null_idx;
  R_ProtectWithIndex(results_without_null = R_NilValue, &results_without_null_idx); ++NP;

  if (n_groups > 1){
    // Re-order the results
    R_Reprotect(results_without_null = cheapr::list_as_df(results), results_without_null_idx);

    if (TYPEOF(grp) == NILSXP){
      Rf_protect(group_id = cpp_group_id(data)); ++NP;
      Rf_protect(group_sizes = cpp_group_size(data)); ++NP;
    } else {
      Rf_protect(group_id = get_list_element(grp, "group.id")); ++NP;
      Rf_protect(group_sizes = get_list_element(grp, "group.sizes")); ++NP;
    }
    Rf_protect(order = cpp_orig_order(group_id, group_sizes)); ++NP;
    Rf_protect(sorted_sym = Rf_install("sorted")); ++NP;
    Rf_protect(is_already_ordered = Rf_getAttrib(order, sorted_sym)); ++NP;
    if (TYPEOF(is_already_ordered) != LGLSXP || !LOGICAL(is_already_ordered)[0]){
      R_Reprotect(results_without_null = cheapr::sset(results_without_null, order, true), results_without_null_idx);
    }
    Rf_protect(results = cheapr::list_assign(results, results_without_null)); ++NP;
  }
  Rf_unprotect(NP);
  return results;
}

[[cpp11::register]]
SEXP cpp_nest_split(SEXP data, SEXP drop, SEXP order){
  int32_t NP = 0;

  SEXP tbl_class = Rf_protect(Rf_allocVector(STRSXP, 3)); ++NP;
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));

  SEXP group_data = Rf_protect(cpp_group_data(data)); ++NP;
  SEXP group_vars = Rf_protect(cpp_group_vars(data)); ++NP;
  SEXP rows = Rf_protect(cpp_group_rows(data)); ++NP;
  SEXP names = Rf_protect(Rf_getAttrib(data, R_NamesSymbol)); ++NP;
  SEXP locs, frame;

  PROTECT_INDEX locs_idx, frame_idx;
  R_ProtectWithIndex(locs = R_NilValue, &locs_idx); ++NP;
  R_ProtectWithIndex(frame = R_NilValue, &frame_idx); ++NP;

  SEXP temp_cols = Rf_protect(cheapr::setdiff(names, group_vars, false)); ++NP;
  SEXP temp = Rf_protect(cheapr::df_select(data, temp_cols)); ++NP;

  const SEXP *p_rows = VECTOR_PTR_RO(rows);
  int n_groups = Rf_length(rows);
  SEXP frames = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
  SHALLOW_DUPLICATE_ATTRIB(frames, rows);


  // Slice group chunks

  for (int i = 0; i < n_groups; ++i){
    R_Reprotect(locs = p_rows[i], locs_idx);
    R_Reprotect(frame = cheapr::df_slice(temp, locs, false), frame_idx);
    Rf_classgets(frame, tbl_class);
    SET_VECTOR_ELT(frames, i, frame);
  }

  SEXP out = Rf_protect(Rf_shallow_duplicate(group_data)); ++NP;
  SEXP out_names = Rf_protect(Rf_duplicate(Rf_getAttrib(out, R_NamesSymbol))); ++NP;
  SET_STRING_ELT(out_names, Rf_length(out) - 1, Rf_mkChar("data"));
  Rf_namesgets(out, out_names);


  // Set prototype of data

  SEXP frames_class = Rf_protect(Rf_allocVector(STRSXP, 3)); ++NP;
  SET_STRING_ELT(frames_class, 0, Rf_mkChar("vctrs_list_of"));
  SET_STRING_ELT(frames_class, 1, Rf_mkChar("vctrs_vctr"));
  SET_STRING_ELT(frames_class, 2, Rf_mkChar("list"));
  SEXP frame_ptype = Rf_protect(cheapr::get_ptype(VECTOR_ELT(frames, 0))); ++NP;
  set_as_tbl(frame_ptype);
  Rf_setAttrib(frames, Rf_install("ptype"), frame_ptype);
  Rf_classgets(frames, frames_class);

  SET_VECTOR_ELT(out, Rf_length(out) - 1, frames);

  // Add groups attribute

  SEXP groups = Rf_protect(Rf_shallow_duplicate(group_data)); ++NP;
  Rf_setAttrib(groups, Rf_install(".drop"), drop);
  Rf_setAttrib(groups, Rf_install("ordered"), order);
  SEXP group_rows_seq = Rf_protect(cheapr::seq_len(df_nrow(groups))); ++NP;
  SEXP group_rows = Rf_protect(Rf_coerceVector(group_rows_seq, VECSXP)); ++NP;
  set_as_vctrs_new_list_of_int(group_rows);
  SET_VECTOR_ELT(groups, Rf_length(groups) - 1, group_rows);
  Rf_setAttrib(out, Rf_install("groups"), groups);

  SEXP out_class = Rf_protect(Rf_allocVector(STRSXP, 4)); ++NP;

  SET_STRING_ELT(out_class, 0, Rf_mkChar("grouped_df"));
  SET_STRING_ELT(out_class, 1, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(out_class, 2, Rf_mkChar("tbl"));
  SET_STRING_ELT(out_class, 3, Rf_mkChar("data.frame"));

  Rf_classgets(out, out_class);
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_split(SEXP data){

  SEXP rows = Rf_protect(cpp_group_rows(data));
  SEXP tbl_class = Rf_protect(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));

  const SEXP *p_rows = VECTOR_PTR_RO(rows);
  int n_groups = Rf_length(rows);
  SEXP frames = Rf_protect(Rf_allocVector(VECSXP, n_groups));

  Rf_protect(data = cpp_ungroup(data));


  SEXP frame;
  PROTECT_INDEX frame_idx;
  R_ProtectWithIndex(frame = R_NilValue, &frame_idx);

  // Slice group chunks

  for (int i = 0; i < n_groups; ++i){
    R_Reprotect(frame = cheapr::df_slice(data, p_rows[i], false), frame_idx);
    Rf_classgets(frame, tbl_class);
    SET_VECTOR_ELT(frames, i, frame);

    // A method that is fast and preserves structure of input data
    // R_Reprotect(frame = cheapr::df_slice(data, p_rows[i], false), frame_idx);
    // SET_VECTOR_ELT(frames, i, cheapr::rebuild(frame, data, false));
  }

  Rf_unprotect(5);
  return frames;
}

// A fast method for converting a 'grouped_df' into a 'GRP'

[[cpp11::register]]
SEXP cpp_grouped_df_as_grp(SEXP data){
  int32_t NP = 0;
  int nrows = df_nrow(data);

  // Initialise needed symbols
  SEXP grp_char = Rf_protect(Rf_mkCharCE("GRP", CE_UTF8)); ++NP;

  SEXP n_groups_char = Rf_protect(Rf_mkCharCE("N.groups", CE_UTF8)); ++NP;
  SEXP group_id_char = Rf_protect(Rf_mkCharCE("group.id", CE_UTF8)); ++NP;
  SEXP group_sizes_char = Rf_protect(Rf_mkCharCE("group.sizes", CE_UTF8)); ++NP;
  SEXP groups_char = Rf_protect(Rf_mkCharCE("groups", CE_UTF8)); ++NP;
  SEXP group_vars_char = Rf_protect(Rf_mkCharCE("group.vars", CE_UTF8)); ++NP;
  SEXP order_char = Rf_protect(Rf_mkCharCE("order", CE_UTF8)); ++NP;
  SEXP group_starts_char = Rf_protect(Rf_mkCharCE("group.starts", CE_UTF8)); ++NP;
  SEXP call_char = Rf_protect(Rf_mkCharCE("call", CE_UTF8)); ++NP;
  SEXP locs_char = Rf_protect(Rf_mkCharCE("locs", CE_UTF8)); ++NP;

  SEXP starts_char = Rf_protect(Rf_mkCharCE("starts", CE_UTF8)); ++NP;
  SEXP maxgrpn_char = Rf_protect(Rf_mkCharCE("maxgrpn", CE_UTF8)); ++NP;
  SEXP ordered_char = Rf_protect(Rf_mkCharCE("ordered", CE_UTF8)); ++NP;
  SEXP sorted_char = Rf_protect(Rf_mkCharCE("sorted", CE_UTF8)); ++NP;


  SEXP grp = Rf_protect(Rf_getAttrib(data, Rf_installChar(grp_char))); ++NP;
  if (TYPEOF(grp) != NILSXP){
    Rf_unprotect(NP);
    return grp;
  }

  // Initialise needed components

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, 10)); ++NP;
  SEXP out_names = Rf_protect(Rf_allocVector(STRSXP, 10)); ++NP;
  SET_STRING_ELT(out_names, 0, n_groups_char);
  SET_STRING_ELT(out_names, 1, group_id_char);
  SET_STRING_ELT(out_names, 2, group_sizes_char);
  SET_STRING_ELT(out_names, 3, groups_char);
  SET_STRING_ELT(out_names, 4, group_vars_char);
  SET_STRING_ELT(out_names, 5, ordered_char);
  SET_STRING_ELT(out_names, 6, order_char);
  SET_STRING_ELT(out_names, 7, group_starts_char);
  SET_STRING_ELT(out_names, 8, call_char);
  SET_STRING_ELT(out_names, 9, locs_char);
  Rf_namesgets(out, out_names);

  SEXP group_data = Rf_protect(cpp_group_data(data)); ++NP;
  int n_group_vars = Rf_length(group_data) - 1;
  SEXP group_rows = VECTOR_ELT(group_data, n_group_vars);
  int ngroups = Rf_length(group_rows);

  bool groups_are_ordered = cpp_group_by_order_default(data);

  SEXP grp_class = Rf_protect(Rf_ScalarString(grp_char)); ++NP;
  SEXP n_groups = Rf_protect(Rf_ScalarInteger(ngroups)); ++NP;
  SEXP group_id = Rf_protect(Rf_allocVector(INTSXP, nrows)); ++NP;
  SEXP group_order = Rf_protect(Rf_allocVector(INTSXP, nrows)); ++NP;
  SEXP group_starts = Rf_protect(Rf_allocVector(INTSXP, nrows == 0 ? 0 : ngroups)); ++NP;
  SEXP group_sizes = Rf_protect(Rf_allocVector(INTSXP, nrows == 0 ? 0 : ngroups)); ++NP;
  SEXP sorted_group_starts = Rf_protect(Rf_allocVector(INTSXP, nrows == 0 ? 0 : ngroups)); ++NP;
  SEXP sorted = Rf_protect(Rf_allocVector(LGLSXP, 1)); ++NP;
  SEXP r_max_group_size = Rf_protect(Rf_allocVector(INTSXP, 1)); ++NP;
  SEXP groups = Rf_protect(cpp_group_keys(data)); ++NP;
  SEXP group_vars = Rf_protect(cpp_group_vars(data)); ++NP;
  SEXP ordered = Rf_protect(Rf_allocVector(LGLSXP, 2)); ++NP;
  SEXP ordered_nms = Rf_protect(Rf_allocVector(STRSXP, 2)); ++NP;

  SEXP group_locs = Rf_protect(Rf_shallow_duplicate(group_rows)); ++NP;
  Rf_protect(group_locs = cheapr::set_rm_attrs(group_locs)); ++NP;

  LOGICAL(ordered)[0] = groups_are_ordered;
  LOGICAL(ordered)[1] = groups_are_ordered ? true : NA_LOGICAL;
  SET_STRING_ELT(ordered_nms, 0, ordered_char);
  SET_STRING_ELT(ordered_nms, 1, sorted_char);
  Rf_namesgets(ordered, ordered_nms);

  // Pointers

  int* __restrict__ p_group_id = INTEGER(group_id);
  int* __restrict__ p_group_order = INTEGER(group_order);
  int* __restrict__ p_group_starts = INTEGER(group_starts);
  int* __restrict__ p_group_sizes = INTEGER(group_sizes);
  int* __restrict__ p_sorted_group_starts = INTEGER(sorted_group_starts);
  const SEXP* p_group_rows = VECTOR_PTR_RO(group_rows);

  // If no groups then the results are trivial

  if (ngroups <= 1){
    LOGICAL(sorted)[0] = true;
    INTEGER(r_max_group_size)[0] = nrows;
    if (nrows != 0){
      p_group_starts[0] = 1;
      p_group_sizes[0] = nrows;
      p_sorted_group_starts[0] = 1;
    }
    for (int i = 0; i < nrows; ++i){
      p_group_id[i] = 1;
      p_group_order[i] = i + 1;
    }

    Rf_setAttrib(group_order, Rf_installChar(starts_char), sorted_group_starts);
    Rf_setAttrib(group_order, Rf_installChar(maxgrpn_char), r_max_group_size);
    Rf_setAttrib(group_order, Rf_installChar(sorted_char), sorted);

    SET_VECTOR_ELT(out, 0, n_groups);
    SET_VECTOR_ELT(out, 1, group_id);
    SET_VECTOR_ELT(out, 2, group_sizes);
    SET_VECTOR_ELT(out, 3, groups);
    SET_VECTOR_ELT(out, 4, group_vars);
    SET_VECTOR_ELT(out, 5, ordered);
    SET_VECTOR_ELT(out, 6, group_order);
    SET_VECTOR_ELT(out, 7, group_starts);
    SET_VECTOR_ELT(out, 9, group_locs);
    Rf_classgets(out, grp_class);
    Rf_unprotect(NP);
    return out;
  }

  bool groups_sorted = true;
  int max_group_size = 0;

  int group_size = 0;

  SEXP rows_i = R_NilValue;

  int k = 0; // Keep track of which row we are in

  if (nrows > 0){

    rows_i = p_group_rows[0];
    group_size = Rf_length(rows_i);
    const int* __restrict__ p_rows_i = INTEGER_RO(rows_i);

    p_group_starts[0] = p_rows_i[0];
    p_group_sizes[0] = group_size;
    p_sorted_group_starts[0] = 1;
    max_group_size = max_group_size < group_size ? group_size : max_group_size;

    if (group_size > 0){
      memcpy(&p_group_order[k], &p_rows_i[0], group_size * sizeof(int));
    }

    for (int j = 0; j < group_size; ++j, ++k){
      p_group_id[p_rows_i[j] - 1] = 1;
    }

    for (int i = 1; i < ngroups; ++i){
      rows_i = p_group_rows[i];
      group_size = Rf_length(rows_i);
      const int* __restrict__ p_rows_i = INTEGER_RO(rows_i);

      p_group_starts[i] = p_rows_i[0];
      p_sorted_group_starts[i] = k + 1;
      p_group_sizes[i] = group_size;
      max_group_size = max_group_size < group_size ? group_size : max_group_size;

      if (group_size != 0){
        memcpy(&p_group_order[k], &p_rows_i[0], group_size * sizeof(int));
      }

      for (int j = 0; j < group_size; ++j, ++k){
        p_group_id[p_rows_i[j] - 1] = i + 1;
        groups_sorted = groups_sorted && p_group_order[k] > p_group_order[k - 1];
      }
    }
  }
  LOGICAL(sorted)[0] = groups_sorted;
  INTEGER(r_max_group_size)[0] = max_group_size;

  Rf_setAttrib(group_order, Rf_installChar(starts_char), sorted_group_starts);
  Rf_setAttrib(group_order, Rf_installChar(maxgrpn_char), r_max_group_size);
  Rf_setAttrib(group_order, Rf_installChar(sorted_char), sorted);
  SET_VECTOR_ELT(out, 0, n_groups);
  SET_VECTOR_ELT(out, 1, group_id);
  SET_VECTOR_ELT(out, 2, group_sizes);
  SET_VECTOR_ELT(out, 3, groups);
  SET_VECTOR_ELT(out, 4, group_vars);
  SET_VECTOR_ELT(out, 5, ordered);
  SET_VECTOR_ELT(out, 6, group_order);
  SET_VECTOR_ELT(out, 7, group_starts);
  SET_VECTOR_ELT(out, 9, group_locs);
  Rf_classgets(out, grp_class);
  Rf_unprotect(NP);
  return out;
}
