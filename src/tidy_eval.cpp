#include "fastplyr.h"
#include "cheapr_api.h"

// Basically R's get()

SEXP cpp_get(SEXP sym, SEXP rho){

  int NP = 0;
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

bool call_is_namespaced(SEXP expr){

  int NP = 0;

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

  int NP = 0;

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

[[cpp11::register]]
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

bool is_call2(SEXP expr, SEXP fn){

  if (TYPEOF(fn) != STRSXP || Rf_length(fn) != 1){
    Rf_error("`fn` must be a character vector of length one in %s", __func__);
  }

  int NP = 0;
  bool out = false;

  if (TYPEOF(fn) != SYMSXP){
    Rf_protect(fn = Rf_coerceVector(fn, SYMSXP)); ++NP;
  }

  if (TYPEOF(expr) == LANGSXP && call_is_namespaced(expr)){
    SEXP call_tree = Rf_protect(as_list_call(expr)); ++NP;
    SEXP fn_expr_tree = Rf_protect(as_list_call(VECTOR_ELT(call_tree, 0))); ++NP;
    out = TYPEOF(VECTOR_ELT(fn_expr_tree, 2)) == SYMSXP && VECTOR_ELT(fn_expr_tree, 2) == fn;
  } else if (TYPEOF(expr) == LANGSXP && TYPEOF(CAR(expr)) == SYMSXP){
    out = CAR(expr) == fn;
  }
  Rf_unprotect(NP);
  return out;
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
    if (TYPEOF(fn) != STRSXP || Rf_length(fn) != 1){
      Rf_error("`fn` must be a character vector of length one in %s", __func__);
    }

    if (TYPEOF(ns) != NILSXP && (TYPEOF(fn) != STRSXP || Rf_length(fn) != 1)){
      Rf_error("`ns` must be `NULL` or a character vector of length one in %s", __func__);
    }

    if (TYPEOF(expr) != LANGSXP){
      return false;
    }

    int NP = 0;

    if (TYPEOF(ns) == NILSXP){
      return is_call2(expr, fn);
    } else {
      SEXP fn_ns;
      if (call_is_namespaced(expr)){
        SEXP call_tree = Rf_protect(as_list_call(expr)); ++NP;
        SEXP fn_expr_tree = Rf_protect(as_list_call(VECTOR_ELT(call_tree, 0))); ++NP;
        fn_ns = Rf_protect(rlang::sym_as_string(VECTOR_ELT(fn_expr_tree, 1))); ++NP;
      } else {
        fn_ns = Rf_protect(cpp_fun_ns(fn, rho)); ++NP;
      }
      bool out = is_call2(expr, fn) && std::strcmp(CHAR(fn_ns), CHAR(STRING_ELT(ns, 0))) == 0;
      Rf_unprotect(NP);
      return out;
    }
}

// checks if call is or contains any calls to a namespace
// it doesn't require the function to actually be called via `::`
[[cpp11::register]]
bool cpp_call_contains_ns(SEXP expr, SEXP ns, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int NP = 0;
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
     SEXP fun_ns = Rf_protect(cpp_fun_ns(branch_name, rho)); ++NP;
     if (std::strcmp(CHAR(fun_ns), CHAR(ns_str)) == 0){
       out = true;
       break;
     }
    }
  }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
bool cpp_call_contains_fn(SEXP expr, SEXP fn, SEXP ns, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }
  int NP = 0;
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
  //       SEXP fn_ns = Rf_protect(cpp_fun_ns(branch_name, rho)); ++NP;
  //       if (branch == fn_sym && std::strcmp(CHAR(fn_ns), CHAR(ns_str)) == 0){
  //         out = true; break;
  //       }
  //     }
  //   }
  // }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
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

[[cpp11::register]]
SEXP cpp_unnest_expr(SEXP expr){
  int NP = 0;
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

[[cpp11::register]]
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
  Rf_protect(out = cheapr::intersect(Rf_getAttrib(data, R_NamesSymbol), out, false));
  Rf_unprotect(4);
  return out;
}

// unname the names of quos with calls to `dplyr::across()`

// SEXP cpp_quos_adjust_across(SEXP quos){
//
//   SEXP names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol));
//
//   if (Rf_isNull(names)){
//     Rf_unprotect(1);
//     Rf_error("`quos` must be a named list of quosures in %s", __func__);
//   }
//
//   SEXP across_str = Rf_protect(Rf_mkString("across"));
//
//   for (int i = 0; i < Rf_length(quos); ++i){
//     if (is_fn_call(rlang::quo_get_expr(VECTOR_ELT(quos, i)), across_str)){
//       SET_STRING_ELT(names, i, R_BlankString);
//     }
//   }
//   Rf_unprotect(2);
//   return quos;
// }

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
  Rf_setAttrib(out, R_NamesSymbol, cheapr::sset_vec(Rf_getAttrib(quos, R_NamesSymbol), not_null_locs, false));
  Rf_classgets(out, Rf_getAttrib(quos, R_ClassSymbol));
  Rf_unprotect(4);
  return out;
}
// cpp11::list cpp_quos_drop_null2(cpp11::list quos){
//
//   cpp11::writable::integers not_null_locs;
//
//   for (int i = 0; i < quos.size(); ++i){
//     if (TYPEOF(rlang::quo_get_expr(quos[i])) != NILSXP){
//       not_null_locs.push_back(i + 1);
//     }
//   }
//   cpp11::list out = cheapr::sset(quos, not_null_locs, false);
//   return out;
// }
SEXP get_mask_top_env(SEXP mask){

  if (TYPEOF(mask) != ENVSXP){
    Rf_error("Object must be a data mask `environment` in %s", __func__);
  }

  SEXP top_env_sym = Rf_protect(Rf_install(".top_env"));
  SEXP top_env = Rf_protect(Rf_findVar(top_env_sym, mask));
  Rf_unprotect(2);
  return top_env;
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

SEXP new_bare_data_mask(){
  SEXP env = Rf_protect(R_NewEnv(R_EmptyEnv, false, 0));
  SEXP mask = Rf_protect(rlang::new_data_mask(env, env));
  SEXP top_env = Rf_protect(get_mask_top_env(mask));

  // Add .data pronoun
  SEXP data_pronoun_sym = Rf_protect(Rf_install(".data"));
  SEXP data_pronoun = Rf_protect(rlang::as_data_pronoun(env));
  Rf_defineVar(data_pronoun_sym, data_pronoun, top_env);

  Rf_unprotect(5);
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
    SET_VECTOR_ELT(rows, 0, cheapr::seq_len(df_nrow(x)));
    set_as_vctrs_new_list_of_int(rows);
    SET_VECTOR_ELT(groups, 0, rows);
    Rf_namesgets(groups, names);
    Rf_protect(groups = cheapr::list_as_df(groups));
    set_as_tbl(groups);
    Rf_unprotect(4);
    return groups;
  } else {
    Rf_error("`x` must be a data frame");
  }
}

[[cpp11::register]]
SEXP cpp_group_keys(SEXP x){
  if (cpp_n_group_vars(x) == 0){
    SEXP r_nrows = Rf_protect(Rf_ScalarInteger(1));
    SEXP empty_list = Rf_protect(Rf_allocVector(VECSXP, 0));
    SEXP out = Rf_protect(cheapr::new_df(empty_list, r_nrows, false, false));
    Rf_unprotect(3);
    return out;
  } else {
    SEXP group_data = Rf_protect(cpp_group_data(x));
    SEXP seq = Rf_protect(cheapr::seq_len(Rf_length(group_data) - 1));
    SEXP groups = Rf_protect(cheapr::df_select(group_data, seq));
    Rf_unprotect(3);
    return groups;
  }
}

[[cpp11::register]]
SEXP cpp_group_vars(SEXP x){
  return Rf_inherits(x, "grouped_df") ? Rf_getAttrib(cpp_group_keys(x), R_NamesSymbol) : Rf_allocVector(STRSXP, 0);
}

[[cpp11::register]]
int cpp_n_group_vars(SEXP x){
  return Rf_length(cpp_group_vars(x));
}

[[cpp11::register]]
SEXP cpp_group_rows(SEXP x){
  SEXP group_data = Rf_protect(cpp_group_data(x));
  SEXP loc = Rf_protect(Rf_ScalarInteger(Rf_length(group_data)));
  SEXP rows = Rf_protect(cheapr::df_select(group_data, loc));
  SEXP out = Rf_protect(VECTOR_ELT(rows, 0));
  Rf_unprotect(4);
  return out;
}

[[cpp11::register]]
SEXP cpp_ungroup(SEXP data){
  if (Rf_inherits(data, "grouped_df")){
    SEXP out = Rf_protect(Rf_shallow_duplicate(data));
    SEXP groups_sym = Rf_protect(Rf_install("groups"));
    Rf_setAttrib(out, groups_sym, R_NilValue);
    SEXP old_class = Rf_getAttrib(out, R_ClassSymbol);
    SEXP grouped_df_char = Rf_protect(Rf_mkString("grouped_df"));
    SEXP new_class = Rf_protect(cheapr::val_remove(old_class, grouped_df_char));
    Rf_setAttrib(out, R_ClassSymbol, new_class);
    Rf_unprotect(4);
    return out;
  }
  return data;
}

// the recycle arg recyles results on a by-group basis
// useful for `reframe()`
//
[[cpp11::register]]
SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos, bool recycle){
  int NP = 0;
  int n_quos = Rf_length(quos);
  bool has_groups = Rf_inherits(data, "grouped_df");
  SEXP group_data = R_NilValue;
  SEXP groups = Rf_protect(cpp_group_keys(data)); ++NP;
  SEXP rows = R_NilValue;
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
  const SEXP *p_quo_name_syms = VECTOR_PTR_RO(quo_name_syms);
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

    // Some C trickery here.. We repeat out the rows of the group keys at the end
    // but if we recycle we only need to do this once and we only need one
    // result sizes vector, otherwise we need one for each expression
    if (recycle){
      R_Reprotect(recycled_sizes = Rf_allocVector(INTSXP, n_groups), recycled_sizes_idx);
      for (int m = 0; m < n_quos; ++m){
        SET_VECTOR_ELT(recycled_sizes_container, m, recycled_sizes);
      }
    } else {
      for (int m = 0; m < n_quos; ++m){
        SET_VECTOR_ELT(recycled_sizes_container, m, Rf_allocVector(INTSXP, n_groups));
      }
    }


  for (int i = 0; i < n_groups; ++i){

    recycled_size = -1; // Initialise this to < 0 for later logic to work

    // Filter on the rows relevant to the current group

    if (has_groups){
      R_Reprotect(chunk_locs = p_rows[i], chunk_locs_idx);
      R_Reprotect(chunk = cheapr::df_slice(data_subset, chunk_locs, false), chunk_idx);
    }

    // assign variables to existing data mask
    // essentially list2env()

    for (int l = 0; l < chunk_n_cols; ++l){
      Rf_defineVar(VECTOR_ELT(quo_data_syms, l), VECTOR_ELT(chunk, l), top_env);
    }

    // inner container will contain the results of our expressions
    // and we assign these inner containers to the bigger outer container
    R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_quos), inner_container_idx);

    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(
        VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
      ), result_idx);
      if (p_quo_name_syms[m] != R_UnboundValue){
        Rf_defineVar(p_quo_name_syms[m], result, top_env);
      }
      SET_VECTOR_ELT(inner_container, m, result);
      result_size = cheapr::vec_length(result);
      recycled_size = recycle ? (result_size == 0 ? 0 : std::max(recycled_size, result_size)) : result_size;
      INTEGER(p_recycled_sizes_container[m])[i] = recycled_size;
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
  SEXP groups_container = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;

  SEXP repeated_groups;
  PROTECT_INDEX repeated_groups_idx;

  if (recycle && n_quos > 0){
    R_ProtectWithIndex(repeated_groups = cheapr::rep(groups, p_recycled_sizes_container[0]), &repeated_groups_idx); ++NP;
  } else {
    R_ProtectWithIndex(repeated_groups = R_NilValue, &repeated_groups_idx); ++NP;
  }
  for (int m = 0; m < n_quos; ++m){
    R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_groups), inner_container_idx);
    for (int j = 0; j < n_groups; ++j){
      SET_VECTOR_ELT(inner_container, j, VECTOR_ELT(p_outer_container[j], m));
    }
    R_Reprotect(result = cheapr::c(inner_container), result_idx);
    if (!recycle){
      R_Reprotect(repeated_groups = cheapr::rep(groups, p_recycled_sizes_container[m]), repeated_groups_idx);
    }
    SET_VECTOR_ELT(groups_container, m, repeated_groups);
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
  int NP = 0;
  int n_quos = Rf_length(quos);
  int n_rows = df_nrow(data);
  int n_groups = 1;
  bool has_groups = Rf_inherits(data, "grouped_df") && n_rows > 0;
  SEXP group_data = R_NilValue;
  SEXP rows = R_NilValue;
  SEXP slice_order = R_NilValue;
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
  SEXP new_col_types = Rf_protect(Rf_allocVector(INTSXP, n_quos)); ++NP;
  SEXP col_is_simple = Rf_protect(Rf_allocVector(LGLSXP, n_quos)); ++NP;
  int *p_new_col_types = INTEGER(new_col_types);
  int *p_col_is_simple = LOGICAL(col_is_simple);
  bool all_simple = true;

  const SEXP *p_rows;

  if (has_groups){
    Rf_protect(group_data = cpp_group_data(data)); ++NP;
    n_groups = df_nrow(group_data);
    // Get group locations and final ordering
    Rf_protect(rows = VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
    p_rows = VECTOR_PTR_RO(rows);
  } else {
    p_rows = VECTOR_PTR_RO(data);
  }

  n_groups = std::max(n_groups, 1);

  // calls_vars_v grabs the variable names the quosures point to
  SEXP quo_data_vars = Rf_protect(cpp_quo_data_vars(quos, data)); ++NP;
  int chunk_n_cols = Rf_length(quo_data_vars);
  SEXP quo_data_syms = Rf_protect(Rf_allocVector(VECSXP, chunk_n_cols)); ++NP;

  for (int i = 0; i < chunk_n_cols; ++i){
    SET_VECTOR_ELT(quo_data_syms, i, Rf_installChar(STRING_ELT(quo_data_vars, i)));
  }

  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
    p_new_col_types[i] = 0;
    p_col_is_simple[i] = true;
  }

  SEXP data_subset = Rf_protect(cheapr::df_select(data, quo_data_vars)); ++NP;

  // Initialise components

  SEXP mask = Rf_protect(new_bare_data_mask()); ++NP;
  SEXP top_env = Rf_protect(get_mask_top_env(mask)); ++NP;

  SEXP chunk_locs, chunk, result, inner_container;

  PROTECT_INDEX chunk_locs_idx, chunk_idx,
  result_idx, inner_container_idx;

  R_ProtectWithIndex(chunk_locs = R_NilValue, &chunk_locs_idx); ++NP;
  R_ProtectWithIndex(chunk = R_NilValue, &chunk_idx); ++NP;
  R_ProtectWithIndex(result = R_NilValue, &result_idx); ++NP;
  R_ProtectWithIndex(inner_container = R_NilValue, &inner_container_idx); ++NP;

  SEXP outer_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;

  int chunk_size;

  for (int i = 0; i < n_groups; ++i){

    if (!has_groups){
      R_Reprotect(chunk = data_subset, chunk_idx);
      chunk_size = n_rows;
    } else {
      R_Reprotect(chunk_locs = p_rows[i], chunk_locs_idx);
      chunk_size = Rf_length(chunk_locs);
      R_Reprotect(chunk = cheapr::df_slice(data_subset, chunk_locs, false), chunk_idx);
    }

    // Assign variables to new mask
    // essentially list2env()
    for (int l = 0; l < chunk_n_cols; ++l){
      Rf_defineVar(VECTOR_ELT(quo_data_syms, l), VECTOR_ELT(chunk, l), top_env);
    }
    R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_quos), inner_container_idx);
    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(
        VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
      ), result_idx);
      R_Reprotect(result = cheapr::rep_len(result, chunk_size), result_idx);
      Rf_defineVar(VECTOR_ELT(quo_name_syms, m), result, top_env);
      SET_VECTOR_ELT(inner_container, m, result);

      p_new_col_types[m] = std::max(p_new_col_types[m], TYPEOF(result));
      p_col_is_simple[m] = p_col_is_simple[m] && !Rf_isObject(result);
      all_simple = all_simple && p_col_is_simple[m];
    }
    SET_VECTOR_ELT(outer_container, i, inner_container);
  }

  // Combine results
  SEXP intermediate_container = R_NilValue;
  SEXP cols_to_add = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  Rf_setAttrib(cols_to_add, R_NamesSymbol, quo_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);

  if (!all_simple){
    intermediate_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
    if (has_groups){
      SEXP unlisted_rows = Rf_protect(cheapr::c(rows)); ++NP;
      Rf_protect(slice_order = Rf_allocVector(INTSXP, n_rows)); ++NP;
      int *p_slice_order = INTEGER(slice_order);

      R_orderVector1(p_slice_order, n_rows, unlisted_rows, TRUE, FALSE);

      // slice_order here is 0-based so we add 1 to make it 1-based
      for (int i = 0; i < n_rows; ++i) ++p_slice_order[i];
    }
  }

  for (int m = 0; m < n_quos; ++m){
    if (p_col_is_simple[m]){
      SET_VECTOR_ELT(cols_to_add, m, Rf_allocVector(p_new_col_types[m], n_rows));
      for (int j = 0; j < n_groups; ++j){

        R_Reprotect(result =
          Rf_coerceVector(
            VECTOR_ELT(p_outer_container[j], m), p_new_col_types[m]
          ), result_idx);

        if (!has_groups){
          SET_VECTOR_ELT(cols_to_add, m, result);
        } else {
          R_Reprotect(chunk_locs = p_rows[j], chunk_locs_idx);
          cheapr::loc_set_replace(VECTOR_ELT(cols_to_add, m), chunk_locs, result);
        }
      }
    } else {
      for (int j = 0; j < n_groups; ++j){
        SET_VECTOR_ELT(intermediate_container, j, VECTOR_ELT(p_outer_container[j], m));
      }
      if (!has_groups){
        R_Reprotect(result = VECTOR_ELT(intermediate_container, 0), result_idx);
      } else {
        R_Reprotect(result = cheapr::c(intermediate_container), result_idx);
        R_Reprotect(result = cheapr::sset(result, slice_order, false), result_idx);
      }
      SET_VECTOR_ELT(cols_to_add, m, result);
    }
  }
  Rf_unprotect(NP);
  return cols_to_add;
}

[[cpp11::register]]
SEXP cpp_group_split(SEXP data, SEXP drop, SEXP order){
  int NP = 0;

  SEXP tbl_class = Rf_protect(Rf_allocVector(STRSXP, 3)); ++NP;
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));

  SEXP group_data = Rf_protect(cpp_group_data(data)); ++NP;
  SEXP names = Rf_protect(Rf_getAttrib(group_data, R_NamesSymbol)); ++NP;
  SEXP seq = Rf_protect(cheapr::seq_len(Rf_length(group_data) - 1)); ++NP;
  SEXP group_vars = Rf_protect(cheapr::sset(names, seq, true)); ++NP;

  Rf_protect(names = Rf_getAttrib(data, R_NamesSymbol)); ++NP;

  SEXP locs, frame;

  PROTECT_INDEX locs_idx, frame_idx;
  R_ProtectWithIndex(locs = R_NilValue, &locs_idx); ++NP;
  R_ProtectWithIndex(frame = R_NilValue, &frame_idx); ++NP;

  SEXP temp_cols = Rf_protect(cheapr::setdiff(names, group_vars)); ++NP;
  SEXP temp = Rf_protect(cheapr::df_select(data, temp_cols)); ++NP;

  SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
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
