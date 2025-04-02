#include "fastplyr.h"
#include "cheapr_api.h"

// Basically R's get()

[[cpp11::register]]
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

// Basic version of rlang::is_call(expr, ns = ns)
[[cpp11::register]]
bool cpp_is_call(SEXP expr, SEXP ns){

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
      (std::strcmp(CHAR(rlang::sym_as_string(first)), "::") != 0 &&
      std::strcmp(CHAR(rlang::sym_as_string(first)), ":::") != 0)){
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
  if (cpp_is_call(expr, ns)){
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

bool is_call2(SEXP expr, SEXP fn){

  if (TYPEOF(fn) != STRSXP || Rf_length(fn) != 1){
    Rf_error("`fn` must be a character vector of length one in %s", __func__);
  }

  int NP = 0;
  bool out = false;

  if (TYPEOF(expr) == LANGSXP && TYPEOF(CAR(expr)) == SYMSXP){
    SEXP expr_str = Rf_protect(rlang::sym_as_string(CAR(expr))); ++NP;
    out = expr_str == STRING_ELT(fn, 0);
  }
  Rf_unprotect(NP);
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

[[cpp11::register]]
SEXP cpp_quos_adjust_across(SEXP quos){

  SEXP names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol));

  if (Rf_isNull(names)){
    Rf_unprotect(1);
    Rf_error("`quos` must be a named list of quosures in %s", __func__);
  }

  SEXP across_str = Rf_protect(Rf_mkString("across"));

  for (int i = 0; i < Rf_length(quos); ++i){
    if (is_call2(rlang::quo_get_expr(VECTOR_ELT(quos, i)), across_str)){
      SET_STRING_ELT(names, i, R_BlankString);
    }
  }
  Rf_unprotect(2);
  return quos;
}
// SEXP cpp_quos_adjust_across(SEXP quos){
//
//   SEXP names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol));
//
//   if (Rf_isNull(names)){
//     Rf_unprotect(1);
//     Rf_error("`quos` must be a named list of quosures in %s", __func__);
//   }
//
//   SEXP across_char = Rf_protect(Rf_mkChar("across"));
//
//   SEXP expr, expr_str;
//   PROTECT_INDEX expr_idx, expr_str_idx;
//   R_ProtectWithIndex(expr = R_NilValue, &expr_idx);
//   R_ProtectWithIndex(expr_str = R_NilValue, &expr_str_idx);
//
//   for (int i = 0; i < Rf_length(quos); ++i){
//     R_Reprotect(expr = rlang::quo_get_expr(VECTOR_ELT(quos, i)), expr_idx);
//
//     if (TYPEOF(expr) == LANGSXP && TYPEOF(CAR(expr)) == SYMSXP){
//       R_Reprotect(expr_str = rlang::sym_as_character(CAR(expr)), expr_str_idx);
//       if (STRING_ELT(expr_str, 0) == across_char){
//         SET_STRING_ELT(names, i, R_BlankString);
//       }
//     }
//   }
//   Rf_unprotect(4);
//   return quos;
// }


[[cpp11::register]]
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
SEXP cpp_list_tidy(SEXP quos, bool keep_null){
  SEXP mask = Rf_protect(new_bare_data_mask());
  SEXP out = Rf_protect(cpp_eval_all_tidy(quos, mask));

  if (!keep_null){
    Rf_protect(out = cheapr::drop_null(out, false));
    Rf_unprotect(3); return out;
  } else {
    Rf_unprotect(2); return out;
  }
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
  SEXP group_data = Rf_protect(cpp_group_data(x));
  SEXP seq = Rf_protect(cheapr::seq_len(Rf_length(group_data) - 1));
  SEXP groups = Rf_protect(cheapr::df_select(group_data, seq));
  Rf_unprotect(3);
  return groups;
}

[[cpp11::register]]
SEXP cpp_group_vars(SEXP x){
  return Rf_inherits(x, "grouped_df") ? Rf_getAttrib(cpp_group_keys(x), R_NamesSymbol) : Rf_allocVector(STRSXP, 0);
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

[[cpp11::register]]
SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos){
  int NP = 0;
  int n_quos = Rf_length(quos);
  bool has_groups = Rf_inherits(data, "grouped_df");
  SEXP group_data = R_NilValue;
  SEXP groups = R_NilValue;
  SEXP rows = R_NilValue;
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;

  const SEXP *p_rows;

  int n_groups = 1;

  if (has_groups){
    Rf_protect(group_data = cpp_group_data(data)); ++NP;
    Rf_protect(groups = cpp_group_keys(data)); ++NP;
    n_groups = df_nrow(group_data);
    // Get group locations
    Rf_protect(rows = VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
    p_rows = VECTOR_PTR_RO(rows);
  } else {
    // n_groups = std::min(1, n_rows);
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

  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
  }

  SEXP data_subset = Rf_protect(cheapr::df_select(data, quo_data_vars)); ++NP;

  // Initialise components

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
  // group container will contain the rows of each distinct group
  SEXP outer_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
  SEXP result_sizes = Rf_protect(Rf_allocVector(INTSXP, n_groups)); ++NP;
  int *p_result_sizes = INTEGER(result_sizes);

  for (int i = 0; i < n_groups; ++i){
    if (has_groups){
      R_Reprotect(chunk_locs = p_rows[i], chunk_locs_idx);
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
      Rf_defineVar(VECTOR_ELT(quo_name_syms, m), result, top_env);
      SET_VECTOR_ELT(inner_container, m, result);
    }
    R_Reprotect(inner_container = cheapr::recycle(inner_container, R_NilValue), inner_container_idx);
    p_result_sizes[i] = n_quos == 0 ? 0 : cheapr::vec_length(VECTOR_ELT(inner_container, 0));
    SET_VECTOR_ELT(outer_container, i, inner_container);
  }
  // Combine results
  SEXP intermediate_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
  int n_results = 1 + n_quos;
  SEXP results = Rf_protect(Rf_allocVector(VECSXP, n_results)); ++NP;
  SEXP result_names;
  result_names = Rf_protect(Rf_allocVector(STRSXP, n_results)); ++NP;
  SET_STRING_ELT(result_names, 0, Rf_mkChar("result_sizes"));
  for (int i = 1; i < n_results; ++i){
    SET_STRING_ELT(result_names, i, STRING_ELT(quo_names, i - 1));
  }
  Rf_setAttrib(results, R_NamesSymbol, result_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);
  SET_VECTOR_ELT(results, 0, result_sizes);

  for (int m = 1; m < n_results; ++m){
    for (int j = 0; j < n_groups; ++j){
      SET_VECTOR_ELT(intermediate_container, j, VECTOR_ELT(p_outer_container[j], m - 1));
    }
    R_Reprotect(result = cheapr::c(intermediate_container), result_idx);
    SET_VECTOR_ELT(results, m, result);
  }
  Rf_unprotect(NP);
  return results;
}
// SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos){
//   int NP = 0;
//   int n_quos = Rf_length(quos);
//   SEXP group_data = R_NilValue;
//   SEXP groups = R_NilValue;
//   SEXP rows = R_NilValue;
//   SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
//   SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
//   SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
//   SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
//
//   const SEXP *p_rows;
//
//   Rf_protect(group_data = cpp_group_data(data)); ++NP;
//   Rf_protect(groups = cpp_group_keys(group_data)); ++NP;
//   int n_groups = df_nrow(group_data);
//   // Get group locations
//   Rf_protect(rows = VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
//   p_rows = VECTOR_PTR_RO(rows);
//
//
//   // grab the variable names the expressions point to
//   SEXP quo_data_vars = Rf_protect(cpp_quo_data_vars(quos, data)); ++NP;
//   int chunk_n_cols = Rf_length(quo_data_vars);
//   SEXP quo_data_syms = Rf_protect(Rf_allocVector(VECSXP, chunk_n_cols)); ++NP;
//
//   for (int i = 0; i < chunk_n_cols; ++i){
//     SET_VECTOR_ELT(quo_data_syms, i, Rf_installChar(STRING_ELT(quo_data_vars, i)));
//   }
//
//   for (int i = 0; i < n_quos; ++i){
//     SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
//     SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
//     SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
//   }
//
//   SEXP data_subset = Rf_protect(cheapr::df_select(data, quo_data_vars)); ++NP;
//
//   // Initialise components
//
//   SEXP mask = Rf_protect(new_bare_data_mask()); ++NP;
//   SEXP top_env = Rf_protect(get_mask_top_env(mask)); ++NP;
//
//   SEXP chunk_locs, chunk, result, inner_container;
//
//   PROTECT_INDEX chunk_locs_idx, chunk_idx,
//   result_idx, inner_container_idx;
//
//   R_ProtectWithIndex(chunk_locs = R_NilValue, &chunk_locs_idx); ++NP;
//   R_ProtectWithIndex(chunk = R_NilValue, &chunk_idx); ++NP;
//   R_ProtectWithIndex(result = R_NilValue, &result_idx); ++NP;
//   R_ProtectWithIndex(inner_container = R_NilValue, &inner_container_idx); ++NP;
//
//
//   // outer container will contain lists of expressions for each group
//   // group container will contain the rows of each distinct group
//   SEXP outer_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
//   SEXP result_sizes = Rf_protect(Rf_allocVector(INTSXP, n_groups)); ++NP;
//   int *p_result_sizes = INTEGER(result_sizes);
//
//   for (int i = 0; i < n_groups; ++i){
//     R_Reprotect(chunk_locs = p_rows[i], chunk_locs_idx);
//     R_Reprotect(chunk = cheapr::df_slice(data_subset, chunk_locs, false), chunk_idx);
//
//     // Assign variables to new mask
//     // essentially list2env()
//     for (int l = 0; l < chunk_n_cols; ++l){
//       Rf_defineVar(VECTOR_ELT(quo_data_syms, l), VECTOR_ELT(chunk, l), top_env);
//     }
//     R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_quos), inner_container_idx);
//
//     for (int m = 0; m < n_quos; ++m){
//       R_Reprotect(result = rlang::eval_tidy(
//         VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
//       ), result_idx);
//       // result_size = cheapr::vec_length(result);
//       // final_result_size = result_size == 0 ? 0 : std::max(final_result_size, result_size);
//       Rf_defineVar(VECTOR_ELT(quo_name_syms, m), result, top_env);
//       SET_VECTOR_ELT(inner_container, m, result);
//     }
//     R_Reprotect(inner_container = cheapr::recycle(inner_container, R_NilValue), inner_container_idx);
//     p_result_sizes[i] = n_quos == 0 ? 0 : cheapr::vec_length(VECTOR_ELT(inner_container, 0));
//     SET_VECTOR_ELT(outer_container, i, inner_container);
//   }
//   // Combine results
//   SEXP intermediate_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
//   int n_results = 1 + n_quos;
//   SEXP results = Rf_protect(Rf_allocVector(VECSXP, n_results)); ++NP;
//   SEXP result_names;
//   result_names = Rf_protect(Rf_allocVector(STRSXP, n_results)); ++NP;
//   SET_STRING_ELT(result_names, 0, Rf_mkChar("result_sizes"));
//   for (int i = 1; i < n_results; ++i){
//     SET_STRING_ELT(result_names, i, STRING_ELT(quo_names, i - 1));
//   }
//   Rf_setAttrib(results, R_NamesSymbol, result_names);
//   const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);
//   SET_VECTOR_ELT(results, 0, result_sizes);
//
//   for (int m = 1; m < n_results; ++m){
//     for (int j = 0; j < n_groups; ++j){
//       SET_VECTOR_ELT(intermediate_container, j, VECTOR_ELT(p_outer_container[j], m - 1));
//     }
//     R_Reprotect(result = cheapr::c(intermediate_container), result_idx);
//     SET_VECTOR_ELT(results, m, result);
//   }
//   Rf_unprotect(NP);
//   return results;
// }
// SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos){
//   int NP = 0;
//   int n_quos = Rf_length(quos);
//   SEXP group_data = R_NilValue;
//   SEXP groups = R_NilValue;
//   SEXP rows = R_NilValue;
//   SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
//   SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
//   SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
//   SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
//
//   const SEXP *p_rows;
//
//   Rf_protect(group_data = cpp_group_data(data)); ++NP;
//   Rf_protect(groups = cpp_group_keys(group_data)); ++NP;
//   int n_groups = df_nrow(group_data);
//   // Get group locations
//   Rf_protect(rows = VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
//   p_rows = VECTOR_PTR_RO(rows);
//
//
//   // grab the variable names the expressions point to
//   SEXP quo_data_vars = Rf_protect(cpp_quo_data_vars(quos, data)); ++NP;
//   int chunk_n_cols = Rf_length(quo_data_vars);
//   SEXP quo_data_syms = Rf_protect(Rf_allocVector(VECSXP, chunk_n_cols)); ++NP;
//
//   for (int i = 0; i < chunk_n_cols; ++i){
//     SET_VECTOR_ELT(quo_data_syms, i, Rf_installChar(STRING_ELT(quo_data_vars, i)));
//   }
//
//   for (int i = 0; i < n_quos; ++i){
//     SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
//     SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
//     SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
//   }
//
//   SEXP data_subset = Rf_protect(cheapr::df_select(data, quo_data_vars)); ++NP;
//
//   // Initialise components
//
//   SEXP mask = Rf_protect(new_bare_data_mask()); ++NP;
//   SEXP top_env = Rf_protect(get_mask_top_env(mask)); ++NP;
//
//   SEXP chunk_locs, chunk, result, inner_container;
//
//   PROTECT_INDEX chunk_locs_idx, chunk_idx,
//   result_idx, inner_container_idx;
//
//   R_ProtectWithIndex(chunk_locs = R_NilValue, &chunk_locs_idx); ++NP;
//   R_ProtectWithIndex(chunk = R_NilValue, &chunk_idx); ++NP;
//   R_ProtectWithIndex(result = R_NilValue, &result_idx); ++NP;
//   R_ProtectWithIndex(inner_container = R_NilValue, &inner_container_idx); ++NP;
//
//
//   // outer container will contain lists of expressions for each group
//   // group container will contain the rows of each distinct group
//   SEXP outer_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
//   SEXP group_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
//
//   for (int i = 0; i < n_groups; ++i){
//     R_Reprotect(chunk_locs = p_rows[i], chunk_locs_idx);
//     R_Reprotect(chunk = cheapr::df_slice(data_subset, chunk_locs, false), chunk_idx);
//
//     // Assign variables to new mask
//     // essentially list2env()
//     for (int l = 0; l < chunk_n_cols; ++l){
//       Rf_defineVar(VECTOR_ELT(quo_data_syms, l), VECTOR_ELT(chunk, l), top_env);
//     }
//     R_Reprotect(inner_container = Rf_allocVector(VECSXP, n_quos), inner_container_idx);
//     for (int m = 0; m < n_quos; ++m){
//       R_Reprotect(result = rlang::eval_tidy(
//         VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
//       ), result_idx);
//       Rf_defineVar(VECTOR_ELT(quo_name_syms, m), result, top_env);
//       SET_VECTOR_ELT(inner_container, m, result);
//     }
//     SET_VECTOR_ELT(outer_container, i, inner_container);
//     SET_VECTOR_ELT(group_container, i, cheapr::slice_loc(groups, i));
//   }
//   // Combine results
//   SEXP intermediate_container = Rf_protect(Rf_allocVector(VECSXP, n_groups)); ++NP;
//   int n_results = 1 + n_quos;
//   SEXP results = Rf_protect(Rf_allocVector(VECSXP, n_results)); ++NP;
//   SEXP result_names;
//   result_names = Rf_protect(Rf_allocVector(STRSXP, n_results)); ++NP;
//   SET_STRING_ELT(result_names, 0, Rf_mkChar("groups"));
//   for (int i = 1; i < n_results; ++i){
//     SET_STRING_ELT(result_names, i, STRING_ELT(quo_names, i - 1));
//   }
//   Rf_setAttrib(results, R_NamesSymbol, result_names);
//   const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);
//   SET_VECTOR_ELT(results, 0, cheapr::c(group_container));
//
//   for (int m = 1; m < n_results; ++m){
//     for (int j = 0; j < n_groups; ++j){
//       SET_VECTOR_ELT(intermediate_container, j, VECTOR_ELT(p_outer_container[j], m - 1));
//     }
//     R_Reprotect(result = cheapr::c(intermediate_container), result_idx);
//     SET_VECTOR_ELT(results, m, result);
//   }
//   Rf_unprotect(NP);
//   return results;
// }

[[cpp11::register]]
SEXP cpp_grouped_eval_mutate2(SEXP data, SEXP quos){
  int NP = 0;
  int n_quos = Rf_length(quos);
  int n_rows = df_nrow(data);
  int n_groups = 1;
  bool has_groups = Rf_inherits(data, "grouped_df");
  SEXP group_data = R_NilValue;
  SEXP rows = R_NilValue;
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
    // Get group locations
    Rf_protect(rows = VECTOR_ELT(group_data, Rf_length(group_data) - 1)); ++NP;
    p_rows = VECTOR_PTR_RO(rows);
  } else {
    p_rows = VECTOR_PTR_RO(data);
  }


  // grab the variable names the quosures point to
  SEXP quo_data_vars = Rf_protect(cpp_quo_data_vars(quos, data)); ++NP;
  int chunk_n_cols = Rf_length(quo_data_vars);
  SEXP quo_data_syms = Rf_protect(Rf_allocVector(VECSXP, chunk_n_cols)); ++NP;

  // Pre-install symbols
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
  int k = 0; // Keep track of how many expressions we're looping through

  for (int i = 0; i < n_groups; ++i, ++k){
    if (k == n_quos) k = 0;

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
  SEXP cols_to_add = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  Rf_setAttrib(cols_to_add, R_NamesSymbol, quo_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);
  // const SEXP *p_cols_to_add = VECTOR_PTR_RO(cols_to_add);

  SEXP col_container;
  PROTECT_INDEX col_container_idx;
  R_ProtectWithIndex(col_container = R_NilValue, &col_container_idx); ++NP;


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

      // ALternative method for anything that isn't an unclassed atomic vector
      if (has_groups){
        R_Reprotect(col_container = Rf_allocVector(VECSXP, n_rows), col_container_idx);
        for (int j = 0; j < n_groups; ++j){
          int resulti = 0;
          R_Reprotect(chunk_locs = p_rows[j], chunk_locs_idx);
          int *p_chunk_locs = INTEGER(chunk_locs);
          for (int n = 0; n < chunk_size; ++n, ++resulti){
            SET_VECTOR_ELT(col_container, p_chunk_locs[n] - 1, cheapr::slice_loc(VECTOR_ELT(p_outer_container[j], m), resulti));
          }
          SET_VECTOR_ELT(cols_to_add, m, cheapr::c(col_container));
        }
      } else {
        SET_VECTOR_ELT(cols_to_add, m, VECTOR_ELT(p_outer_container[0], m));
      }
    }
  }
  Rf_unprotect(NP);
  return cols_to_add;
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
  int k = 0; // Keep track of how many expressions we're looping through

  for (int i = 0; i < n_groups; ++i, ++k){
    if (k == n_quos) k = 0;

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

// DO THE SAME AS BELOW BUT instead of combining at the end..
// We can initialise a a list of length n_quos and the quo names
// Then for each group we copy (memcpy) our result vector into
// the variables of this initialised list
// To do that we probably need to use cheapr's cpp_loc_set_replace
// And a new function 'cpp_init' to initialise memory (faster than na_init)

// We can do above approach for when quosures point to a data frame of
// only simple vectors, otherwise we use cheapr::c

// Version that always uses cheapr::sset
[[cpp11::register]]
SEXP cpp_grouped_eval_mutate3(SEXP data, SEXP quos){
  int NP = 0;
  int n_quos = Rf_length(quos);
  int n_rows = df_nrow(data);
  int n_groups = 1;
  bool has_groups = Rf_inherits(data, "grouped_df");
  SEXP group_data = R_NilValue;
  SEXP rows = R_NilValue;
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol)); ++NP;
  SEXP col_containers = Rf_protect(Rf_allocVector(VECSXP, n_quos)); ++NP;
  Rf_namesgets(col_containers, quo_names);
  SEXP col_container = R_NilValue;

  const SEXP *p_col_containers = VECTOR_PTR_RO(col_containers);

  // SEXP new_col_types = Rf_protect(Rf_allocVector(INTSXP, n_quos)); ++NP;
  // SEXP col_is_simple = Rf_protect(Rf_allocVector(LGLSXP, n_quos)); ++NP;

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
    Rf_protect(col_container = Rf_allocVector(VECSXP, n_rows)); ++NP;
    SET_VECTOR_ELT(col_containers, i, col_container);
    // SET_INTEGER_ELT(new_col_types, i, 0);
    // SET_LOGICAL_ELT(col_is_simple, i, true);
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

  int chunk_size;
  int k = 0; // Keep track of how many expressions we're looping through
  int *p_chunk_locs;

  for (int i = 0; i < n_groups; ++i, ++k){
    if (k == n_quos) k = 0;

    if (!has_groups){
      R_Reprotect(chunk = data_subset, chunk_idx);
      chunk_size = n_rows;
    } else {
      R_Reprotect(chunk_locs = p_rows[i], chunk_locs_idx);
      chunk_size = Rf_length(chunk_locs);
      R_Reprotect(chunk = cheapr::df_slice(data_subset, chunk_locs, false), chunk_idx);
      p_chunk_locs = INTEGER(chunk_locs);
    }

    // Assign variables to new mask
    // essentially list2env()
    for (int l = 0; l < chunk_n_cols; ++l){
      Rf_defineVar(VECTOR_ELT(quo_data_syms, l), VECTOR_ELT(chunk, l), top_env);
    }
    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(
        VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
      ), result_idx);
      R_Reprotect(result = cheapr::rep_len(result, chunk_size), result_idx);
      Rf_defineVar(VECTOR_ELT(quo_name_syms, m), result, top_env);

      if (has_groups){
        int resulti = 0;

        for (int n = 0; n < chunk_size; ++n, ++resulti){
          SET_VECTOR_ELT(p_col_containers[m], p_chunk_locs[n] - 1, cheapr::slice_loc(result, resulti));
        }

      } else {
        SET_VECTOR_ELT(col_containers, m, result);
      }
    }
  }

  if (has_groups){
    for (int i = 0; i < n_quos; ++i){
      SET_VECTOR_ELT(col_containers, i, cheapr::c(VECTOR_ELT(col_containers, i)));
    }
  }

  Rf_unprotect(NP);
  return col_containers;
}

// SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos, bool as_df, bool check_size){
//   SEXP group_data = Rf_protect(cpp_group_data(data));
//   int n_quos = Rf_length(quos);
//   SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos));
//   SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos));
//   SEXP expr_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos));
//
//   SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol));
//
//   SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
//   const SEXP *p_rows = VECTOR_PTR_RO(rows);
//
//   int n_groups = df_nrow(group_data);
//
//   SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));
//
//   SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
//   const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);
//
//   // Select only essential cols for each expr
//
//   SEXP select_list = Rf_protect(Rf_allocVector(VECSXP, n_quos));
//   for (int i = 0; i < n_quos; ++i){
//     SET_VECTOR_ELT(select_list, i, cheapr::df_select(data, p_quov[i]));
//     SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
//     SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
//     SET_VECTOR_ELT(expr_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
//   }
//   const SEXP *p_select_list = VECTOR_PTR_RO(select_list);
//
//   // Initialise components
//
//   SEXP chunk_locs, chunk, mask, result, top_env, chunk_names;
//   int chunk_size;
//
//   PROTECT_INDEX index1, index2, index3, index4, index5, index6;
//   R_ProtectWithIndex(chunk_locs = R_NilValue, &index1);
//   R_ProtectWithIndex(chunk = R_NilValue, &index2);
//   R_ProtectWithIndex(mask = new_bare_data_mask(), &index3);
//   R_ProtectWithIndex(result = R_NilValue, &index4);
//   R_ProtectWithIndex(top_env = get_mask_top_env(mask), &index5);
//   R_ProtectWithIndex(chunk_names = R_NilValue, &index6);
//
//
//   int k = 0; // Keep track of how many quosures we're looping through
//   int NP = 15;
//   for (int i = 0; i < n_groups; ++i, ++k){
//     if (k == n_quos) k = 0;
//     R_Reprotect(chunk_locs = p_rows[i], index1);
//     R_Reprotect(chunk = p_select_list[k], index2);
//     R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
//     R_Reprotect(chunk_names = Rf_getAttrib(chunk, R_NamesSymbol), index6);
//
//     // Assign variables to new mask
//     // essentially list2env()
//     for (int l = 0; l < Rf_length(chunk); ++l){
//       Rf_defineVar(
//         Rf_installChar(STRING_ELT(chunk_names, l)),
//         VECTOR_ELT(chunk, l), top_env
//       );
//     }
//     R_Reprotect(chunk_names = Rf_allocVector(VECSXP, n_quos), index4);
//     for (int m = 0; m < n_quos; ++m){
//       R_Reprotect(result = rlang::eval_tidy(
//         VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
//       ), index4);
//       Rf_defineVar(VECTOR_ELT(expr_syms, m), result, top_env);
//     }
//     if (check_size){
//       chunk_size = Rf_length(chunk_locs);
//       for (int j = 0; j < n_quos; ++j){
//         if (cheapr::vec_length(VECTOR_ELT(result, j)) != chunk_size){
//           Rf_unprotect(NP);
//           Rf_error("In group %d, result must be length %d", i + 1, chunk_size);
//         }
//       }
//     }
//     if (as_df){
//       R_Reprotect(result = cheapr::recycle(result, R_NilValue), index4);
//       R_Reprotect(result = cheapr::list_as_df(result), index4);
//     }
//     SET_VECTOR_ELT(out, i, result);
//   }
//
//   Rf_unprotect(NP);
//   return out;
// }
[[cpp11::register]]
SEXP cpp_grouped_eval_tidy2(SEXP group_data, SEXP data, SEXP quos, bool as_df, bool check_size){
  SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
  const SEXP *p_rows = VECTOR_PTR_RO(rows);

  int n_groups = df_nrow(group_data);
  int n_quos = Rf_length(quos);

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));

  SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
  const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);

  // Select only essential cols for each expr

  SEXP select_list = Rf_protect(Rf_allocVector(VECSXP, n_quos));
  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(select_list, i, cheapr::df_select(data, p_quov[i]));
  }
  const SEXP *p_select_list = VECTOR_PTR_RO(select_list);

  // Initialise components

  SEXP chunk_locs, chunk, mask, result, top_env, chunk_names;
  int chunk_size;

  PROTECT_INDEX index1, index2, index3, index4, index5, index6;
  R_ProtectWithIndex(chunk_locs = R_NilValue, &index1);
  R_ProtectWithIndex(chunk = R_NilValue, &index2);
  R_ProtectWithIndex(mask = new_bare_data_mask(), &index3);
  R_ProtectWithIndex(result = R_NilValue, &index4);
  // R_ProtectWithIndex(top_env = R_NilValue, &index5);
  R_ProtectWithIndex(top_env = get_mask_top_env(mask), &index5);
  R_ProtectWithIndex(chunk_names = R_NilValue, &index6);


  int k = 0; // Keep track of how many quosures we're looping through
  int NP = 10;
  for (int i = 0; i < n_groups; ++i, ++k){
    if (k == n_quos) k = 0;
    R_Reprotect(chunk_locs = p_rows[i], index1);
    R_Reprotect(chunk = p_select_list[k], index2);
    R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
    R_Reprotect(chunk_names = Rf_getAttrib(chunk, R_NamesSymbol), index6);
    // R_Reprotect(mask = new_bare_data_mask(), index3);
    // R_Reprotect(top_env = get_mask_top_env(mask), index5);

    // Assign variables to new mask
    for (int l = 0; l < Rf_length(chunk); ++l){
      Rf_defineVar(
        Rf_installChar(STRING_ELT(chunk_names, l)),
        VECTOR_ELT(chunk, l), top_env
      );
    }
    R_Reprotect(result = cpp_eval_all_tidy(quos, mask), index4);
    if (check_size){
      chunk_size = Rf_length(chunk_locs);
      for (int j = 0; j < n_quos; ++j){
        if (cheapr::vec_length(VECTOR_ELT(result, j)) != chunk_size){
          Rf_unprotect(NP);
          Rf_error("In group %d, result must be length %d", i + 1, chunk_size);
        }
      }
    }
    if (as_df){
      R_Reprotect(result = cheapr::recycle(result, R_NilValue), index4);
      R_Reprotect(result = cheapr::list_as_df(result), index4);
    }
    SET_VECTOR_ELT(out, i, result);
  }

  Rf_unprotect(NP);
  return out;
}
// SEXP cpp_grouped_eval_tidy(SEXP group_data, SEXP data, SEXP quos, bool as_df, bool check_size){
//   SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
//   const SEXP *p_rows = VECTOR_PTR_RO(rows);
//
//   int n_groups = df_nrow(group_data);
//   int n_quos = Rf_length(quos);
//
//   SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));
//
//   SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
//   const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);
//
//   // Select only essential cols for each expr
//
//   SEXP select_list = Rf_protect(Rf_allocVector(VECSXP, n_quos));
//   for (int i = 0; i < n_quos; ++i){
//     SET_VECTOR_ELT(select_list, i, cheapr::df_select(data, p_quov[i]));
//   }
//   const SEXP *p_select_list = VECTOR_PTR_RO(select_list);
//
//   // Initialise components
//
//   SEXP chunk_locs, chunk, mask, result;
//   int chunk_size;
//
//   PROTECT_INDEX index1, index2, index3, index4;
//   R_ProtectWithIndex(chunk_locs = R_NilValue, &index1);
//   R_ProtectWithIndex(chunk = R_NilValue, &index2);
//   R_ProtectWithIndex(mask = R_NilValue, &index3);
//   R_ProtectWithIndex(result = R_NilValue, &index4);
//
//   int k = 0; // Keep track of how many quosures we're looping through
//   int NP = 8;
//   for (int i = 0; i < n_groups; ++i, ++k){
//     if (k == n_quos) k = 0;
//     R_Reprotect(chunk_locs = p_rows[i], index1);
//     R_Reprotect(chunk = p_select_list[k], index2);
//     R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
//     R_Reprotect(mask = rlang::as_data_mask(chunk), index3);
//     R_Reprotect(result = cpp_eval_all_tidy(quos, mask), index4);
//     chunk_size = Rf_length(chunk_locs);
//     if (check_size){
//       for (int j = 0; j < n_quos; ++j){
//         if (cheapr::vec_length(VECTOR_ELT(result, j)) != chunk_size){
//           Rf_unprotect(NP);
//           Rf_error("In group %d, result must be length %d", i + 1, chunk_size);
//         }
//       }
//     }
//     if (as_df){
//       R_Reprotect(result = cheapr::recycle(result, Rf_ScalarInteger(chunk_size)), index4);
//       R_Reprotect(result = cheapr::list_as_df(result), index4);
//     }
//     SET_VECTOR_ELT(out, i, result);
//   }
//
//   Rf_unprotect(NP);
//   return out;
// }
// SEXP cpp_grouped_eval_tidy(SEXP group_data, SEXP data, SEXP quos, bool as_df, bool check_size){
//   SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
//   const SEXP *p_rows = VECTOR_PTR_RO(rows);
//
//   int n_groups = df_nrow(group_data);
//   int n_quos = Rf_length(quos);
//
//   SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));
//
//   SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
//   const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);
//
//   // Initialise components
//
//   // SEXP tbl_class = Rf_protect(Rf_allocVector(STRSXP, 3));
//   // SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
//   // SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
//   // SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));
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
//   int NP = 7;
//   for (int i = 0; i < n_groups; ++i, ++k){
//     if (k == n_quos) k = 0;
//     R_Reprotect(chunk_locs = p_rows[i], index1);
//     R_Reprotect(chunk = cheapr::df_select(data, p_quov[k]), index2);
//     R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
//     R_Reprotect(mask = rlang::as_data_mask(chunk), index3);
//     R_Reprotect(result = cpp_eval_all_tidy(quos, mask), index4);
//     if (check_size){
//       int chunk_size = Rf_length(Rf_getAttrib(chunk, R_RowNamesSymbol));
//       for (int j = 0; j < n_quos; ++j){
//         if (cheapr::vec_length(VECTOR_ELT(result, j)) != chunk_size){
//           Rf_unprotect(NP);
//           Rf_error("In group %d, result must be length %d", i + 1, chunk_size);
//         }
//       }
//     }
//     if (as_df){
//       // R_Reprotect(result = cheapr::recycle(result, R_NilValue), index4);
//       R_Reprotect(result = cheapr::list_as_df(result), index4);
//       // Rf_classgets(result, tbl_class);
//     }
//     SET_VECTOR_ELT(out, i, result);
//   }
//
//   Rf_unprotect(NP);
//   return out;
// }
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
