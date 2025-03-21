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
  SEXP top_env = Rf_protect(Rf_findVar(top_env_sym, mask));
  // SEXP top_env = Rf_protect(cpp_get(top_env_sym, mask));
  // if (Rf_isNull(top_env)){
  //   Rf_unprotect(2);
  //   Rf_error("`.top_env` cannot be found in data mask");
  // }
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
  // SEXP env = Rf_protect(R_NewEnv(R_EmptyEnv, false, 0));
  // SEXP mask = Rf_protect(rlang::new_data_mask(env, env));
  // SEXP top_env = Rf_protect(get_mask_top_env(mask));
  //
  // // Add .data pronoun
  // SEXP data_pronoun_sym = Rf_protect(Rf_install(".data"));
  // SEXP data_pronoun = Rf_protect(rlang::as_data_pronoun(env));
  // Rf_defineVar(data_pronoun_sym, data_pronoun, top_env);

  SEXP mask = Rf_protect(new_bare_data_mask());

  SEXP out = Rf_protect(cpp_eval_all_tidy(quos, mask));

  if (!keep_null){
    Rf_protect(out = cheapr::drop_null(out, false));
    Rf_unprotect(3); return out;
  } else {
    Rf_unprotect(2); return out;
  }
}

SEXP get_group_data(SEXP x){
  if (Rf_inherits(x, "grouped_df")){
    return Rf_getAttrib(x, Rf_install("groups"));
  } else if (Rf_inherits(x, "data.frame")){
   SEXP groups = Rf_protect(Rf_allocVector(VECSXP, 1));
    SEXP names = Rf_protect(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(names, 0, Rf_mkChar(".rows"));

    // Rows
    SEXP rows = Rf_protect(Rf_allocVector(VECSXP, 1));
    SEXP rows_class = Rf_protect(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(rows_class, 0, Rf_mkChar("vctrs_list_of"));
    SET_STRING_ELT(rows_class, 1, Rf_mkChar("vctrs_vctr"));
    SET_STRING_ELT(rows_class, 2, Rf_mkChar("list"));
    Rf_setAttrib(rows, Rf_install("ptype"), Rf_allocVector(INTSXP, 0));
    Rf_classgets(rows, rows_class);

    SET_VECTOR_ELT(rows, 0, cheapr::seq_len(Rf_length(Rf_getAttrib(x, R_RowNamesSymbol))));
    SET_VECTOR_ELT(groups, 0, rows);
    Rf_setAttrib(groups, R_NamesSymbol, names);
    Rf_protect(groups = cheapr::list_as_df(groups));

    SEXP tbl_class = Rf_protect(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
    SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
    SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));
    Rf_classgets(groups, tbl_class);
    Rf_unprotect(6);
    return groups;
  } else {
    Rf_error("`x` must be a data frame");
  }
}

[[cpp11::register]]
SEXP cpp_grouped_eval_mutate(SEXP data, SEXP quos){
  SEXP group_data = Rf_protect(get_group_data(data));
  int n_quos = Rf_length(quos);
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos));
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos));
  SEXP expr_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos));

  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol));

  SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
  const SEXP *p_rows = VECTOR_PTR_RO(rows);

  int n_groups = Rf_length(Rf_GetRowNames(group_data));

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));

  SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
  const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);

  // Select only essential cols for each expr

  SEXP select_list = Rf_protect(Rf_allocVector(VECSXP, n_quos));
  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(select_list, i, cheapr::df_select(data, p_quov[i]));
    SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(expr_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
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
  R_ProtectWithIndex(top_env = get_mask_top_env(mask), &index5);
  R_ProtectWithIndex(chunk_names = R_NilValue, &index6);


  int k = 0; // Keep track of how many quosures we're looping through
  int NP = 15;
  for (int i = 0; i < n_groups; ++i, ++k){
    if (k == n_quos) k = 0;
    R_Reprotect(chunk_locs = p_rows[i], index1);
    R_Reprotect(chunk = p_select_list[k], index2);
    R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
    R_Reprotect(chunk_names = Rf_getAttrib(chunk, R_NamesSymbol), index6);

    // Assign variables to new mask
    // essentially list2env()
    for (int l = 0; l < Rf_length(chunk); ++l){
      Rf_defineVar(
        Rf_installChar(STRING_ELT(chunk_names, l)),
        VECTOR_ELT(chunk, l), top_env
      );
    }
    R_Reprotect(chunk_names = Rf_allocVector(VECSXP, n_quos), index4);
    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(
        VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
      ), index4);
      R_Reprotect(result = cheapr::rep_len(result, Rf_length(chunk_locs)), index4);
      Rf_defineVar(VECTOR_ELT(expr_syms, m), result, top_env);
    }
    SET_VECTOR_ELT(out, i, result);
  }

  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos, bool as_df, bool check_size){
  SEXP group_data = Rf_protect(get_group_data(data));
  int n_quos = Rf_length(quos);
  SEXP exprs = Rf_protect(Rf_allocVector(VECSXP, n_quos));
  SEXP envs = Rf_protect(Rf_allocVector(VECSXP, n_quos));
  SEXP expr_syms = Rf_protect(Rf_allocVector(VECSXP, n_quos));

  SEXP quo_names = Rf_protect(Rf_getAttrib(quos, R_NamesSymbol));

  SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
  const SEXP *p_rows = VECTOR_PTR_RO(rows);

  int n_groups = Rf_length(Rf_GetRowNames(group_data));

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));

  SEXP quo_data_vars = Rf_protect(cpp11::package("fastplyr")["call_vars_v"](quos, data));
  const SEXP *p_quov = VECTOR_PTR_RO(quo_data_vars);

  // Select only essential cols for each expr

  SEXP select_list = Rf_protect(Rf_allocVector(VECSXP, n_quos));
  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(select_list, i, cheapr::df_select(data, p_quov[i]));
    SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
    SET_VECTOR_ELT(expr_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
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
  R_ProtectWithIndex(top_env = get_mask_top_env(mask), &index5);
  R_ProtectWithIndex(chunk_names = R_NilValue, &index6);


  int k = 0; // Keep track of how many quosures we're looping through
  int NP = 15;
  for (int i = 0; i < n_groups; ++i, ++k){
    if (k == n_quos) k = 0;
    R_Reprotect(chunk_locs = p_rows[i], index1);
    R_Reprotect(chunk = p_select_list[k], index2);
    R_Reprotect(chunk = cheapr::df_slice(chunk, chunk_locs, false), index2);
    R_Reprotect(chunk_names = Rf_getAttrib(chunk, R_NamesSymbol), index6);

    // Assign variables to new mask
    // essentially list2env()
    for (int l = 0; l < Rf_length(chunk); ++l){
      Rf_defineVar(
        Rf_installChar(STRING_ELT(chunk_names, l)),
        VECTOR_ELT(chunk, l), top_env
      );
    }
    R_Reprotect(chunk_names = Rf_allocVector(VECSXP, n_quos), index4);
    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(
        VECTOR_ELT(exprs, m), mask, VECTOR_ELT(envs, m)
      ), index4);
      Rf_defineVar(VECTOR_ELT(expr_syms, m), result, top_env);
    }
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
[[cpp11::register]]
SEXP cpp_grouped_eval_tidy2(SEXP group_data, SEXP data, SEXP quos, bool as_df, bool check_size){
  SEXP rows = Rf_protect(VECTOR_ELT(group_data, Rf_length(group_data) - 1));
  const SEXP *p_rows = VECTOR_PTR_RO(rows);

  int n_groups = Rf_length(Rf_GetRowNames(group_data));
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
//   int n_groups = Rf_length(Rf_GetRowNames(group_data));
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
//   int n_groups = Rf_length(Rf_GetRowNames(group_data));
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

