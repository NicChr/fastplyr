#include "fastplyr.h"
#include "cheapr_api.h"

static SEXP top_env_sym = NULL;
static SEXP data_pronoun_sym = NULL;

[[cpp11::init]]
void init_mask_symbols(DllInfo* dll){

  // Symbols get added to internal R protected list so no need to preserve

  top_env_sym = Rf_installChar(Rf_mkCharCE(".top_env", CE_UTF8));
  data_pronoun_sym = Rf_installChar(Rf_mkCharCE(".data", CE_UTF8));
}

bool is_data_pronoun_call(SEXP expr, SEXP env){

  int32_t NP = 0;

  if (Rf_length(expr) != 3){
    YIELD(NP);
    return false;
  }

  SEXP dollar_str = SHIELD(Rf_ScalarString(Rf_mkCharCE("$", CE_UTF8))); ++NP;
  SEXP double_brackets_str = SHIELD(Rf_ScalarString(Rf_mkCharCE("[[", CE_UTF8))); ++NP;

  if (!(is_fn_call(expr, dollar_str, R_NilValue, env) ||
      is_fn_call(expr, double_brackets_str, R_NilValue, env))){
      YIELD(NP);
    return false;
  }

  bool out = CAR(CDR(expr)) == data_pronoun_sym;

  YIELD(NP);
  return out;
}

// Get the var of a .data call
SEXP data_pronoun_var(SEXP expr, SEXP env){

  int32_t NP = 0;

  if (!is_data_pronoun_call(expr, env)){
    YIELD(NP);
    Rf_error("`expr` must be a `.data` pronoun expression");
  }

  SEXP dollar_sym = SHIELD(Rf_installChar(Rf_mkCharCE("$", CE_UTF8))); ++NP;

  SEXP vars = CAR(CDDR(expr));
  SEXP out_var = SHIELD(new_vec(STRSXP, 0)); ++NP;

  if (CAR(expr) == dollar_sym){
    if (TYPEOF(vars) == SYMSXP){
      SHIELD(vars = rlang::sym_as_character(vars)); ++NP;
    } else if (TYPEOF(vars) == STRSXP){
      if (Rf_length(vars) != 1){
        YIELD(NP);
        Rf_error("A string or symbol must be supplied to `.data$`");
      }
    }
  } else {
    SHIELD(vars = rlang::eval_tidy(vars, R_NilValue, env)); ++NP;
    if (TYPEOF(vars) != STRSXP &&
        TYPEOF(vars) != SYMSXP){
      YIELD(NP);
      Rf_error("A string or symbol must be supplied to `.data[[`");
    }
    if (Rf_length(vars) != 1){
      YIELD(NP);
      Rf_error("A string or symbol must be supplied to `.data[[`");
    }
    if (TYPEOF(vars) == SYMSXP){
      SHIELD(vars = rlang::sym_as_character(vars)); ++NP;
    }
  }

  SHIELD(out_var = vars); ++NP;

  YIELD(NP);
  return out_var;
}

cpp11::writable::strings all_call_names(cpp11::data_frame data, cpp11::sexp expr, cpp11::environment env){

  using namespace cpp11;
  writable::strings out;
  strings temp;

  if (is_data_pronoun_call(expr, env)){
    temp = data_pronoun_var(expr, env);
    out.push_back(temp[0]);
  } else if (TYPEOF(expr) == SYMSXP){
    out.push_back(rlang::sym_as_string(expr));
  }else if (TYPEOF(expr) == LANGSXP){
    list tree = call_args(expr);
    for (int i = 0; i < tree.size(); ++i){
      sexp branch = tree[i];
      temp = all_call_names(data, branch, env);
      for (int j = 0; j < temp.size(); ++j){
        out.push_back(temp[j]);
      }
    }
  }
  return out;
}

// Which variables are quosures pointing to?

[[cpp11::register]]
SEXP quo_vars(SEXP quos, SEXP data, bool combine){

  int n_quos = Rf_length(quos);

  SEXP quo_vars = SHIELD(new_vec(VECSXP, n_quos));
  SEXP quo_names = SHIELD(get_names(quos));
  set_names(quo_vars, quo_names);
  SEXP names = SHIELD(get_names(data));

  SEXP expr, env;
  PROTECT_INDEX expr_idx, env_idx;
  R_ProtectWithIndex(expr = R_NilValue, &expr_idx);
  R_ProtectWithIndex(env = R_NilValue, &env_idx);

  for (int i = 0; i < n_quos; ++i){
    R_Reprotect(expr = rlang::quo_get_expr(VECTOR_ELT(quos, i)), expr_idx);
    R_Reprotect(env = rlang::quo_get_env(VECTOR_ELT(quos, i)), env_idx);
    SET_VECTOR_ELT(quo_vars, i, all_call_names(data, expr, env));
    SET_VECTOR_ELT(quo_vars, i, cheapr::intersect(names, VECTOR_ELT(quo_vars, i), false));
  }
  if (combine){
    SEXP out = SHIELD(cheapr::c(quo_vars));
    SHIELD(out = cheapr::intersect(names, out, false));
    YIELD(7);
    return out;
  } else {
    YIELD(5);
    return quo_vars;
  }
}


[[cpp11::register]]
SEXP cpp_quos_drop_null(SEXP quos){

  int n = Rf_length(quos);

  SEXP not_null = SHIELD(new_vec(LGLSXP, n));
  int *p_not_null = INTEGER(not_null);
  const SEXP *p_quos = VECTOR_PTR_RO(quos);
  int n_null = 0;

  for (int i = 0; i < n; ++i){
    p_not_null[i] = TYPEOF(rlang::quo_get_expr(p_quos[i])) != NILSXP;
    n_null += !p_not_null[i];
  }
  if (n_null == 0){
    YIELD(1);
    return quos;
  }
  SEXP r_true = SHIELD(new_vec(LGLSXP, 1));
  LOGICAL(r_true)[0] = TRUE;
  SEXP not_null_locs = SHIELD(cheapr::val_find(not_null, r_true, false));
  SEXP out = SHIELD(cheapr::sset_vec(quos, not_null_locs, false));
  Rf_copyMostAttrib(quos, out);
  SEXP names = SHIELD(get_names(quos));
  set_names(out, cheapr::sset_vec(names, not_null_locs, false));
  SEXP cls = SHIELD(Rf_getAttrib(quos, R_ClassSymbol));
  Rf_classgets(out, cls);
  YIELD(6);
  return out;
}

bool call_contains_dplyr_mask(SEXP expr, SEXP rho){
  if (TYPEOF(expr) != LANGSXP){
    return false;
  }

  int32_t NP = 0;

  SEXP dplyr_mask_fns = SHIELD(new_vec(STRSXP, 11)); ++NP;
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
  SEXP dplyr_str = SHIELD(Rf_mkString("dplyr")); ++NP;

  if (is_fn_call(expr, dplyr_mask_fns, dplyr_str, rho)){
    YIELD(NP);
    return true;
  }

  bool out = false;

  SEXP tree = SHIELD(as_list_call(expr)); ++NP;
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
      SEXP branch_name = SHIELD(rlang::sym_as_character(branch)); ++NP;
      if (is_fn_call(branch_name, dplyr_mask_fns, dplyr_str, rho)){
        out = true;
        break;
      }
    }
  }
  YIELD(NP);
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
  YIELD(2);
  return out;
}

SEXP get_mask_top_env(SEXP mask){
  return Rf_findVarInFrame(mask, top_env_sym);
}

SEXP new_bare_data_mask(){
  SEXP env = SHIELD(R_NewEnv(R_EmptyEnv, false, 0));
  SEXP mask = SHIELD(rlang::new_data_mask(env, env));
  SEXP top_env = SHIELD(get_mask_top_env(mask));

  // Add .data pronoun
  SEXP data_pronoun = SHIELD(rlang::as_data_pronoun(env));
  Rf_defineVar(data_pronoun_sym, data_pronoun, top_env);

  YIELD(4);
  return mask;
}

void reset_mask_top_env(SEXP mask, int env_size){
  Rf_defineVar(top_env_sym, mask, R_NewEnv(R_EmptyEnv, false, env_size));
}

// Just a wrapper around rlang::eval_tidy
// but only supplying a quosure and data mask

// This is slow on very large numbers of repeated calls
// in which case it is faster to save the exprs/envs and
// call rlang::eval_tidy directly on the exprs/envs
SEXP eval_tidy(SEXP quo, SEXP mask){
  return rlang::eval_tidy(quo, mask, R_NilValue);
}

// Eval a list of quos

[[cpp11::register]]
SEXP cpp_eval_all_tidy(SEXP quos, SEXP mask){
  int32_t NP = 0;
  int n_exprs = Rf_length(quos);
  SEXP expr_names = SHIELD(get_names(quos)); ++NP;

  if (TYPEOF(expr_names) == NILSXP){
    SHIELD(expr_names = new_vec(STRSXP, n_exprs)); ++NP;
  }
  SEXP top_env = SHIELD(get_mask_top_env(mask));++NP;

  SEXP out = SHIELD(new_vec(VECSXP, n_exprs)); ++NP;
  SEXP out_names = SHIELD(new_vec(STRSXP, n_exprs)); ++NP;

  const SEXP *p_quos = VECTOR_PTR_RO(quos);
  const SEXP *p_expr_names = STRING_PTR_RO(expr_names);

  for (int i = 0; i < n_exprs; ++i){
    SEXP result = SHIELD(eval_tidy(p_quos[i], mask)); ++NP;
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
  set_names(out, out_names);
  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_list_tidy(SEXP quos){
  SEXP mask = SHIELD(new_bare_data_mask());
  SEXP out = SHIELD(cpp_eval_all_tidy(quos, mask));
  YIELD(2); return out;
}

void set_as_tbl(SEXP x){
  SEXP tbl_class = SHIELD(new_vec(STRSXP, 3));
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));
  Rf_classgets(x, tbl_class);
  YIELD(1);
}

void set_as_vctrs_new_list_of_int(SEXP x){
  if (TYPEOF(x) != VECSXP){
    Rf_error("`x` must be a list of integers in %s", __func__);
  }
  SEXP rows_class = SHIELD(new_vec(STRSXP, 3));
  SET_STRING_ELT(rows_class, 0, Rf_mkChar("vctrs_list_of"));
  SET_STRING_ELT(rows_class, 1, Rf_mkChar("vctrs_vctr"));
  SET_STRING_ELT(rows_class, 2, Rf_mkChar("list"));
  Rf_setAttrib(x, Rf_install("ptype"), new_vec(INTSXP, 0));
  Rf_classgets(x, rows_class);
  YIELD(1);
}

SEXP compact_int_seq_len(int n){
  if (n == NA_INTEGER || n < 0){
    Rf_error("`n` must be >= 0");
  }
  if (n == 0){
    return new_vec(INTSXP, 0);
  }
  SEXP start = SHIELD(Rf_ScalarInteger(1));
  SEXP end = SHIELD(Rf_ScalarInteger(n));
  SEXP expr = SHIELD(Rf_lang3(Rf_install(":"), start, end));
  SEXP out = SHIELD(Rf_eval(expr, R_BaseEnv));
  YIELD(4);
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

    SEXP out = SHIELD(new_vec(INTSXP, out_size));
    int* __restrict__ p_out = INTEGER(out);

    for (int i = 0; i < n; k += m, ++i){
      m = Rf_length(p_x[i]);
      safe_memcpy(&p_out[k], &loc_ptrs[i][0], m * sizeof(int));
    }
    YIELD(1);
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

    SEXP out = SHIELD(new_vec(INTSXP, out_size));
    int* __restrict__ p_out = INTEGER(out);

    for (int i = 0; i < n; k += m, ++i){
      m = p_gs[i];
      safe_memcpy(&p_out[k], &loc_ptrs[i][0], m * sizeof(int));
    }
    YIELD(1);
    return out;
  }
}

// Are group IDs sorted?
// This function expects no NAs
[[cpp11::register]]
bool cpp_group_id_sorted(SEXP x){
  int n = Rf_length(x);
  const int* __restrict__ p_x = INTEGER_RO(x);
  for (int i = 1; i < n; ++i){
    if (p_x[i] < p_x[i - 1]){
      return false;
    }
  }
  return true;
}


// Inspired by purrr::transpose
// Converts a list of n groups x m results list into an
// m results x n groups list

[[cpp11::register]]
SEXP transpose_eval_results(SEXP x) {

  int32_t NP = 0;

  if (TYPEOF(x) != VECSXP) {
    Rf_error("`x` must be a list in %s", __func__);
  }

  const SEXP *p_x = VECTOR_PTR_RO(x);

  int n = Rf_length(x);
  if (n == 0) {
    return new_vec(VECSXP, 0);
  }

  SEXP x1 = VECTOR_ELT(x, 0);
  int m = Rf_length(x1);

  // Create space for output
  SEXP out = SHIELD(new_vec(VECSXP, m)); ++NP;
  SEXP names1 = SHIELD(get_names(x)); ++NP;

  const SEXP *p_out = VECTOR_PTR_RO(out);

  for (int j = 0; j < m; ++j) {
    SEXP xj = SHIELD(new_vec(VECSXP, n));
    if (!Rf_isNull(names1)) {
      set_names(xj, names1);
    }
    SET_VECTOR_ELT(out, j, xj);
    YIELD(1);
  }

  SEXP names2 = SHIELD(get_names(x1)); ++NP;

  if (!Rf_isNull(names2)) {
    set_names(out, names2);
  }

  PROTECT_INDEX index_idx;
  SEXP index;
  R_ProtectWithIndex(index = R_NilValue, &index_idx); ++NP;

  // Fill output
  for (int i = 0; i < n; ++i) {
    SEXP xi = p_x[i];

    const SEXP *p_xi = VECTOR_PTR_RO(xi);

    // find mapping between names and index. Use -1 to indicate not found
    SEXP names_i = get_names(xi);
    SEXP index;

    int* __restrict__ p_index;

    if (!Rf_isNull(names2) && !Rf_isNull(names_i)) {
      R_Reprotect(index = Rf_match(names_i, names2, 0), index_idx);
      p_index = INTEGER(index);
    } else {
      R_Reprotect(index = new_vec(INTSXP, m), index_idx);
      p_index = INTEGER(index);

      int mi = Rf_length(xi);

      if (m != mi) {
        YIELD(NP);
        Rf_error("Element %d must be length %d, not %d", i + 1, m, mi);
      }
      for (int i = 0; i < m; ++i) {
        p_index[i] = (i < mi) ? i + 1 : 0;
      }

    }

    for (int j = 0; j < m; ++j) {
      int pos = p_index[j] - 1;
      if (pos == -1) continue;
      SET_VECTOR_ELT(p_out[j], i, p_xi[pos]);
    }
  }

  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP recycle_eval_results(SEXP x){
  int32_t NP = 0;

  if (TYPEOF(x) != VECSXP) {
    Rf_error("`x` must be a list in %s", __func__);
  }

  const SEXP *p_x = VECTOR_PTR_RO(x);

  int n = Rf_length(x);
  if (n == 0) {
    return new_vec(VECSXP, 0);
  }

  SEXP out = SHIELD(new_vec(VECSXP, n)); ++NP;

  for (int i = 0; i < n; ++i){
    SET_VECTOR_ELT(out, i, cheapr::recycle(p_x[i], R_NilValue));
  }
  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos, bool recycle, bool add_groups){

  int32_t NP = 0;

  int n_quos = Rf_length(quos);

  if (n_quos == 0){
    SEXP out = SHIELD(new_vec(VECSXP, 2));
    SET_VECTOR_ELT(out, 0, new_vec(VECSXP, 0));
    SET_VECTOR_ELT(out, 1, new_vec(VECSXP, 0));
    set_names(VECTOR_ELT(out, 0), new_vec(STRSXP, 0));
    set_names(VECTOR_ELT(out, 1), new_vec(STRSXP, 0));
    SEXP out_names = SHIELD(new_vec(STRSXP, 2));
    SET_STRING_ELT(out_names, 0, Rf_mkChar("groups"));
    SET_STRING_ELT(out_names, 1, Rf_mkChar("results"));
    set_names(out, out_names);
    YIELD(2);
    return out;
  }

  bool has_groups = Rf_inherits(data, "grouped_df");
  SEXP groups = SHIELD(cpp_group_keys(data)); ++NP;
  SEXP exprs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP envs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP quo_names = SHIELD(get_names(quos)); ++NP;
  const SEXP *p_quo_name_syms = VECTOR_PTR_RO(quo_name_syms);
  const SEXP *p_exprs = VECTOR_PTR_RO(exprs);
  const SEXP *p_envs = VECTOR_PTR_RO(envs);

  // Get group locations
  SEXP rows = SHIELD(cpp_group_rows(data)); ++NP;
  int n_groups = std::max(Rf_length(rows), 1);
  const SEXP *p_rows = VECTOR_PTR_RO(rows);


  // grab the variable names the expressions point to
  SEXP quo_data_vars = SHIELD(quo_vars(quos, data, true)); ++NP;
  int chunk_n_cols = Rf_length(quo_data_vars);
  SEXP quo_data_syms = SHIELD(new_vec(VECSXP, chunk_n_cols)); ++NP;
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

  SEXP data_subset = SHIELD(cheapr::df_select(data, quo_data_vars)); ++NP;

  // We will re-use the mask across all groups but add the same
  // vars for each slice in each iteration
  SEXP mask = SHIELD(new_bare_data_mask()); ++NP;
  SEXP top_env = SHIELD(get_mask_top_env(mask)); ++NP;

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

  SEXP outer_container = SHIELD(new_vec(VECSXP, n_groups)); ++NP;

  // Recycling in this context means to recycle the results to a common size
  // on a by-group basis, meaning the recycled sizes may differ between different
  // groups

  int recycled_size;
  int result_size;

  SEXP recycled_sizes;
  PROTECT_INDEX recycled_sizes_idx;
  R_ProtectWithIndex(recycled_sizes = R_NilValue, &recycled_sizes_idx); ++NP;

  SEXP recycled_sizes_container = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  const SEXP *p_recycled_sizes_container = VECTOR_PTR_RO(recycled_sizes_container);
  std::vector<int *> recycled_pointers(n_quos);

  // Some C trickery here.. We repeat out the rows of the group keys at the end
  // but if we recycle we only need to do this once and we only need one
  // result sizes vector, otherwise we need one for each expression
  if (recycle && n_quos > 0){
    R_Reprotect(recycled_sizes = new_vec(INTSXP, n_groups), recycled_sizes_idx);
    recycled_pointers[0] = INTEGER(recycled_sizes);
    for (int m = 0; m < n_quos; ++m){
      SET_VECTOR_ELT(recycled_sizes_container, m, recycled_sizes);
      recycled_pointers[m] = recycled_pointers[0];
    }
  } else {
    for (int m = 0; m < n_quos; ++m){
      SET_VECTOR_ELT(recycled_sizes_container, m, new_vec(INTSXP, n_groups));
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
    R_Reprotect(inner_container = new_vec(VECSXP, n_quos), inner_container_idx);

    for (int m = 0; m < n_quos; ++m){

      // Evaluate the expression
      R_Reprotect(result = rlang::eval_tidy(
        p_exprs[m], mask, p_envs[m]
      ), result_idx);

      // Only add result to mask if expr is named
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

  SEXP results = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  set_names(results, quo_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);


  // groups container will hold the repeated out rows of the group keys
  SEXP groups_container = R_NilValue;
  if (add_groups){
    SHIELD(groups_container = new_vec(VECSXP, n_quos)); ++NP;
    set_names(groups_container, quo_names);
  }

  SEXP repeated_groups;
  PROTECT_INDEX repeated_groups_idx;
  R_ProtectWithIndex(repeated_groups = R_NilValue, &repeated_groups_idx); ++NP;

  if (add_groups && recycle && n_quos > 0){
    R_Reprotect(repeated_groups = cheapr::rep(groups, p_recycled_sizes_container[0]), repeated_groups_idx);
  }
  for (int m = 0; m < n_quos; ++m){
    R_Reprotect(inner_container = new_vec(VECSXP, n_groups), inner_container_idx);
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
  SEXP out = SHIELD(new_vec(VECSXP, 2)); ++NP;
  SET_VECTOR_ELT(out, 0, groups_container);
  SET_VECTOR_ELT(out, 1, results);
  SEXP out_names = SHIELD(new_vec(STRSXP, 2)); ++NP;
  SET_STRING_ELT(out_names, 0, Rf_mkChar("groups"));
  SET_STRING_ELT(out_names, 1, Rf_mkChar("results"));
  set_names(out, out_names);
  YIELD(NP);
  return out;
}

// SEXP cpp_grouped_eval_tidy(SEXP data, SEXP quos, bool recycle, bool add_groups){
//
//   int32_t NP = 0;
//
//   int n_quos = Rf_length(quos);
//
//   if (n_quos == 0){
//     SEXP out = SHIELD(new_vec(VECSXP, 2));
//     SET_VECTOR_ELT(out, 0, new_vec(VECSXP, 0));
//     SET_VECTOR_ELT(out, 1, new_vec(VECSXP, 0));
//     set_names(VECTOR_ELT(out, 0), new_vec(STRSXP, 0));
//     set_names(VECTOR_ELT(out, 1), new_vec(STRSXP, 0));
//     SEXP out_names = SHIELD(new_vec(STRSXP, 2));
//     SET_STRING_ELT(out_names, 0, Rf_mkChar("groups"));
//     SET_STRING_ELT(out_names, 1, Rf_mkChar("results"));
//     set_names(out, out_names);
//     YIELD(2);
//     return out;
//   }
//
//   bool has_groups = Rf_inherits(data, "grouped_df");
//   SEXP groups = SHIELD(cpp_group_keys(data)); ++NP;
//   SEXP exprs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
//   SEXP envs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
//   SEXP quo_name_syms = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
//   SEXP quo_names = SHIELD(get_names(quos)); ++NP;
//   const SEXP *p_quo_name_syms = VECTOR_PTR_RO(quo_name_syms);
//   const SEXP *p_exprs = VECTOR_PTR_RO(exprs);
//   const SEXP *p_envs = VECTOR_PTR_RO(envs);
//
//   // Get group locations
//   SEXP rows = SHIELD(cpp_group_rows(data)); ++NP;
//   int n_groups = std::max(Rf_length(rows), 1);
//   const SEXP *p_rows = VECTOR_PTR_RO(rows);
//
//
//   // grab the variable names the expressions point to
//   SEXP quo_data_vars = SHIELD(quo_vars(quos, data, false)); ++NP;
//   SEXP quo_data_syms = SHIELD(new_vec(VECSXP, n_quos)); ++NP; // list of symbol lists
//   SEXP col_subsets = SHIELD(new_vec(VECSXP, n_quos)); ++NP; // List of col subsets
//
//   const SEXP *p_col_subsets = VECTOR_PTR_RO(col_subsets);
//
//   SEXP vars, new_vars;
//   PROTECT_INDEX vars_idx, new_vars_idx;
//   R_ProtectWithIndex(vars = new_vec(STRSXP, 0), &vars_idx); ++NP;
//   R_ProtectWithIndex(new_vars = new_vec(STRSXP, 0), &new_vars_idx); ++NP;
//
//   for (int i = 0; i < n_quos; ++i){
//
//     // Initialise data frame col subsets + col name symbols
//
//     // As we go through expressions, exclude previously
//     // referenced variables
//     // This is because when we go to evaluate them,
//     // previously referenced variables are already added to
//     // the mask top env and so don't need to be added again
//     // and data doesn't need to be re-filtered
//
//     R_Reprotect(new_vars = cheapr::setdiff(VECTOR_ELT(quo_data_vars, i), vars, false), new_vars_idx);
//     R_Reprotect(vars = binary_combine(vars, new_vars), vars_idx);
//     SET_VECTOR_ELT(col_subsets, i, cheapr::df_select(data, new_vars));
//
//     int n_vars = Rf_length(new_vars);
//     SET_VECTOR_ELT(quo_data_syms, i, new_vec(VECSXP, n_vars));
//
//     for (int j = 0; j < n_vars; ++j){
//       SET_VECTOR_ELT(VECTOR_ELT(quo_data_syms, i), j, Rf_installChar(STRING_ELT(new_vars, j)));
//     }
//
//     // Initialise expressions, environments and symbols of expression names
//
//     SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(VECTOR_ELT(quos, i)));
//     SET_VECTOR_ELT(envs, i, rlang::quo_get_env(VECTOR_ELT(quos, i)));
//     if (STRING_ELT(quo_names, i) == R_BlankString){
//       SET_VECTOR_ELT(quo_name_syms, i, R_UnboundValue);
//     } else {
//       SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
//     }
//   }
//
//   int n_used_vars = Rf_length(vars);
//
//   // Initialise components
//
//   // We will re-use the mask across all groups but add the same
//   // vars for each slice in each iteration
//   SEXP mask = SHIELD(new_bare_data_mask()); ++NP;
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
//   // Basically for the mask to work nicely we loop through each and group and
//   // in each group we loop through the expressions
//
//   // At the end we invert that so that we have a
//   // list of results of length `length(quos)`
//
//   SEXP outer_container = SHIELD(new_vec(VECSXP, n_groups)); ++NP;
//
//   // Recycling in this context means to recycle the results to a common size
//   // on a by-group basis, meaning the recycled sizes may differ between different
//   // groups
//
//   int recycled_size;
//   int result_size;
//
//   SEXP recycled_sizes;
//   PROTECT_INDEX recycled_sizes_idx;
//   R_ProtectWithIndex(recycled_sizes = R_NilValue, &recycled_sizes_idx); ++NP;
//
//   SEXP recycled_sizes_container = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
//   const SEXP *p_recycled_sizes_container = VECTOR_PTR_RO(recycled_sizes_container);
//   std::vector<int *> recycled_pointers(n_quos);
//
//   // Some C trickery here.. We repeat out the rows of the group keys at the end
//   // but if we recycle we only need to do this once and we only need one
//   // result sizes vector, otherwise we need one for each expression
//   if (recycle && n_quos > 0){
//     R_Reprotect(recycled_sizes = new_vec(INTSXP, n_groups), recycled_sizes_idx);
//     recycled_pointers[0] = INTEGER(recycled_sizes);
//     for (int m = 0; m < n_quos; ++m){
//       SET_VECTOR_ELT(recycled_sizes_container, m, recycled_sizes);
//       recycled_pointers[m] = recycled_pointers[0];
//     }
//   } else {
//     for (int m = 0; m < n_quos; ++m){
//       SET_VECTOR_ELT(recycled_sizes_container, m, new_vec(INTSXP, n_groups));
//       recycled_pointers[m] = INTEGER(VECTOR_ELT(recycled_sizes_container, m));
//     }
//   }
//
//   // YIELD(NP); return col_subsets;
//   for (int i = 0; i < n_groups; ++i){
//
//     // Start with a fresh top env for each group
//     reset_mask_top_env(mask, n_used_vars);
//     SEXP top_env = get_mask_top_env(mask);
//
//     recycled_size = -1; // Initialise this to < 0 for later logic to work
//
//     // inner container will contain the results of our expressions
//     // and we assign these inner containers to the bigger outer container
//     R_Reprotect(inner_container = new_vec(VECSXP, n_quos), inner_container_idx);
//
//     for (int m = 0; m < n_quos; ++m){
//
//       SEXP col_subset = p_col_subsets[m];
//
//       if (has_groups){
//         R_Reprotect(chunk = cheapr::df_slice(col_subset, p_rows[i], false), chunk_idx);
//       } else {
//         chunk = col_subset;
//       }
//
//       for (int l = 0; l < Rf_length(chunk); ++l){
//         Rf_defineVar(
//           VECTOR_ELT(VECTOR_ELT(quo_data_syms, m), l),
//           VECTOR_ELT(chunk, l), top_env
//         );
//       }
//
//       R_Reprotect(result = rlang::eval_tidy(
//         p_exprs[m], mask, p_envs[m]
//       ), result_idx);
//       if (p_quo_name_syms[m] != R_UnboundValue){
//         Rf_defineVar(p_quo_name_syms[m], result, top_env);
//       }
//       SET_VECTOR_ELT(inner_container, m, result);
//       result_size = cheapr::vec_length(result);
//       recycled_size = recycle ? (result_size == 0 ? 0 : recycled_size > result_size ? recycled_size : result_size) : result_size;
//       recycled_pointers[m][i] = recycled_size;
//     }
//
//     // recycle expression results to a common length
//
//     if (recycle){
//       for (int m = 0; m < n_quos; ++m){
//         SET_VECTOR_ELT(inner_container, m, cheapr::rep_len(VECTOR_ELT(inner_container, m), recycled_size));
//       }
//     }
//     SET_VECTOR_ELT(outer_container, i, inner_container);
//   }
//
//   // Combine results
//
//   SEXP results = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
//   set_names(results, quo_names);
//   const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);
//
//
//   // groups container will hold the repeated out rows of the group keys
//   SEXP groups_container = R_NilValue;
//   if (add_groups){
//     SHIELD(groups_container = new_vec(VECSXP, n_quos)); ++NP;
//     set_names(groups_container, quo_names);
//   }
//
//   SEXP repeated_groups;
//   PROTECT_INDEX repeated_groups_idx;
//   R_ProtectWithIndex(repeated_groups = R_NilValue, &repeated_groups_idx); ++NP;
//
//   if (add_groups && recycle && n_quos > 0){
//     R_Reprotect(repeated_groups = cheapr::rep(groups, p_recycled_sizes_container[0]), repeated_groups_idx);
//   }
//   for (int m = 0; m < n_quos; ++m){
//     R_Reprotect(inner_container = new_vec(VECSXP, n_groups), inner_container_idx);
//     for (int j = 0; j < n_groups; ++j){
//       SET_VECTOR_ELT(inner_container, j, VECTOR_ELT(p_outer_container[j], m));
//     }
//     R_Reprotect(result = cheapr::c(inner_container), result_idx);
//     if (add_groups){
//       if (!recycle){
//         R_Reprotect(repeated_groups = cheapr::rep(groups, p_recycled_sizes_container[m]), repeated_groups_idx);
//       }
//       SET_VECTOR_ELT(groups_container, m, repeated_groups);
//     }
//     SET_VECTOR_ELT(results, m, result);
//   }
//   SEXP out = SHIELD(new_vec(VECSXP, 2)); ++NP;
//   SET_VECTOR_ELT(out, 0, groups_container);
//   SET_VECTOR_ELT(out, 1, results);
//   SEXP out_names = SHIELD(new_vec(STRSXP, 2)); ++NP;
//   SET_STRING_ELT(out_names, 0, Rf_mkChar("groups"));
//   SET_STRING_ELT(out_names, 1, Rf_mkChar("results"));
//   set_names(out, out_names);
//   YIELD(NP);
//   return out;
// }

// Similar to above but with specific constraints around result sizes
// which makes the code somewhat simpler
// The structure is slightly different in that
// the groups are always constant for each result
// and so only one set of group keys is returned

[[cpp11::register]]
SEXP cpp_grouped_eval_summarise(SEXP data, SEXP quos){

  int32_t NP = 0;
  int n_quos = Rf_length(quos);

  SEXP group_keys = SHIELD(cpp_group_keys(data)); ++NP;

  // Add groups

  SEXP out = SHIELD(new_vec(VECSXP, 2)); ++NP;
  SEXP out_names = SHIELD(new_vec(STRSXP, 2)); ++NP;
  SET_STRING_ELT(out_names, 0, Rf_mkCharCE("groups", CE_UTF8));
  SET_STRING_ELT(out_names, 1, Rf_mkCharCE("results", CE_UTF8));
  set_names(out, out_names);

  SET_VECTOR_ELT(out, 0, group_keys);
  SET_VECTOR_ELT(out, 1, new_vec(VECSXP, 0));
  set_names(VECTOR_ELT(out, 1), new_vec(STRSXP, 0));

  if (n_quos == 0){
    YIELD(NP);
    return out;
  }

  SEXP rows = SHIELD(cpp_group_rows(data)); ++NP;
  SEXP quo_name_syms = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP quo_names = SHIELD(get_names(quos)); ++NP;
  SEXP exprs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP envs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  const SEXP *p_quos = VECTOR_PTR_RO(quos);
  const SEXP *p_quo_name_syms = VECTOR_PTR_RO(quo_name_syms);
  const SEXP *p_rows = VECTOR_PTR_RO(rows);
  const SEXP *p_exprs = VECTOR_PTR_RO(exprs);
  const SEXP *p_envs = VECTOR_PTR_RO(envs);

  int n_rows = df_nrow(data);
  int n_groups = std::max(df_nrow(group_keys), 1);

  // grab the variable names the expressions point to
  SEXP quo_data_vars = SHIELD(quo_vars(quos, data, true)); ++NP;
  int chunk_n_cols = Rf_length(quo_data_vars);
  SEXP quo_data_syms = SHIELD(new_vec(VECSXP, chunk_n_cols)); ++NP;
  const SEXP *p_quo_data_syms = VECTOR_PTR_RO(quo_data_syms);

  for (int i = 0; i < chunk_n_cols; ++i){
    SET_VECTOR_ELT(quo_data_syms, i, Rf_installChar(STRING_ELT(quo_data_vars, i)));
  }

  // We will re-use the mask across all groups but add the same
  // vars for each slice in each iteration
  SEXP mask = SHIELD(new_bare_data_mask()); ++NP;
  SEXP top_env = SHIELD(get_mask_top_env(mask)); ++NP;

  // Initialise symbols of expression names
  for (int i = 0; i < n_quos; ++i){
    SET_VECTOR_ELT(exprs, i, rlang::quo_get_expr(p_quos[i]));
    SET_VECTOR_ELT(envs, i, rlang::quo_get_env(p_quos[i]));
    if (STRING_ELT(quo_names, i) == R_BlankString){
      SET_VECTOR_ELT(quo_name_syms, i, R_UnboundValue);
    } else {
      SET_VECTOR_ELT(quo_name_syms, i, Rf_installChar(STRING_ELT(quo_names, i)));
    }
  }

  // Initialise components

  SEXP data_subset = SHIELD(cheapr::df_select(data, quo_data_vars)); ++NP;

  SEXP chunk, result, inner_container;

  PROTECT_INDEX chunk_idx, result_idx, inner_container_idx;

  R_ProtectWithIndex(chunk = data_subset, &chunk_idx); ++NP;
  R_ProtectWithIndex(result = R_NilValue, &result_idx); ++NP;
  R_ProtectWithIndex(inner_container = R_NilValue, &inner_container_idx); ++NP;


  // outer container will contain lists of expressions for each group
  // Basically for the mask to work nicely we loop through each and group and
  // in each group we loop through the expressions

  // At the end we invert that so that we have a
  // list of results of length `length(quos)`

  SEXP outer_container = SHIELD(new_vec(VECSXP, n_groups)); ++NP;

  for (int i = 0; i < n_groups; ++i){

    // Filter on the rows relevant to the current group

    R_Reprotect(chunk = cheapr::df_slice(data_subset, p_rows[i], false), chunk_idx);

    // assign variables to existing data mask
    // essentially list2env()

    for (int l = 0; l < chunk_n_cols; ++l){
      Rf_defineVar(p_quo_data_syms[l], VECTOR_ELT(chunk, l), top_env);
    }

    // inner container will contain the results of our expressions
    // and we assign these inner containers to the bigger outer container
    R_Reprotect(inner_container = new_vec(VECSXP, n_quos), inner_container_idx);

    for (int m = 0; m < n_quos; ++m){
      R_Reprotect(result = rlang::eval_tidy(p_exprs[m], mask, p_envs[m]), result_idx);

      int result_size = cheapr::vec_length(result);

      if (result_size != 1 && n_rows > 0){
        YIELD(NP);
        Rf_error(
          "Result size of %s in group %i must be 1, not %i",
          CHAR(STRING_ELT(quo_names, m)),
          i + 1,
          result_size
        );
      }

      if (p_quo_name_syms[m] != R_UnboundValue){
        Rf_defineVar(p_quo_name_syms[m], result, top_env);
      }
      SET_VECTOR_ELT(inner_container, m, result);
    }
    SET_VECTOR_ELT(outer_container, i, inner_container);
  }

  SEXP results = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  set_names(results, quo_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);

  for (int m = 0; m < n_quos; ++m){
    R_Reprotect(inner_container = new_vec(VECSXP, n_groups), inner_container_idx);
    for (int j = 0; j < n_groups; ++j){
      SET_VECTOR_ELT(inner_container, j, VECTOR_ELT(p_outer_container[j], m));
    }
    SET_VECTOR_ELT(results, m, cheapr::c(inner_container));
  }

  SET_VECTOR_ELT(out, 1, results);
  YIELD(NP);
  return out;
}

// Working, pretty fast but very messy

[[cpp11::register]]
SEXP cpp_grouped_eval_mutate(SEXP data, SEXP quos){
  int32_t NP = 0;
  int n_quos = Rf_length(quos);

  if (n_quos == 0){
    SEXP out = SHIELD(new_vec(VECSXP, 0));
    set_names(out, new_vec(STRSXP, 0));
    YIELD(1);
    return out;
  }

  bool has_groups = Rf_inherits(data, "grouped_df");
  int n_rows = df_nrow(data);
  SEXP exprs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP envs = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP quo_name_syms = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  SEXP quo_names = SHIELD(get_names(quos)); ++NP;
  const SEXP *p_quo_name_syms = VECTOR_PTR_RO(quo_name_syms);
  const SEXP *p_exprs = VECTOR_PTR_RO(exprs);
  const SEXP *p_envs = VECTOR_PTR_RO(envs);

  // Get group locations
  SEXP rows = SHIELD(cpp_group_rows(data)); ++NP;
  const SEXP *p_rows = VECTOR_PTR_RO(rows);
  int n_groups = std::max(Rf_length(rows), 1);


  // grab the variable names the expressions point to
  SEXP quo_data_vars = SHIELD(quo_vars(quos, data, true)); ++NP;
  int chunk_n_cols = Rf_length(quo_data_vars);
  SEXP quo_data_syms = SHIELD(new_vec(VECSXP, chunk_n_cols)); ++NP;
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

  SEXP data_subset = SHIELD(cheapr::df_select(data, quo_data_vars)); ++NP;

  // We will re-use the mask across all groups but add the same
  // vars for each slice in each iteration
  SEXP mask = SHIELD(new_bare_data_mask()); ++NP;
  SEXP top_env = SHIELD(get_mask_top_env(mask)); ++NP;

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

  SEXP outer_container = SHIELD(new_vec(VECSXP, n_groups)); ++NP;

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
    R_Reprotect(inner_container = new_vec(VECSXP, n_quos), inner_container_idx);

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

  SEXP results = SHIELD(new_vec(VECSXP, n_quos)); ++NP;
  set_names(results, quo_names);
  const SEXP *p_outer_container = VECTOR_PTR_RO(outer_container);

  for (int m = 0; m < n_quos; ++m){
    R_Reprotect(inner_container = new_vec(VECSXP, n_groups), inner_container_idx);
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
  SEXP grp_sym = SHIELD(Rf_install(".GRP")); ++NP;
  SEXP grp = SHIELD(Rf_getAttrib(quos, grp_sym)); ++NP;

  // Initialise
  SEXP results_without_null;
  PROTECT_INDEX results_without_null_idx;
  R_ProtectWithIndex(results_without_null = R_NilValue, &results_without_null_idx); ++NP;

  if (n_groups > 1){
    // Re-order the results
    R_Reprotect(results_without_null = cheapr::list_as_df(results), results_without_null_idx);

    if (TYPEOF(grp) == NILSXP){
      SHIELD(group_id = cpp_group_id(data)); ++NP;
      SHIELD(group_sizes = cpp_group_size(data)); ++NP;
    } else {
      SHIELD(group_id = get_list_element(grp, "group.id")); ++NP;
      SHIELD(group_sizes = get_list_element(grp, "group.sizes")); ++NP;
    }
    SHIELD(order = cpp_orig_order(group_id, group_sizes)); ++NP;
    SHIELD(sorted_sym = Rf_install("sorted")); ++NP;
    SHIELD(is_already_ordered = Rf_getAttrib(order, sorted_sym)); ++NP;
    if (TYPEOF(is_already_ordered) != LGLSXP || !LOGICAL(is_already_ordered)[0]){
      R_Reprotect(results_without_null = cheapr::sset(results_without_null, order, true), results_without_null_idx);
    }
    SHIELD(results = cheapr::list_assign(results, results_without_null)); ++NP;
  }
  YIELD(NP);
  return results;
}

[[cpp11::register]]
SEXP cpp_nest_split(SEXP data, SEXP drop, SEXP order){
  int32_t NP = 0;

  SEXP tbl_class = SHIELD(new_vec(STRSXP, 3)); ++NP;
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));

  SEXP group_data = SHIELD(cpp_group_data(data)); ++NP;
  SEXP group_vars = SHIELD(cpp_group_vars(data)); ++NP;
  SEXP rows = SHIELD(cpp_group_rows(data)); ++NP;
  SEXP names = SHIELD(get_names(data)); ++NP;
  SEXP frame;

  PROTECT_INDEX frame_idx;
  R_ProtectWithIndex(frame = R_NilValue, &frame_idx); ++NP;

  SEXP temp_cols = SHIELD(cheapr::setdiff(names, group_vars, false)); ++NP;
  SEXP temp = SHIELD(cheapr::df_select(data, temp_cols)); ++NP;

  const SEXP *p_rows = VECTOR_PTR_RO(rows);
  int n_groups = Rf_length(rows);
  SEXP frames = SHIELD(new_vec(VECSXP, n_groups)); ++NP;
  SHALLOW_DUPLICATE_ATTRIB(frames, rows);


  // Slice group chunks

  for (int i = 0; i < n_groups; ++i){
    R_Reprotect(frame = cheapr::df_slice(temp, p_rows[i], false), frame_idx);
    Rf_classgets(frame, tbl_class);
    SET_VECTOR_ELT(frames, i, frame);
  }

  SEXP out = SHIELD(Rf_shallow_duplicate(group_data)); ++NP;
  SEXP out_names = SHIELD(Rf_duplicate(get_names(out))); ++NP;
  SET_STRING_ELT(out_names, Rf_length(out) - 1, Rf_mkChar("data"));
  set_names(out, out_names);


  // Set prototype of data

  SEXP frames_class = SHIELD(new_vec(STRSXP, 3)); ++NP;
  SET_STRING_ELT(frames_class, 0, Rf_mkChar("vctrs_list_of"));
  SET_STRING_ELT(frames_class, 1, Rf_mkChar("vctrs_vctr"));
  SET_STRING_ELT(frames_class, 2, Rf_mkChar("list"));
  SEXP frame_ptype = SHIELD(cheapr::get_ptype(VECTOR_ELT(frames, 0))); ++NP;
  set_as_tbl(frame_ptype);
  Rf_setAttrib(frames, Rf_install("ptype"), frame_ptype);
  Rf_classgets(frames, frames_class);

  SET_VECTOR_ELT(out, Rf_length(out) - 1, frames);

  // Add groups attribute

  SEXP groups = SHIELD(Rf_shallow_duplicate(group_data)); ++NP;
  Rf_setAttrib(groups, Rf_install(".drop"), drop);
  Rf_setAttrib(groups, Rf_install("ordered"), order);
  SEXP group_rows_seq = SHIELD(cheapr::seq_len(df_nrow(groups))); ++NP;
  SEXP group_rows = SHIELD(Rf_coerceVector(group_rows_seq, VECSXP)); ++NP;
  set_as_vctrs_new_list_of_int(group_rows);
  SET_VECTOR_ELT(groups, Rf_length(groups) - 1, group_rows);
  Rf_setAttrib(out, Rf_install("groups"), groups);

  SEXP out_class = SHIELD(new_vec(STRSXP, 4)); ++NP;

  SET_STRING_ELT(out_class, 0, Rf_mkChar("grouped_df"));
  SET_STRING_ELT(out_class, 1, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(out_class, 2, Rf_mkChar("tbl"));
  SET_STRING_ELT(out_class, 3, Rf_mkChar("data.frame"));

  Rf_classgets(out, out_class);
  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_split(SEXP data){

  SEXP rows = SHIELD(cpp_group_rows(data));
  SEXP tbl_class = SHIELD(new_vec(STRSXP, 3));
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));

  const SEXP *p_rows = VECTOR_PTR_RO(rows);
  int n_groups = Rf_length(rows);
  SEXP frames = SHIELD(new_vec(VECSXP, n_groups));

  SHIELD(data = cpp_ungroup(data));


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

  YIELD(5);
  return frames;
}

// A fast method for converting a 'grouped_df' into a 'GRP'

[[cpp11::register]]
SEXP cpp_grouped_df_as_grp(SEXP data){
  int32_t NP = 0;
  int nrows = df_nrow(data);

  // Initialise needed symbols
  SEXP grp_char = SHIELD(Rf_mkCharCE("GRP", CE_UTF8)); ++NP;

  SEXP n_groups_char = SHIELD(Rf_mkCharCE("N.groups", CE_UTF8)); ++NP;
  SEXP group_id_char = SHIELD(Rf_mkCharCE("group.id", CE_UTF8)); ++NP;
  SEXP group_sizes_char = SHIELD(Rf_mkCharCE("group.sizes", CE_UTF8)); ++NP;
  SEXP groups_char = SHIELD(Rf_mkCharCE("groups", CE_UTF8)); ++NP;
  SEXP group_vars_char = SHIELD(Rf_mkCharCE("group.vars", CE_UTF8)); ++NP;
  SEXP order_char = SHIELD(Rf_mkCharCE("order", CE_UTF8)); ++NP;
  SEXP group_starts_char = SHIELD(Rf_mkCharCE("group.starts", CE_UTF8)); ++NP;
  SEXP call_char = SHIELD(Rf_mkCharCE("call", CE_UTF8)); ++NP;
  SEXP locs_char = SHIELD(Rf_mkCharCE("locs", CE_UTF8)); ++NP;

  SEXP starts_char = SHIELD(Rf_mkCharCE("starts", CE_UTF8)); ++NP;
  SEXP maxgrpn_char = SHIELD(Rf_mkCharCE("maxgrpn", CE_UTF8)); ++NP;
  SEXP ordered_char = SHIELD(Rf_mkCharCE("ordered", CE_UTF8)); ++NP;
  SEXP sorted_char = SHIELD(Rf_mkCharCE("sorted", CE_UTF8)); ++NP;


  SEXP grp = SHIELD(Rf_getAttrib(data, Rf_installChar(grp_char))); ++NP;
  if (TYPEOF(grp) != NILSXP){
    YIELD(NP);
    return grp;
  }

  // Initialise needed components

  SEXP out = SHIELD(new_vec(VECSXP, 10)); ++NP;
  SEXP out_names = SHIELD(new_vec(STRSXP, 10)); ++NP;
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
  set_names(out, out_names);

  SEXP group_data = SHIELD(cpp_group_data(data)); ++NP;
  int n_group_vars = Rf_length(group_data) - 1;
  SEXP group_rows = VECTOR_ELT(group_data, n_group_vars);
  int ngroups = Rf_length(group_rows);

  bool groups_are_ordered = cpp_group_by_order_default(data);

  SEXP grp_class = SHIELD(Rf_ScalarString(grp_char)); ++NP;
  SEXP n_groups = SHIELD(Rf_ScalarInteger(ngroups)); ++NP;
  SEXP group_id = SHIELD(new_vec(INTSXP, nrows)); ++NP;
  SEXP group_order = SHIELD(new_vec(INTSXP, nrows)); ++NP;
  SEXP group_starts = SHIELD(new_vec(INTSXP, nrows == 0 ? 0 : ngroups)); ++NP;
  SEXP group_sizes = SHIELD(new_vec(INTSXP, nrows == 0 ? 0 : ngroups)); ++NP;
  SEXP sorted_group_starts = SHIELD(new_vec(INTSXP, nrows == 0 ? 0 : ngroups)); ++NP;
  SEXP sorted = SHIELD(new_vec(LGLSXP, 1)); ++NP;
  SEXP r_max_group_size = SHIELD(new_vec(INTSXP, 1)); ++NP;
  SEXP groups = SHIELD(cpp_group_keys(data)); ++NP;
  SEXP group_vars = SHIELD(cpp_group_vars(data)); ++NP;
  SEXP ordered = SHIELD(new_vec(LGLSXP, 2)); ++NP;
  SEXP ordered_nms = SHIELD(new_vec(STRSXP, 2)); ++NP;

  SEXP group_locs = SHIELD(Rf_shallow_duplicate(group_rows)); ++NP;
  SHIELD(group_locs = cheapr::set_rm_attrs(group_locs)); ++NP;

  LOGICAL(ordered)[0] = groups_are_ordered;
  LOGICAL(ordered)[1] = groups_are_ordered ? true : NA_LOGICAL;
  SET_STRING_ELT(ordered_nms, 0, ordered_char);
  SET_STRING_ELT(ordered_nms, 1, sorted_char);
  set_names(ordered, ordered_nms);

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
    YIELD(NP);
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

    safe_memcpy(&p_group_order[k], &p_rows_i[0], group_size * sizeof(int));

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

      safe_memcpy(&p_group_order[k], &p_rows_i[0], group_size * sizeof(int));

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
  YIELD(NP);
  return out;
}
