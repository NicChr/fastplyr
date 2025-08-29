#include "fastplyr.h"
#include <R.h>
#include <cheapr_api.h>

SEXP get_data_GRP(SEXP x){
  return Rf_getAttrib(x, Rf_install("GRP"));
}

[[cpp11::register]]
SEXP cpp_group_data(SEXP x){
  if (Rf_inherits(x, "grouped_df")){
    return Rf_getAttrib(x, Rf_install("groups"));
  } else if (Rf_inherits(x, "data.frame")){

    SEXP groups = SHIELD(new_vec(VECSXP, 1));
    SEXP names = SHIELD(new_vec(STRSXP, 1));
    SET_STRING_ELT(names, 0, Rf_mkChar(".rows"));

    // Rows
    SEXP rows = SHIELD(new_vec(VECSXP, 1));
    SET_VECTOR_ELT(rows, 0, compact_int_seq_len(df_nrow(x)));
    set_as_vctrs_new_list_of_int(rows);
    SET_VECTOR_ELT(groups, 0, rows);
    set_names(groups, names);
    SHIELD(groups = cheapr::list_as_df(groups));
    set_as_tbl(groups);
    YIELD(4);
    return groups;
  } else {
    Rf_error("`x` must be a data frame in %s", __func__);
  }
}

[[cpp11::register]]
SEXP cpp_group_keys(SEXP x){

  SEXP out = R_NilValue;

  if (Rf_inherits(x, "grouped_df")){
    SEXP group_data = SHIELD(cpp_group_data(x));
    SEXP seq = SHIELD(cheapr::seq_len(Rf_length(group_data) - 1));
    out = SHIELD(cheapr::df_select(group_data, seq));
  } else {
    SEXP r_nrows = SHIELD(Rf_ScalarInteger(1));
    SEXP empty_list = SHIELD(new_vec(VECSXP, 0));
    out = SHIELD(cheapr::new_df(empty_list, r_nrows, false, false));
  }
  set_as_tbl(out);
  YIELD(3);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_vars(SEXP x){
  if (Rf_inherits(x, "grouped_df")){
    SEXP group_keys = SHIELD(cpp_group_keys(x));
    SEXP out = SHIELD(get_names(group_keys));
    YIELD(2);
    return out;
  } else {
    return new_vec(STRSXP, 0);
  }
  // return Rf_inherits(x, "grouped_df") ? Rf_getAttrib(cpp_group_keys(x), R_NamesSymbol) : new_vec(STRSXP, 0);
}

[[cpp11::register]]
SEXP cpp_group_rows(SEXP x){
  SEXP group_data = SHIELD(cpp_group_data(x));
  SEXP out = VECTOR_ELT(group_data, Rf_length(group_data) - 1);
  YIELD(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_size(SEXP x){

  SEXP out = R_NilValue;

  if (Rf_inherits(x, "fastplyr_grouped_df")){
    SEXP grp = SHIELD(get_data_GRP(x));
    SHIELD(out = get_list_element(grp, "group.sizes"));
  } else {
    SEXP group_rows = SHIELD(cpp_group_rows(x));
    SHIELD(out = cheapr::lengths(group_rows, false));
  }
  YIELD(2);
  return out;
}


[[cpp11::register]]
SEXP cpp_ungroup(SEXP data){
  int32_t NP = 0;
  if (Rf_inherits(data, "grouped_df")){
    SEXP out = SHIELD(Rf_shallow_duplicate(data)); ++NP;
    SEXP groups_sym = Rf_install("groups");
    SEXP grp_sym = Rf_install("GRP");
    Rf_setAttrib(out, groups_sym, R_NilValue);
    Rf_setAttrib(out, grp_sym, R_NilValue);
    SEXP old_class = SHIELD(Rf_getAttrib(out, R_ClassSymbol)); ++NP;
    SEXP grouped_df_char = SHIELD(Rf_mkChar("grouped_df")); ++NP;
    SEXP fp_grouped_df_char = SHIELD(Rf_mkChar("fastplyr_grouped_df")); ++NP;
    SEXP grp_df_char = SHIELD(Rf_mkChar("GRP_df")); ++NP;
    SEXP remove = SHIELD(new_vec(STRSXP, 3)); ++NP;
    SET_STRING_ELT(remove, 0, grouped_df_char);
    SET_STRING_ELT(remove, 1, fp_grouped_df_char);
    SET_STRING_ELT(remove, 2, grp_df_char);

    SEXP new_class = SHIELD(cheapr::setdiff(old_class, remove, false)); ++NP;
    Rf_classgets(out, new_class);
    YIELD(NP);
    return out;
  }
  YIELD(NP);
  return data;
}

// Taken from dplyr::group_indices,
// All credits go to dplyr

[[cpp11::register]]
SEXP cpp_group_indices(SEXP rows, int size) {

  SEXP indices = SHIELD(new_vec(INTSXP, size));
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
  YIELD(1);
  return indices;
}

[[cpp11::register]]
bool cpp_group_by_drop_default(SEXP x){
  if (Rf_inherits(x, "grouped_df")){
    SEXP groups = SHIELD(cpp_group_data(x));
    SEXP drop_sym = SHIELD(Rf_install(".drop"));
    SEXP out = Rf_getAttrib(groups, drop_sym);
    YIELD(2);
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

  SEXP ordered_sym = SHIELD(Rf_install("ordered")); ++NP;

  if (Rf_inherits(x, "grouped_df")){
    SEXP group_data = SHIELD(cpp_group_data(x)); ++NP;
    SEXP ordered = SHIELD(Rf_getAttrib(group_data, ordered_sym)); ++NP;
    if (TYPEOF(ordered) == NILSXP){
      out = true;
      YIELD(NP);
      return out;
    } else if (Rf_length(ordered) == 1){
      out = LOGICAL(ordered)[0];
      YIELD(NP);
      return out;
    }
  }

  SEXP fp_order_groups_sym = SHIELD(Rf_install(".fastplyr.order.groups")); ++NP;
  SEXP order_groups = SHIELD(Rf_GetOption1(fp_order_groups_sym)); ++NP;

  if (TYPEOF(order_groups) != NILSXP){
    if (TYPEOF(order_groups) != LGLSXP || Rf_length(order_groups) != 1){
      YIELD(NP);
      Rf_error("'.fastplyr.order.groups' option must be either `TRUE` or `FALSE`");
    }
    out = LOGICAL(order_groups)[0];
    if (out == NA_LOGICAL){
      YIELD(NP);
      Rf_error("'.fastplyr.order.groups' option must be either `TRUE` or `FALSE`");
    }
  }
  YIELD(NP);
  return out;
}

int n_group_vars(SEXP x){
  return Rf_length(cpp_group_vars(x));
}

[[cpp11::register]]
SEXP cpp_group_id(SEXP x){
  if (!Rf_inherits(x, "grouped_df") && !Rf_inherits(x, "data.frame")){
    Rf_error("Can only calculate group indices on data frames in %s", __func__);
  }

  if (Rf_inherits(x, "fastplyr_grouped_df")){
    SEXP grp = SHIELD(get_data_GRP(x));
    SEXP out = SHIELD(get_list_element(grp, "group.id"));
    YIELD(2);
    return out;
  }

  int n = df_nrow(x);
  SEXP out;
  if (n_group_vars(x) == 0){
    SEXP r_one = SHIELD(Rf_ScalarInteger(1));
    out = SHIELD(cheapr::rep_len(r_one, n));
  } else {
    SEXP group_rows = SHIELD(cpp_group_rows(x));
    out = SHIELD(cpp_group_indices(group_rows, n));
  }
  YIELD(2);
  return out;
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


// Take a vector of group sizes (sorted by group)
// And this will return a vector of the start indices of each group (in sorted order)

[[cpp11::register]]
SEXP cpp_sorted_group_starts(SEXP group_sizes, int init_loc = 1){
  const int* __restrict__ p_gsizes = INTEGER_RO(group_sizes);
  int n = Rf_length(group_sizes);
  SEXP out = SHIELD(new_vec(INTSXP, n));
  int* __restrict__ p_out = INTEGER(out);
  if (n > 0){
    int init = init_loc;
    p_out[0] = init;
    // cumsum over group_sizes[-length(group_sizes)]
    for (int i = 0; i < (n - 1); ++i){
      p_out[i + 1] = (init += p_gsizes[i]);
    }
  }
  YIELD(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_locs(SEXP order, SEXP group_sizes){
  int32_t NP = 0;
  unsigned int n_groups = Rf_length(group_sizes);
  const int* __restrict__ p_o = INTEGER_RO(order);
  const int* __restrict__ p_gs = INTEGER_RO(group_sizes);
  SEXP group_locs = SHIELD(new_vec(VECSXP, n_groups)); ++NP;
  const SEXP *p_out = VECTOR_PTR_RO(group_locs);
  unsigned int k = 0;
  unsigned int group_size = 0;

  for (unsigned int i = 0; i < n_groups; ++i, k += group_size){
    group_size = p_gs[i];
    SET_VECTOR_ELT(group_locs, i, new_vec(INTSXP, group_size));
    safe_memcpy(&INTEGER(p_out[i])[0], &p_o[k], group_size * sizeof(int));
  }
  YIELD(NP);
  return group_locs;
}

// Alternative to above that can calculate it using
// group IDs instead of the order

// group IDs must be in range [1, n_groups]

[[cpp11::register]]
SEXP cpp_group_locs2(SEXP group_id, SEXP group_sizes){
  int32_t NP = 0;
  unsigned int n_groups = Rf_length(group_sizes);
  SEXP group_locs = SHIELD(new_vec(VECSXP, n_groups)); ++NP;
  const int* __restrict__ p_group_sizes = INTEGER_RO(group_sizes);
  const int* __restrict__ p_group_id = INTEGER_RO(group_id);
  const SEXP *p_out = VECTOR_PTR_RO(group_locs);

  // Store a vector of pointers
  // Speeds up later allocation
  int **loc_ptrs = (int **) R_Calloc(n_groups, int*);

  // Initialise locations
  for (unsigned int i = 0; i != n_groups; ++i){
    SET_VECTOR_ELT(group_locs, i, new_vec(INTSXP, p_group_sizes[i]));
    loc_ptrs[i] = INTEGER(p_out[i]);
  }

  // Initialise a vector of group location indices

  SEXP loc_indices = SHIELD(new_vec(INTSXP, n_groups)); ++NP;
  int* __restrict__ p_loc_indices = INTEGER(loc_indices);
  safe_memset(p_loc_indices, 0, n_groups * sizeof(int));

  int n = Rf_length(group_id);
  int cur_group;
  int cur_group_loc;

  for (int i = 0; i < n; ++i){
    cur_group = p_group_id[i] - 1;
    cur_group_loc = p_loc_indices[cur_group]++;
    loc_ptrs[cur_group][cur_group_loc] = i + 1;
  }
  R_Free(loc_ptrs);
  YIELD(NP);
  return group_locs;
}
[[cpp11::register]]
SEXP cpp_vec_group_split(SEXP x, SEXP locs){

  int n_groups = Rf_length(locs);

  const SEXP *p_locs = VECTOR_PTR_RO(locs);
  SEXP out = SHIELD(new_vec(VECSXP, n_groups));

  for (int i = 0; i < n_groups; ++i){
    SET_VECTOR_ELT(out, i, cheapr::sset(x, p_locs[i], true));
  }
  YIELD(1);
  return out;
}

// Using a combination of group_id and sorted group_sizes
// we can quickly calculate an order vector that places
// sorted group IDs back into original order

[[cpp11::register]]
SEXP cpp_orig_order(SEXP group_id, SEXP group_sizes){
  int n = Rf_length(group_id);
  int n_groups = Rf_length(group_sizes);
  const int* __restrict__ p_group_id = INTEGER(group_id);

  if (n_groups == 0){
    return new_vec(INTSXP, 0);
  }

  // Sorted group start locs
  SEXP cumulative_sizes = SHIELD(cpp_sorted_group_starts(group_sizes, 0));
  int* __restrict__ p_cumulative_sizes = INTEGER(cumulative_sizes);

  SEXP out = SHIELD(new_vec(INTSXP, n));
  int* __restrict__ p_out = INTEGER(out);

  int ans;

  bool sorted = true;
  // for (int i = 0; i < n; ++i){
  //   p_out[i] = ++p_cumulative_sizes[p_group_id[i] - 1];
  // }
  for (int i = 0; i < n; ++i){
    ans = ++p_cumulative_sizes[p_group_id[i] - 1];
    sorted = sorted && ans == (i + 1);
    p_out[i] = ans;
  }
  SEXP sorted_sym = SHIELD(Rf_install("sorted"));
  SEXP r_sorted = SHIELD(new_vec(LGLSXP, 1));
  LOGICAL(r_sorted)[0] = sorted;
  Rf_setAttrib(out, sorted_sym, r_sorted);
  YIELD(4);
  return out;
}
