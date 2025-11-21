#include "fastplyr.h"
#include <R.h>

SEXP get_list_element(SEXP list, const char *str){
  SEXP out = R_NilValue;
  SEXP names = SHIELD(get_names(list));

  for (int i = 0; i < Rf_length(list); ++i){
    if (std::strcmp(CHAR(STRING_ELT(names, i)), str) == 0){
      out = VECTOR_ELT(list, i);
      break;
    }
  }
  YIELD(1);
  return out;
}

// Compare the addresses between 2 similar lists

[[cpp11::register]]
SEXP cpp_frame_addresses_equal(SEXP x, SEXP y) {
  const SEXP* p_x = VECTOR_PTR_RO(x);
  const SEXP* p_y = VECTOR_PTR_RO(y);
  int n1 = Rf_length(x);
  int n2 = Rf_length(y);
  if (n1 != n2){
    Rf_error("x and y must be of the same length");
  }
  SEXP out = SHIELD(new_vec(LGLSXP, n1));
  int* __restrict__ p_out = LOGICAL(out);
  for (int i = 0; i < n1; ++i) {
    p_out[i] = (cheapr::r_address(p_x[i]) == cheapr::r_address(p_y[i]));
  }
  YIELD(1);
  return out;
}


// nrows/ncols of a list of data frames, typically supplied through ...

[[cpp11::register]]
SEXP cpp_frame_dims(SEXP x, bool check_rows_equal, bool check_cols_equal) {
  int32_t NP = 0;
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);
  SEXP nrows = SHIELD(new_vec(INTSXP, n)); ++NP;
  SEXP ncols = SHIELD(new_vec(INTSXP, n)); ++NP;
  int* __restrict__ p_nrows = INTEGER(nrows);
  int* __restrict__ p_ncols = INTEGER(ncols);

  if (n < 2){
    for (int i = 0; i < n; ++i) {
      if (!Rf_inherits(p_x[i], "data.frame")){
        YIELD(NP);
        Rf_error("All inputs must be data frames");
      }
      p_nrows[i] = df_nrow(p_x[i]);
      p_ncols[i] = Rf_length(p_x[i]);
    }
  } else {

    // First data frame

    if (!Rf_inherits(p_x[0], "data.frame")){
      YIELD(NP);
      Rf_error("All inputs must be data frames");
    }
    int n_rows = df_nrow(p_x[0]);
    int n_cols = Rf_length(p_x[0]);
    p_nrows[0] = n_rows;
    p_ncols[0] = n_cols;

    // All others

    for (int i = 1; i < n; ++i) {
      if (!Rf_inherits(p_x[i], "data.frame")){
        YIELD(NP);
        Rf_error("All inputs must be data frames");
      }
      p_nrows[i] = df_nrow(p_x[i]);
      p_ncols[i] = Rf_length(p_x[i]);
      if (check_rows_equal && p_nrows[i] != n_rows){
        YIELD(NP);
        Rf_error("All input data frames must have the same number of rows");
      }
      if (check_cols_equal && p_ncols[i] != n_cols){
        YIELD(NP);
        Rf_error("All input data frames must have the same number of cols");
      }
    }
  }
  SEXP out = SHIELD(new_vec(VECSXP, 2)); ++NP;
  SET_VECTOR_ELT(out, 0, nrows);
  SET_VECTOR_ELT(out, 1, ncols);
  YIELD(NP);
  return out;
}

[[cpp11::register]]
bool cpp_is_exotic(SEXP x){
  return !cheapr::is_simple_atomic_vec(x);
}

// Are any list elements data frames?

[[cpp11::register]]
bool cpp_any_frames(SEXP x){
  bool out = false;
  int n_dots = Rf_length(x);
  const SEXP *p_x = VECTOR_PTR_RO(x);
  for (int i = 0; i < n_dots; ++i){
    if (Rf_inherits(p_x[i], "data.frame")){
      out = true;
      break;
    }
  }
  return out;
}

// Specifically applied to a list of data frames, used in `f_bind_rows()`

[[cpp11::register]]
bool cpp_any_frames_exotic(SEXP x){
  bool out = false;
  int n_dots = Rf_length(x);
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int ncol;
  for (int i = 0; i < n_dots; ++i){
    ncol = Rf_length(p_x[i]);
    const SEXP *df_ptr = VECTOR_PTR_RO(p_x[i]);
    for (int j = 0; j < ncol; ++j){
      if (cpp_is_exotic(df_ptr[j])){
        out = true;
        break;
      }
    }
  }
  return out;
}

[[cpp11::register]]
SEXP cpp_as_list_of_frames(SEXP x){
  const SEXP *p_x = VECTOR_PTR_RO(x);

  int n = Rf_length(x);

  SEXP out = SHIELD(new_vec(VECSXP, n));
  SEXP names = SHIELD(get_names(x));
  bool has_names = !Rf_isNull(names);
  SEXP tbl_class = SHIELD(new_vec(STRSXP, 3));
  SET_STRING_ELT(tbl_class, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(tbl_class, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(tbl_class, 2, Rf_mkChar("data.frame"));

  SEXP result;
  PROTECT_INDEX index;

  R_ProtectWithIndex(result = R_NilValue, &index);

  for (int i = 0; i < n; ++i) {
    result = p_x[i];
    if (!Rf_inherits(result, "data.frame")){
      R_Reprotect(result = new_vec(VECSXP, 1), index);
      SET_VECTOR_ELT(result, 0, p_x[i]);
      if (has_names){
        set_names(result, Rf_ScalarString(STRING_ELT(names, i)));
      }
      R_Reprotect(result = cheapr::list_as_df(result), index);
      Rf_classgets(result, tbl_class);
    }
    SET_VECTOR_ELT(out, i, result);
  }
  if (has_names){
    set_names(out, names);
  }
  YIELD(4);
  return out;
}

// Fast extract 1 element from each list element
[[cpp11::register]]
SEXP cpp_pluck_list_of_integers(SEXP x, SEXP i, SEXP default_value){
  int32_t NP = 0;
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);
  int i_n = Rf_length(i);
  int k;
  if (n == 0){
    YIELD(NP);
    return new_vec(INTSXP, 0);
  }
  if (!(i_n == 1 || (n > 0 && i_n == n))){
    YIELD(NP);
    Rf_error("i must be an integer vector of length 1 or of length(x)");
  }
  const int* __restrict__ p_i = INTEGER_RO(i);
  int replace = Rf_asInteger(default_value);
  SEXP out = SHIELD(new_vec(INTSXP, n)); ++NP;
  int* __restrict__ p_out = INTEGER(out);

  for (int j = 0; j < n; ++j) {
    k = (i_n == 1 ? p_i[0] : p_i[j]);
    if (k <= Rf_length(p_x[j]) && k > 0){
      p_out[j] = INTEGER_RO(p_x[j])[k - 1];
    } else {
      p_out[j] = replace;
    }
  }

  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_row_id(SEXP order, SEXP group_sizes, bool ascending){

  int n = Rf_length(order);
  int n_groups = Rf_length(group_sizes);

  SEXP out = SHIELD(new_vec(INTSXP, n));

  int* __restrict__ p_out = INTEGER(out);
  const int* __restrict__ p_o = INTEGER_RO(order);
  const int* __restrict__ p_group_sizes = INTEGER_RO(group_sizes);

  int row_number, group_size;
  int k = 0;

  if (ascending){

    for (int j = 0; j < n_groups; ++j){
      group_size = p_group_sizes[j];
      row_number = 0;
      for (int i = 0; i < group_size; ++i, ++k){
        p_out[p_o[k] - 1] = ++row_number;
      }
    }
  } else {
    for (int j = 0; j < n_groups; ++j){
      group_size = p_group_sizes[j];
      row_number = group_size;
      for (int i = 0; i < group_size; ++i, ++k){
        p_out[p_o[k] - 1] = row_number--;
      }
    }
  }
  YIELD(1);
  return out;
}

// Alternative that goes row-wise through x
// When data is mixed it tends to be a bit faster

[[cpp11::register]]
SEXP cpp_which_all(SEXP x){
  if (!Rf_inherits(x, "data.frame")){
    Rf_error("x must be a data frame");
  }
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int32_t NP = 0;
  int n_true = 0;
  int n_cols = Rf_length(x);
  int n_rows = df_nrow(x);

  SEXP out;

  if (n_cols == 0){
    out = SHIELD(new_vec(INTSXP, 0)); ++NP;
  } else if (n_cols == 1){
    SEXP r_true = SHIELD(new_vec(LGLSXP, 1)); ++NP;
    LOGICAL(r_true)[0] = TRUE;
    out = SHIELD(cheapr::val_find(p_x[0], r_true, false)); ++NP;
  } else {
    SEXP lgl = SHIELD(new_vec(LGLSXP, n_rows)); ++NP;
    int* __restrict__ p_lgl = INTEGER(lgl);
    std::fill(p_lgl, p_lgl + n_rows, 0);

    // Save pointers to logical cols

    std::vector<const int*> col_ptrs(n_cols);

    for (int i = 0; i < n_cols; ++i){
      col_ptrs[i] = INTEGER_RO(p_x[i]);
    }

    bool is_true = false;
    int j;
    for (int i = 0; i < n_rows; ++i){
      is_true = true;
      j = 0;
      while (j < n_cols && is_true){
        is_true = col_ptrs[j++][i] == 1;
      }
      n_true += is_true;
      p_lgl[i] = is_true;
    }
    out = SHIELD(new_vec(INTSXP, n_true)); ++NP;
    int* __restrict__ p_out = INTEGER(out);
    int whichi = 0;
    int i = 0;
    while (whichi < n_true){
      p_out[whichi] = i + 1;
      whichi += (p_lgl[i++] == TRUE);
    }
  }
  YIELD(NP);
  return out;
}

// Slice integers (only in-bounds data is returned)
// indices must NOT INCLUDE NA values

SEXP int_slice(SEXP x, SEXP indices, const int *p_x, int xn, const int *pi, int indn){
  int32_t NP = 0;
  int k = 0;
  SEXP out = SHIELD(new_vec(INTSXP, indn)); ++NP;
  int* __restrict__ p_out = INTEGER(out);
  int j;
  for (int i = 0; i < indn; ++i){
    j = pi[i];
    if (j < 0){
      SEXP new_indices = SHIELD(cheapr::exclude_locs(indices, xn)); ++NP;
      SEXP out2 = SHIELD(cheapr::sset_vec(x, new_indices, false)); ++NP;
      YIELD(NP);
      return out2;
    } else if (j != 0 && j <= xn){
      p_out[k++] = p_x[j - 1];
    }
  }
  if (k != indn){
    SHIELD(out = Rf_lengthgets(out, k)); ++NP;
  }
  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_slice_locs(SEXP group_locs, SEXP locs){

  int32_t NP = 0;
  // int k = 0;

  int n_groups = Rf_length(group_locs);
  int locs_size = Rf_length(locs);

  const SEXP *p_group_locs = VECTOR_PTR_RO(group_locs);
  const int *p_locs = INTEGER_RO(locs);

  SEXP out = SHIELD(new_vec(VECSXP, n_groups)); ++NP;
  SEXP elem = R_NilValue;

  for (int i = 0; i < n_groups; ++i){
    elem = p_group_locs[i];
    SET_VECTOR_ELT(
      out, i,
      int_slice(
        elem, locs, INTEGER_RO(elem), Rf_length(elem), p_locs, locs_size
      )
    );
  }
  YIELD(NP);
  return out;
}

SEXP cpp_df_run_id(SEXP x){
  int32_t NP = 0;
  int n_cols = Rf_length(x);
  int n_rows = df_nrow(x);

  const SEXP *p_x = VECTOR_PTR_RO(x);

  for (int l = n_cols - 1; l >= 0; --l){
    if (cheapr::is_compact_seq(p_x[l])){
      SEXP out = SHIELD(compact_int_seq_len(n_rows)); ++NP;
      YIELD(NP);
      return out;
    }
    if (cpp_is_exotic(p_x[l])){
      SEXP group_ids = SHIELD(fp_group_id(p_x[l], cpp11::named_arg("order") = false)); ++NP;
      SHIELD(x = Rf_shallow_duplicate(x)); ++NP;
      SET_VECTOR_ELT(x, l, group_ids);
    }
  }

  // Re-point to the possibly shallow duplicated list `x`
  p_x = VECTOR_PTR_RO(x);

  SEXP out = SHIELD(new_vec(INTSXP, n_rows)); ++NP;
  int* __restrict__ p_out = INTEGER(out);

  if (n_cols < 1){
    std::fill(p_out, p_out + n_rows, 1);
    YIELD(NP);
    return out;
  }

  int k = 1;
  bool diff = false;

  if (n_rows >= 1){
    p_out[0] = 1;
  }

  for (int i = 1; i < n_rows; ++i){
   diff = false;
   int j = 0;
    while (j < n_cols && !diff){
      switch (TYPEOF(p_x[j])){
      case LGLSXP:
      case INTSXP: {
        const int *p_xj = INTEGER_RO(p_x[j]);
          diff = (p_xj[i] != p_xj[i - 1]);
          p_out[i] = (k += diff);
          break;
        }
      case REALSXP: {
        if (Rf_inherits(x, "integer64")){
        int64_t *p_xj = INTEGER64_PTR(p_x[j]);
        diff = (p_xj[i] != p_xj[i - 1]);
        p_out[i] = (k += diff);
      } else {
        const double *p_xj = REAL_RO(p_x[j]);
        diff = !(
          ((p_xj[i] != p_xj[i]) && (p_xj[i - 1] != p_xj[i - 1])) ||
            (p_xj[i] == p_xj[i - 1])
        );
        p_out[i] = (k += diff);
      }

        break;
      }
      case STRSXP: {
        const SEXP *p_xj = STRING_PTR_RO(p_x[j]);
        diff = (p_xj[i] != p_xj[i - 1]);
        p_out[i] = (k += diff);
        break;
      }
      case CPLXSXP: {
        const Rcomplex *p_xj = COMPLEX_RO(p_x[j]);
        diff = memcmp(&p_xj[i], &p_xj[i - 1], sizeof(Rcomplex)) != 0;
        p_out[i] = (k += diff);
        break;
      }
      case RAWSXP: {
        const Rbyte *p_xj = RAW_RO(p_x[j]);
        diff = memcmp(&p_xj[i], &p_xj[i - 1], sizeof(Rbyte)) != 0;
        p_out[i] = (k += diff);
       break;
      }
      default: {
        YIELD(NP);
        Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(p_x[j])));
      }
      }
      ++j;
    }
  }
  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_consecutive_id(SEXP x){
  if (Rf_inherits(x, "data.frame")){
    return cpp_df_run_id(x);
  } else {
    SEXP temp = SHIELD(new_vec(VECSXP, 1));
    SET_VECTOR_ELT(temp, 0, x);
    SHIELD(temp = cheapr::new_df(temp, R_NilValue, false, false));
    SEXP out = SHIELD(cpp_df_run_id(temp));
    YIELD(3);
    return out;
  }
}

#define FASTPLYR_GROUP_RESET                                   \
                                                               \
group_size = p_group_sizes[i];                                 \
total_group_size += group_size;                                \
if (total_group_size > n){                                     \
  YIELD(NP);                                            \
  Rf_error("`sum(group_sizes)` must equal `length(x)`");       \
}

// x assumed to be an integer vector of unique group IDs

[[cpp11::register]]
SEXP cpp_grouped_run_id(SEXP x, SEXP order, SEXP group_sizes){
  int n = Rf_length(x);
  int *p_x = INTEGER(x);
  int *p_o = INTEGER(order);
  int *p_group_sizes = INTEGER(group_sizes);
  if (n != Rf_length(order)){
    Rf_error("length(order) must match length(x)");
  }
  SEXP out = SHIELD(new_vec(INTSXP, n));
  int *p_out = INTEGER(out);
  int n_groups = Rf_length(group_sizes);
  int k = 0;
  int oi, oi2, group_size;
  int total_group_size = 0;
  for (int i = 0; i < n_groups; ++i){
    group_size = p_group_sizes[i];
    total_group_size += group_size;
    if (total_group_size > n){
      YIELD(1);
      Rf_error("sum(group_sizes) must equal length(x)");
    }
    if (group_size >= 1){
      p_out[p_o[k] - 1] = 1;
      ++k;
    }
    for (int j = 1; j < group_size; ++k, ++j){
      oi = p_o[k] - 1;
      oi2 = p_o[k - 1] - 1;
      p_out[oi] = p_out[oi2] + (p_x[oi] != p_x[oi2]);
    }
  }
  if (total_group_size != n){
    YIELD(1);
    Rf_error("sum(group_sizes) must equal length(x)");
  }
  YIELD(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_set_list_element(SEXP x, R_xlen_t i, SEXP value){
  return SET_VECTOR_ELT(x, i - 1, value);
}


// LOCF - Last-Observation-Carried-Forward
// With a user-specified ordering and run lengths (or group sizes)
// Given groups `g`, this will fill `NA` values using LOCF by-group

[[cpp11::register]]
SEXP cpp_fill_grouped(SEXP x, SEXP order, SEXP group_sizes, double fill_limit) {
  int n = Rf_length(x);
  int o_size = Rf_length(order);
  fill_limit = std::fmax(fill_limit, 0);
  int *p_o = INTEGER(order);
  int *p_group_sizes = INTEGER(group_sizes);
  int n_groups = Rf_length(group_sizes);
  int32_t NP = 0;
  int oi;
  int group_size, nfill;
  int total_group_size = 0;
  int k = 0;

  SEXP out;

  switch(TYPEOF(x)){
  case NILSXP: {
    out = SHIELD(R_NilValue); ++NP;
    break;
  }
  case LGLSXP:
  case INTSXP: {
    if (o_size != n){
    YIELD(NP);
    Rf_error("x and order must both be the same length");
  }
    int last_obs;
    out = SHIELD(Rf_duplicate(x)); ++NP;
    int *p_x = INTEGER(x);
    int *p_out = INTEGER(out);
    for (int i = 0; i < n_groups; ++i){
      nfill = 0; // Reset fill limit
      FASTPLYR_GROUP_RESET
      last_obs = p_x[p_o[k] - 1];
      for (int j = 0; j < group_size; ++k, ++j){
        oi = p_o[k] - 1;
        if (p_x[oi] == NA_INTEGER && nfill < fill_limit){
          p_out[oi] = last_obs;
          ++nfill;
        } else {
          last_obs = p_out[oi];
          nfill = 0;
        }
      }
    }
    if (total_group_size != n){
      YIELD(NP);
      Rf_error("`sum(group_sizes)` must equal `length(x)`");
    }
    break;
  }
  case REALSXP: {
    if (Rf_inherits(x, "integer64")){
    if (o_size != n){
      YIELD(NP);
      Rf_error("x and order must both be the same length");
    }
    int64_t last_obs;
    out = SHIELD(Rf_duplicate(x)); ++NP;
    int64_t *p_x = INTEGER64_PTR(x);
    int64_t *p_out = INTEGER64_PTR(out);
    for (int i = 0; i < n_groups; ++i){
      nfill = 0; // Reset fill limit
      FASTPLYR_GROUP_RESET
      last_obs = p_x[p_o[k] - 1];
      for (int j = 0; j < group_size; ++k, ++j){
        oi = p_o[k] - 1;
        if (p_x[oi] == LLONG_MIN && nfill < fill_limit){
          p_out[oi] = last_obs;
          ++nfill;
        } else {
          last_obs = p_out[oi];
          nfill = 0;
        }
      }
    }
  } else {
    if (o_size != n){
      YIELD(NP);
      Rf_error("x and order must both be the same length");
    }
    double last_obs;
    out = SHIELD(Rf_duplicate(x)); ++NP;
    double *p_x = REAL(x);
    double *p_out = REAL(out);
    for (int i = 0; i < n_groups; ++i){
      nfill = 0; // Reset fill limit
      FASTPLYR_GROUP_RESET
      last_obs = p_x[p_o[k] - 1];
      for (int j = 0; j < group_size; ++k, ++j){
        oi = p_o[k] - 1;
        if (p_x[oi] != p_x[oi] && nfill < fill_limit){
          p_out[oi] = last_obs;
          ++nfill;
        } else {
          last_obs = p_out[oi];
          nfill = 0;
        }
      }
    }
  }
  if (total_group_size != n){
    YIELD(NP);
    Rf_error("`sum(group_sizes)` must equal `length(x)`");
  }
  break;
  }
  case STRSXP: {
    if (o_size != n){
    YIELD(NP);
    Rf_error("x and order must both be the same length");
  }
    SEXP last_obs;
    out = SHIELD(Rf_duplicate(x)); ++NP;
    const SEXP *p_x = STRING_PTR_RO(x);
    const SEXP *p_out = STRING_PTR_RO(out);
    for (int i = 0; i < n_groups; ++i){
      nfill = 0; // Reset fill limit
      FASTPLYR_GROUP_RESET
      last_obs = p_x[p_o[k] - 1];
      for (int j = 0; j < group_size; ++k, ++j){
        oi = p_o[k] - 1;
        if (p_x[oi] == NA_STRING && nfill < fill_limit){
          SET_STRING_ELT(out, oi, last_obs);
          ++nfill;
        } else {
          last_obs = p_out[oi];
          nfill = 0;
        }
      }
    }
    if (total_group_size != n){
      YIELD(NP);
      Rf_error("`sum(group_sizes)` must equal `length(x)`");
    }
    break;
  }
    // No NA to fill here
  case RAWSXP: {
    out = SHIELD(Rf_duplicate(x)); ++NP;
    break;

  }
  case VECSXP: {
    const SEXP *p_x = VECTOR_PTR_RO(x);
    out = SHIELD(new_vec(VECSXP, n)); ++NP;
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (int i = 0; i < n; ++i){
      SET_VECTOR_ELT(out, i, cpp_fill_grouped(p_x[i], order, group_sizes, fill_limit));
    }
    break;
  }
  default: {
    YIELD(NP);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  YIELD(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_df_transform_exotic(SEXP x, bool order, bool as_qg){
  if (!Rf_inherits(x, "data.frame")){
    Rf_error("x must be a data frame");
  }
  SEXP out = SHIELD(Rf_shallow_duplicate(x));
  for (int i = 0; i < Rf_length(x); ++i){
    if (cpp_is_exotic(VECTOR_ELT(x, i))){
     SET_VECTOR_ELT(out, i, fp_group_id(
         VECTOR_ELT(x, i), cpp11::named_arg("order") = order,
         cpp11::named_arg("as_qg") = as_qg
     ));
    }
  }
  YIELD(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_starts(SEXP group_id, int n_groups){

  int n = Rf_length(group_id);

  SEXP out = SHIELD(new_vec(INTSXP, n_groups));
  const int* p_group_id = INTEGER_RO(group_id);
  int* __restrict__ p_out = INTEGER(out);


  // Slower but easier to read alternative
  // std::fill(p_out, p_out + n_groups, 0);
  // for (int i = 0; i < n; ++i) {
  //   const int g = p_group_id[i] - 1;
  //   if (p_out[g] == 0){
  //     p_out[g] = i + 1;
  //   }
  // }


  int fill_value = std::numeric_limits<int>::max();

  if (n < fill_value){
    // Initialise start locations
    std::fill(p_out, p_out + n_groups, fill_value);
    for (int i = 0; i < n; ++i){
      p_out[p_group_id[i] - 1] = std::min(p_out[p_group_id[i] - 1], i + 1);
    }

    for (int i = 0; i < n_groups; ++i){
      if (p_out[i] == fill_value){
        p_out[i] = 0;
      }
    }
  } else {

    // Slightly slower method than above
    // this can handle the edge-case where a group's start location
    // happens to be at .Machine$integer.max

    // Initialise start locations
    std::fill(p_out, p_out + n_groups, 0);
    for (int i = 0; i < n; ++i){
      p_out[p_group_id[i] - 1] = static_cast<int>(
        std::min(
          static_cast<unsigned int>(p_out[p_group_id[i] - 1]) - 1,
          static_cast<unsigned int>(i)
        ) + 1
      );
    }
  }
  YIELD(1);
  return out;
}

// SEXP cpp_unique(SEXP x, SEXP group_id, int n_groups){
//
//   int n = Rf_length(group_id);
//
//   const int* p_group_id = INTEGER_RO(group_id);
//
//   SEXP out = SHIELD(new_vec(REALSXP, n_groups));
//
//   const double *p_x = REAL_RO(x);
//   double* __restrict__ p_out = REAL(out);
//
//   int g;
//
//   std::vector<uint8_t> seen(n_groups);
//   std::fill(seen.begin(), seen.begin() + n_groups, static_cast<uint8_t>(0));
//   std::vector<uint8_t> *p_seen = &seen;
//
//   for (int i = 0; i < n; ++i){
//     g = p_group_id[i] - 1;
//     if ((*p_seen)[g] == static_cast<uint8_t>(0)){
//       p_out[g] = p_x[i];
//       (*p_seen)[g] = static_cast<uint8_t>(1);
//     }
//   }
//   YIELD(1);
//   return out;
// }

[[cpp11::register]]
SEXP cpp_group_ends(SEXP group_id, int n_groups){

  int n = Rf_length(group_id);

  SEXP out = SHIELD(new_vec(INTSXP, n_groups));
  const int* p_group_id = INTEGER_RO(group_id);
  int* __restrict__ p_out = INTEGER(out);

  // Initialise start locations
  std::fill(p_out, p_out + n_groups, 0);
  for (int i = 0; i < n; ++i){
    p_out[p_group_id[i] - 1] = std::max(p_out[p_group_id[i] - 1], i + 1);
  }
  YIELD(1);
  return out;
}

[[cpp11::register]]
SEXP common_length(SEXP x){

  const SEXP *p_x = VECTOR_PTR_RO(x);
  R_xlen_t n = Rf_xlength(x);

  R_xlen_t out = 0;

  for (R_xlen_t i = 0; i < n; ++i){

    // Ignore NULL elements
    if (Rf_isNull(p_x[i])){
      continue;
    } else if (cheapr::vec_length(p_x[i]) == 0){
      return Rf_ScalarInteger(0);
    } else {
      out = std::max(out, cheapr::vec_length(p_x[i]));
    }
  }
  return out <= std::numeric_limits<int>::max() ? Rf_ScalarInteger(out) : Rf_ScalarReal(out);
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
