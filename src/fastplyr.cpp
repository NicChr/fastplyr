#include "fastplyr.h"

// bool r_doubles_equal(double x, double y){
//   return x == y;
// }

SEXP cpp_r_obj_address(SEXP x) {
  static char buf[1000];
  snprintf(buf, 1000, "%p", (void*) x);
  return Rf_mkChar(buf);
}

// Compare the addresses between 2 similar lists

[[cpp11::register]]
SEXP cpp_address_equal(SEXP x, SEXP y) {
  const SEXP* p_x = VECTOR_PTR_RO(x);
  const SEXP* p_y = VECTOR_PTR_RO(y);
  int n1 = Rf_length(x);
  int n2 = Rf_length(y);
  if (n1 != n2){
    Rf_error("x and y must be of the same length");
  }
  SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n1));
  int *p_out = LOGICAL(out);
  for (int i = 0; i < n1; ++i) {
    p_out[i] = (cpp_r_obj_address(p_x[i]) == cpp_r_obj_address(p_y[i]));
  }
  Rf_unprotect(1);
  return out;
}


// nrows/ncols of a list of data frames, typically supplied through ...

[[cpp11::register]]
SEXP cpp_nrows(SEXP x, bool check_rows_equal) {
  Rf_protect(x = Rf_coerceVector(x, VECSXP));
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  if (n < 2){
    for (int i = 0; i < n; ++i) {
      if (!Rf_isFrame(p_x[i])){
        Rf_unprotect(2);
        Rf_error("All inputs must be data frames");
      }
      p_out[i] = Rf_length(Rf_getAttrib(p_x[i], R_RowNamesSymbol));
    }
  } else {
    // First data frame
    if (!Rf_isFrame(p_x[0])){
      Rf_unprotect(2);
      Rf_error("All inputs must be data frames");
    }
    int nrows = Rf_length(Rf_getAttrib(p_x[0], R_RowNamesSymbol));
    p_out[0] = nrows;
    // All others
    for (int i = 1; i < n; ++i) {
      if (!Rf_isFrame(p_x[i])){
        Rf_unprotect(2);
        Rf_error("All inputs must be data frames");
      }
      p_out[i] = Rf_length(Rf_getAttrib(p_x[i], R_RowNamesSymbol));
      if (check_rows_equal && p_out[i] != nrows){
        Rf_unprotect(2);
        Rf_error("All input data frames must have the same number of rows");
      }
    }
  }
  Rf_unprotect(2);
  return out;
}

[[cpp11::register]]
SEXP cpp_ncols(SEXP x, bool check_cols_equal) {
  Rf_protect(x = Rf_coerceVector(x, VECSXP));
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  if (n < 2){
    for (int i = 0; i < n; ++i) {
      if (!Rf_isFrame(p_x[i])){
        Rf_unprotect(2);
        Rf_error("All inputs must be data frames");
      }
      p_out[i] = Rf_length(p_x[i]);
    }
  } else {
    // First data frame
    if (!Rf_isFrame(p_x[0])){
      Rf_unprotect(2);
      Rf_error("All inputs must be data frames");
    }
    int ncols = Rf_length(p_x[0]);
    p_out[0] = ncols;
    // All others
    for (int i = 1; i < n; ++i) {
      if (!Rf_isFrame(p_x[i])){
        Rf_unprotect(2);
        Rf_error("All inputs must be data frames");
      }
      p_out[i] = Rf_length(p_x[i]);
      if (check_cols_equal && p_out[i] != ncols){
        Rf_unprotect(2);
        Rf_error("All input data frames must have the same number of cols");
      }
    }
  }
  Rf_unprotect(2);
  return out;
}

[[cpp11::register]]
bool cpp_is_exotic(SEXP x){
  return Rf_isVectorList(x) || Rf_isS4(x) || !Rf_isVector(x);
}

// Specifically applied to a list of data frames, used in `f_bind_rows()`

[[cpp11::register]]
bool cpp_any_frames_exotic(SEXP x){
  bool out = false;
  int n_dots = Rf_length(x);
  for (int i = 0; i < n_dots; ++i){
    int ncol = Rf_length(VECTOR_ELT(x, i));
    for (int j = 0; j < ncol; ++j){
      if (cpp_is_exotic(VECTOR_ELT(VECTOR_ELT(x, i), j))){
        out = true;
        break;
      }
    }
  }
  return out;
}

// Fast extract 1 element from each list element

[[cpp11::register]]
SEXP cpp_list_subset(SEXP x, SEXP ptype, SEXP i, SEXP default_value) {
  Rf_protect(x = Rf_coerceVector(x, VECSXP));
  Rf_protect(i = Rf_coerceVector(i, INTSXP));
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);
  int i_n = Rf_length(i);
  int k;
  if (n == 0){
    Rf_unprotect(2);
    return ptype;
  }
  if (Rf_length(ptype) > 0){
    Rf_unprotect(2);
    Rf_error("ptype must be a zero-length vector");
  }
  if (!(i_n == 1 || (n > 0 && i_n == n))){
    Rf_unprotect(2);
    Rf_error("i must be an integer vector of length 1 or of length(x)");
  }
  int *p_i = INTEGER(i);
  switch (TYPEOF(ptype)){
  case LGLSXP: {
    bool replace = Rf_asLogical(default_value);
    SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n));
    int *p_out = LOGICAL(out);
    for (int j = 0; j < n; ++j) {
      p_out[j] = replace;
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        p_out[j] = LOGICAL(p_x[j])[k - 1];
      }
    }
    Rf_unprotect(3);
    return out;
  }
  case INTSXP: {
    int replace = Rf_asInteger(default_value);
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    int *p_out = INTEGER(out);
    for (int j = 0; j < n; ++j) {
      p_out[j] = replace;
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        p_out[j] = INTEGER(p_x[j])[k - 1];
      }
    }
    Rf_unprotect(3);
    return out;
  }
  case REALSXP: {
    double replace = Rf_asReal(default_value);
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
    double *p_out = REAL(out);
    for (int j = 0; j < n; ++j) {
      p_out[j] = replace;
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        p_out[j] = REAL(p_x[j])[k - 1];
      }
    }
    Rf_unprotect(3);
    return out;
  }
  case STRSXP: {
    SEXP replace = Rf_protect(Rf_asChar(default_value));
    SEXP out = Rf_protect(Rf_allocVector(STRSXP, n));
    for (int j = 0; j < n; ++j) {
      SET_STRING_ELT(out, j, replace);
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        SET_STRING_ELT(out, j, STRING_ELT(p_x[j], k - 1));
      }
    }
    Rf_unprotect(4);
    return out;
  }
  default: {
    Rf_unprotect(2);
    Rf_error("cpp_list_subset cannot handle supplied SEXP");
  }
  }
}

// Take a vector of group sizes (sorted by group)
// And this will return a vector of the start indices of each group (in sorted order)

[[cpp11::register]]
SEXP cpp_sorted_group_starts(SEXP group_sizes, int init_loc = 1){
  int *p_gsizes = INTEGER(group_sizes);
  int n = Rf_length(group_sizes);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  int init = init_loc;
  p_out[0] = init;
  // cumsum over group_sizes[-length(group_sizes)]
  for (int i = 0; i < (n - 1); ++i){
    init += p_gsizes[i];
    p_out[i + 1] = init;
  }
  Rf_unprotect(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_group_locs(SEXP order, SEXP group_sizes){
  unsigned int n = Rf_length(order);
  unsigned int n_groups = Rf_length(group_sizes);
  int *p_o = INTEGER(order);
  int *p_gs = INTEGER(group_sizes);
  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));
  unsigned int k = 0;
  unsigned int group_size = 0;
  for (unsigned int i = 0; i < n_groups; ++i){
    group_size = p_gs[i];
    if ( (k + group_size) > n){
      Rf_unprotect(1);
      Rf_error("group sizes must sum to length(order)");
    }
    SEXP group_loc = Rf_protect(Rf_allocVector(INTSXP, group_size));
    int* __restrict__ p_loc = INTEGER(group_loc);
    memcpy(p_loc, &p_o[k], sizeof(int) * group_size);
    k += group_size;
    // for (unsigned int j = 0; j < group_size; ++j){
    //   p_loc[j] = p_o[k++];
    // }
    SET_VECTOR_ELT(out, i, group_loc);
    Rf_unprotect(1);
  }
  Rf_unprotect(1);
  return out;
}
[[cpp11::register]]
SEXP cpp_row_id(SEXP order, SEXP group_sizes, bool ascending){
  int n = Rf_length(order);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  int *p_o = INTEGER(order);
  int *p_group_sizes = INTEGER(group_sizes);
  int j = 0;
  int running_group_size;
  if (Rf_length(group_sizes) == 0){
    running_group_size = n;
  } else {
    running_group_size = p_group_sizes[0];
  }
  if (ascending){
    int init = 0;
    for (int i = 0; i < n; ++i){
      if (i >= running_group_size){
        init = 0;
        running_group_size += p_group_sizes[++j];
      }
      p_out[p_o[i] - 1] = ++init;
    }
  } else {
    int init = running_group_size + 1;
    for (int i = 0; i < n; ++i){
      if (i >= running_group_size){
        init = p_group_sizes[++j] + 1;
        running_group_size += p_group_sizes[j];
      }
      p_out[p_o[i] - 1] = --init;
    }
  }
  Rf_unprotect(1);
  return out;
}

// Takes a data frame of logical vectors
// And reduces them to a single logical vector
// Applying the && (AND) condition for each row
// Then uses the cheapr::cpp_which algorithm
// To find which rows are TRUE

[[cpp11::register]]
SEXP cpp_which_all(SEXP x){
  if (!Rf_inherits(x, "data.frame")){
    Rf_error("x must be a data frame");
  }
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int NP = 0;
  int n_true = 0;

  unsigned int ncols = Rf_length(x); // ncols
  unsigned int nrows = Rf_length(Rf_getAttrib(x, R_RowNamesSymbol));

  SEXP out;
  if (ncols == 0){
    out = Rf_protect(Rf_allocVector(INTSXP, 0)); ++NP;
  } else if (ncols == 1){
    cpp11::function cheapr_which = cpp11::package("cheapr")["which_"];
    out = Rf_protect(cheapr_which(p_x[0])); ++NP;
  } else {
    SEXP lgl = Rf_protect(Rf_allocVector(LGLSXP, nrows)); ++NP;
    int *p_lgl = LOGICAL(lgl);

    SEXP first_lgl = Rf_protect(p_x[0]); ++NP;
    int *p_first = LOGICAL(first_lgl);
    memmove(p_lgl, &p_first[0], sizeof(int) * nrows);

    // Starting from 2nd col to 2nd last col
    for (unsigned int i = 1; i < (ncols - 1); ++i) {
      int *p_temp = LOGICAL(p_x[i]);
      for (unsigned int j = 0; j < nrows; ++j){
        p_lgl[j] = p_lgl[j] == TRUE && p_temp[j] == TRUE;
      }
    }
    // Last col
    // This is where we count how many true vals are returned in the final vector
    int *p_temp = LOGICAL(p_x[ncols - 1]);
    for (unsigned int j = 0; j < nrows; ++j){
      p_lgl[j] = (p_lgl[j] == TRUE) && (p_temp[j] == TRUE);
      n_true += p_lgl[j];
    }
    // WHICH algo
    out = Rf_protect(Rf_allocVector(INTSXP, n_true)); ++NP;
    int *p_out = INTEGER(out);
    int whichi = 0;
    int i = 0;
    while (whichi < n_true){
      p_out[whichi] = i + 1;
      whichi += (p_lgl[i++] == TRUE);
    }
  }
  Rf_unprotect(NP);
  return out;
}

// Alternative that goes row-wise through x
// SEXP cpp_which_all(SEXP x){
//   if (!Rf_inherits(x, "data.frame")){
//     Rf_error("x must be a data frame");
//   }
//   const SEXP *p_x = VECTOR_PTR_RO(x);
//   int NP = 0;
//   int n_true = 0;
//   int n_cols = Rf_length(x); // ncols
//   int n_rows = Rf_length(Rf_getAttrib(x, R_RowNamesSymbol));
//
//   SEXP out;
//
//   if (n_cols == 0){
//     out = Rf_protect(Rf_allocVector(INTSXP, 0)); ++NP;
//   } else if (n_cols == 1){
//     cpp11::function cheapr_which = cpp11::package("cheapr")["which_"];
//     out = Rf_protect(cheapr_which(p_x[0])); ++NP;
//   } else {
//     SEXP lgl = Rf_protect(Rf_allocVector(LGLSXP, n_rows)); ++NP;
//     int *p_lgl = LOGICAL(lgl);
//     memset(p_lgl, 0, n_rows * sizeof(int));
//
//     bool is_true = false;
//     for (int i = 0; i < n_rows; ++i){
//       is_true = true;
//       int j = n_cols - 1;
//       while (j >= 0 && is_true){
//         is_true = LOGICAL(p_x[j--])[i] == TRUE;
//       }
//       n_true += is_true;
//       p_lgl[i] = is_true;
//     }
//     out = Rf_protect(Rf_allocVector(INTSXP, n_true)); ++NP;
//     int *p_out = INTEGER(out);
//     int whichi = 0;
//     int i = 0;
//     while (whichi < n_true){
//       p_out[whichi] = i + 1;
//       whichi += (p_lgl[i++] == TRUE);
//     }
//   }
//   Rf_unprotect(NP);
//   return out;
// }

// Taken from dplyr::group_indices,
// All credits go to dplyr

[[cpp11::register]]
SEXP cpp_df_group_indices(SEXP rows, int size) {
  SEXP indices = Rf_protect(Rf_allocVector(INTSXP, size));
  int *p_indices = INTEGER(indices);
  int ng = Rf_length(rows);
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  for (int i = 0; i < ng; ++i) {
    SEXP rows_i = p_rows[i];
    int n_i = Rf_length(rows_i);
    int *p_rows_i = INTEGER(rows_i);
    for (int j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }
  Rf_unprotect(1);
  return indices;
}


// Slice integers (only in-bounds data is returned)
// indices must NOT INCLUDE NA values

SEXP cpp_int_slice(SEXP x, SEXP indices, bool check){
  if (!Rf_isInteger(x)){
    Rf_error("x must be an integer vector");
  }
  if (!Rf_isInteger(indices)){
    Rf_error("indices must be an integer vector");
  }
  int *pi = INTEGER(indices);
  int xn = Rf_length(x);
  int n = Rf_length(indices);
  int NP = 0;
  int zero_count = 0;
  int pos_count = 0;
  int oob_count = 0;
  int neg_count = 0;
  int k = 0;
  int out_size;
  if (check){
    for (int j = 0; j < n; ++j){
      zero_count += (pi[j] == 0);
      pos_count += (pi[j] > 0);
      oob_count += (std::abs(pi[j]) > xn);
    }
    neg_count = n - pos_count - zero_count;
    if ( pos_count > 0 && neg_count > 0){
      Rf_error("Cannot mix positive and negative indices");
    }
  }
  out_size = n - oob_count - zero_count;
  bool no_check = !check || (zero_count == 0 && oob_count == 0 && pos_count == n ) || neg_count > 0;

  SEXP temp = neg_count > 0 ? Rf_protect(cpp11::package("cheapr")["neg_indices_to_pos"](indices, xn)) : Rf_protect(indices); ++NP;
  int *pi2 = INTEGER(temp);
  if (neg_count > 0){
    n = Rf_length(temp);
    out_size = n;
  }
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size)); ++NP;
  int *p_x = INTEGER(x);
  int *p_out = INTEGER(out);
  if (no_check){
    for (int i = 0; i < n; ++i) p_out[k++] = p_x[pi2[i] - 1];
  } else {
    unsigned int rng = xn - 1;
    int xi = 0;
    for (int i = 0; i < n; ++i){
      xi = pi2[i];
      if ((unsigned)(xi - 1) <= rng) p_out[k++] = p_x[xi - 1];
      // if (xi > 0 && xi <= xn) p_out[k++] = p_x[xi - 1];
    }
  }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_slice_locs(SEXP group_locs, SEXP locs){
  int n_groups = Rf_length(group_locs);

  const SEXP *p_groups = VECTOR_PTR_RO(group_locs);

  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n_groups));

  for (int i = 0; i < n_groups; ++i){
    SET_VECTOR_ELT(out, i, cpp_int_slice(p_groups[i], locs, true));
  }
  Rf_unprotect(1);
  return out;
}

bool is_compact_seq(SEXP x){
  if (!ALTREP(x)) return false;
  SEXP alt_attribs = Rf_protect(Rf_coerceVector(ATTRIB(ALTREP_CLASS(x)), VECSXP));
  SEXP alt_class_nm = Rf_protect(STRING_ELT(Rf_coerceVector(VECTOR_ELT(alt_attribs, 0), STRSXP), 0));
  SEXP alt_pkg_nm = Rf_protect(STRING_ELT(Rf_coerceVector(VECTOR_ELT(alt_attribs, 1), STRSXP), 0));
  SEXP intseq_char = Rf_protect(Rf_mkChar("compact_intseq"));
  SEXP realseq_char = Rf_protect(Rf_mkChar("compact_realseq"));
  SEXP base_char = Rf_protect(Rf_mkChar("base"));
  bool out = (alt_class_nm == intseq_char ||
              alt_class_nm == realseq_char) &&
              alt_pkg_nm == base_char;
  Rf_unprotect(6);
  return out;
}

[[cpp11::register]]
SEXP cpp_run_id(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  if (is_compact_seq(x)){
    return cpp11::package("base")[":"](1, n);
  }
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);

#define FP_RUN_ID for (R_xlen_t i = 1; i < n; ++i) p_out[i] = p_out[i - 1] + (p_x[i] != p_x[i - 1]);

  if (n >= 1){
    p_out[0] = 1;
  }

  switch (TYPEOF(x)){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    FP_RUN_ID;
    break;
  }
  case REALSXP: {
    long long *p_x = (long long*) REAL(x);
    FP_RUN_ID;
    break;
  }
  case STRSXP: {
    const SEXP *p_x = STRING_PTR_RO(x);
    FP_RUN_ID;
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    long long x1_re = 0, x2_re = 0, x1_im = 0, x2_im = 0;
    for (R_xlen_t i = 1; i < n; ++i){
      x1_re = p_x[i - 1].r;
      x2_re = p_x[i].r;
      x1_im = p_x[i - 1].i;
      x2_im = p_x[i].i;
      p_out[i] = x1_re == x2_re && x1_im == x2_im ? p_out[i - 1] : p_out[i - 1] + 1;
    }
    break;
  }
  case RAWSXP: {
  Rbyte *p_x = RAW(x);
    for (R_xlen_t i = 1; i < n; ++i){
      p_out[i] = memcmp(&p_x[i - 1], &p_x[i], sizeof(Rbyte)) == 0 ?
      p_out[i - 1] : p_out[i - 1] + 1;
    }
   break;
  }
  default: {
    Rf_unprotect(1);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  Rf_unprotect(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_df_run_id(cpp11::writable::list x){
  int NP = 0;
  int n_cols = Rf_length(x);
  int n_rows = Rf_length(Rf_getAttrib(x, R_RowNamesSymbol));

  if (n_cols == 1){
    SEXP x1 = Rf_protect(VECTOR_ELT(x, 0)); ++NP;
    SEXP out = Rf_protect(cpp_run_id(x1)); ++NP;
    Rf_unprotect(NP);
    return out;
  }

  cpp11::function fastplyr_group_id = cpp11::package("fastplyr")["group_id"];

  const SEXP *p_x = VECTOR_PTR_RO(x);

  for (int l = n_cols - 1; l >= 0; --l){
    if (is_compact_seq(p_x[l])){
      SEXP compact_seq = Rf_protect(p_x[l]); ++NP;
      SEXP out = Rf_protect(cpp_run_id(compact_seq)); ++NP;
      Rf_unprotect(NP);
      return out;
    }
    if (cpp_is_exotic(p_x[l])){
      cpp11::integers group_ids = Rf_protect(fastplyr_group_id(p_x[l], cpp11::named_arg("order") = false));
      x[l] = group_ids;
      Rf_unprotect(1);
    }
  }

  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n_rows)); ++NP;
  int *p_out = INTEGER(out);

  if (n_cols < 1){
    for (int i = 0; i < n_rows; ++i) p_out[i] = 1;
    Rf_unprotect(NP);
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
        int *p_xj = INTEGER(p_x[j]);
          diff = (p_xj[i] != p_xj[i - 1]);
          p_out[i] = (k += diff);
          break;
        }
      case REALSXP: {
        long long *p_xj = (long long*) REAL(p_x[j]);
        diff = (p_xj[i] != p_xj[i - 1]);
        p_out[i] = (k += diff);
        break;
      }
      case STRSXP: {
        const SEXP *p_xj = STRING_PTR_RO(p_x[j]);
        diff = (p_xj[i] != p_xj[i - 1]);
        p_out[i] = (k += diff);
        break;
      }
      case CPLXSXP: {
        Rcomplex *p_xj = COMPLEX(p_x[j]);
        long long x1_re = p_xj[i - 1].r;
        long long x2_re = p_xj[i].r;
        long long x1_im = p_xj[i - 1].i;
        long long x2_im = p_xj[i].i;
        diff = (x1_re != x2_re) || (x1_im != x2_im);
        p_out[i] = (k += diff);
        break;
      }
      case RAWSXP: {
        Rbyte *p_xj = RAW(p_x[j]);
        diff = memcmp(&p_xj[i], &p_xj[i - 1], sizeof(Rbyte)) != 0;
        p_out[i] = (k += diff);
       break;
      }
      default: {
        Rf_unprotect(NP);
        Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(p_x[j])));
      }
      }
      ++j;
    }
  }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_consecutive_id(SEXP x){
  if (Rf_inherits(x, "data.frame")){
    return cpp_df_run_id(x);
  } else if (cpp_is_exotic(x)){
    cpp11::function fastplyr_group_id = cpp11::package("fastplyr")["group_id"];
    SEXP group_ids = Rf_protect(fastplyr_group_id(x, cpp11::named_arg("order") = false));
    SEXP out = Rf_protect(cpp_run_id(group_ids));
    Rf_unprotect(2);
    return out;
  } else {
    return cpp_run_id(x);
  }
}
