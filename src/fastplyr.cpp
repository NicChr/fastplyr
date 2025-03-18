#include "fastplyr.h"
#include "cheapr_api.h"

// Basically R's get()

// SEXP cpp_get(SEXP sym, SEXP rho){
//   Rf_protect(sym = Rf_coerceVector(sym, SYMSXP));
//
//   if (TYPEOF(rho) != ENVSXP)
//     Rf_error("second argument to '%s' must be an environment", __func__);
//
//   SEXP val = Rf_findVar(sym, rho);
//   if (val == R_MissingArg){
//     Rf_unprotect(1);
//     Rf_error("arg `sym` cannot be missing");
//   } else if (val == R_UnboundValue){
//     Rf_unprotect(1);
//     return R_NilValue;
//   }
//   else if (TYPEOF(val) == PROMSXP){
//     Rf_protect(val);
//     val = Rf_eval(val, rho);
//     Rf_unprotect(1);
//   }
//   Rf_unprotect(1);
//   return val;
// }
// SEXP cpp_fun_ns(SEXP x, SEXP rho){
//   int NP = 0;
//   if (!Rf_isFunction(x)){
//     Rf_protect(x = cpp_get(x, rho)); ++NP;
//   }
//   if (TYPEOF(x) != CLOSXP){
//     Rf_unprotect(NP); return Rf_mkString("");
//   }
//   // SEXP env = Rf_protect(cpp11::package("base")["environment"](x)); ++NP;
//   SEXP env = Rf_protect(CLOENV(x)); ++NP;
//   if (Rf_isNull(x) || Rf_isNull(env)){
//     Rf_unprotect(NP); return Rf_mkString("");
//   } else if (env == R_BaseNamespace){
//     Rf_unprotect(NP); return Rf_mkString("base");
//   } else if (R_IsNamespaceEnv(env)) {
//     SEXP ns_name = Rf_protect(R_NamespaceEnvSpec(env)); ++NP;
//     SEXP names = Rf_protect(Rf_getAttrib(ns_name, R_NamesSymbol)); ++NP;
//     SEXP name = Rf_protect(Rf_mkString("name")); ++NP;
//     SEXP name_loc = Rf_protect(Rf_match(names, name, NA_INTEGER)); ++NP;
//
//     if (TYPEOF(ns_name) == STRSXP &&
//         Rf_length(name) != 0 &&
//         INTEGER(name_loc)[0] != NA_INTEGER){
//       SEXP result = Rf_protect(STRING_ELT(ns_name, INTEGER(name_loc)[0] - 1)); ++NP;
//       Rf_protect(result = Rf_ScalarString(result)); ++NP;
//       Rf_unprotect(NP);
//       return result;
//     }
//     Rf_unprotect(NP); return Rf_mkString("");
//   } else {
//     Rf_unprotect(NP); return Rf_mkString("");
//   }
// }

// bool cpp_call_contains_ns(SEXP expr, SEXP ns, SEXP rho){
//   if (TYPEOF(expr) != LANGSXP){
//     return false;
//   }
//   int NP = 0;
//   // if (rlang_is_call(expr, cpp11::named_arg("ns") = ns)){
//   //   return true;
//   // }
//   bool out = false;
//   Rf_error("error");
//   SEXP tree = Rf_protect(base_as_list(expr)); ++NP;
//   SEXP branch;
//   for (int i = 0; i < Rf_length(tree); ++i){
//     branch = Rf_protect(VECTOR_ELT(expr, i)); ++NP;
//     if (TYPEOF(branch) == LANGSXP){
//       return cpp_call_contains_ns(branch, ns, rho);
//     }
//     if (TYPEOF(branch) == SYMSXP &&
//         cpp_fun_ns(rlang::sym_as_character(branch), rho) == ns){
//       out = true;
//       break;
//     }
//   }
//   Rf_unprotect(NP);
//   return out;
// }

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
  SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n1));
  int *p_out = LOGICAL(out);
  for (int i = 0; i < n1; ++i) {
    p_out[i] = (cheapr::r_address(p_x[i]) == cheapr::r_address(p_y[i]));
  }
  Rf_unprotect(1);
  return out;
}


// nrows/ncols of a list of data frames, typically supplied through ...

[[cpp11::register]]
SEXP cpp_frame_dims(SEXP x, bool check_rows_equal, bool check_cols_equal) {
  Rf_protect(x = Rf_coerceVector(x, VECSXP));
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);
  SEXP nrows = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_nrows = INTEGER(nrows);
  SEXP ncols = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_ncols = INTEGER(ncols);
  if (n < 2){
    for (int i = 0; i < n; ++i) {
      if (!Rf_inherits(p_x[i], "data.frame")){
        Rf_unprotect(3);
        Rf_error("All inputs must be data frames");
      }
      p_nrows[i] = Rf_length(Rf_getAttrib(p_x[i], R_RowNamesSymbol));
      p_ncols[i] = Rf_length(p_x[i]);
    }
  } else {

    // First data frame

    if (!Rf_inherits(p_x[0], "data.frame")){
      Rf_unprotect(3);
      Rf_error("All inputs must be data frames");
    }
    int n_rows = Rf_length(Rf_getAttrib(p_x[0], R_RowNamesSymbol));
    int n_cols = Rf_length(p_x[0]);
    p_nrows[0] = n_rows;
    p_ncols[0] = n_cols;

    // All others

    for (int i = 1; i < n; ++i) {
      if (!Rf_inherits(p_x[i], "data.frame")){
        Rf_unprotect(3);
        Rf_error("All inputs must be data frames");
      }
      p_nrows[i] = Rf_length(Rf_getAttrib(p_x[i], R_RowNamesSymbol));
      p_ncols[i] = Rf_length(p_x[i]);
      if (check_rows_equal && p_nrows[i] != n_rows){
        Rf_unprotect(3);
        Rf_error("All input data frames must have the same number of rows");
      }
      if (check_cols_equal && p_ncols[i] != n_cols){
        Rf_unprotect(3);
        Rf_error("All input data frames must have the same number of cols");
      }
    }
  }
  SEXP out = Rf_protect(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, nrows);
  SET_VECTOR_ELT(out, 1, ncols);
  Rf_unprotect(4);
  return out;
}

[[cpp11::register]]
bool cpp_is_exotic(SEXP x){
  // integer64 needs to be turned into proxy group IDs in this package
  return !Rf_isNull(x) && (!Rf_isVectorAtomic(x) || Rf_isS4(x) || Rf_inherits(x, "integer64"));
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
  if (n > 0){
    int init = init_loc;
    p_out[0] = init;
    // cumsum over group_sizes[-length(group_sizes)]
    for (int i = 0; i < (n - 1); ++i){
      p_out[i + 1] = (init += p_gsizes[i]);
    }
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

// Alternative that goes col-wise

// SEXP cpp_which_all_alt(SEXP x){
//   if (!Rf_inherits(x, "data.frame")){
//     Rf_error("x must be a data frame");
//   }
//   const SEXP *p_x = VECTOR_PTR_RO(x);
//   int NP = 0, n_true = 0;
//
//   int n_cols = Rf_length(x);
//   int n_rows = Rf_length(Rf_getAttrib(x, R_RowNamesSymbol));
//
//   SEXP out; // Initialise result (must be assigned a valid SEXP or R crashes)
//
//   if (n_cols == 0){
//     out = Rf_protect(Rf_allocVector(INTSXP, 0)); ++NP;
//   } else if (n_cols == 1){
//     cpp11::function cheapr_which = cpp11::package("cheapr")["which_"];
//     out = Rf_protect(cheapr_which(p_x[0])); ++NP;
//   } else {
//     SEXP lgl = Rf_protect(Rf_allocVector(LGLSXP, n_rows)); ++NP;
//     int *p_lgl = LOGICAL(lgl);
//
//     // Copy values of x[[1]] into our vector
//
//     SEXP first_lgl = Rf_protect(p_x[0]); ++NP;
//     int *p_first = LOGICAL(first_lgl);
//     memmove(p_lgl, &p_first[0], sizeof(int) * n_rows);
//
//     // Starting from 2nd col to 2nd last col
//
//     for (int i = 1; i < (n_cols - 1); ++i) {
//       int *p_temp = LOGICAL(p_x[i]);
//       for (int j = 0; j < n_rows; ++j){
//         p_lgl[j] = p_lgl[j] == TRUE && p_temp[j] == TRUE;
//       }
//     }
//
//     // Last col
//     // This is where we count how many true values
//     // are returned in the final vector
//
//     int *p_temp = LOGICAL(p_x[n_cols - 1]);
//     for (int j = 0; j < n_rows; ++j){
//       p_lgl[j] = (p_lgl[j] == TRUE) && (p_temp[j] == TRUE);
//       n_true += p_lgl[j];
//     }
//
//     // WHICH algo
//
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

// Alternative that goes row-wise through x
// When data is mixed it tends to be a bit faster

[[cpp11::register]]
SEXP cpp_which_all(SEXP x){
  if (!Rf_inherits(x, "data.frame")){
    Rf_error("x must be a data frame");
  }
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int NP = 0;
  int n_true = 0;
  int n_cols = Rf_length(x);
  int n_rows = Rf_length(Rf_getAttrib(x, R_RowNamesSymbol));

  SEXP out;

  if (n_cols == 0){
    out = Rf_protect(Rf_allocVector(INTSXP, 0)); ++NP;
  } else if (n_cols == 1){
    cpp11::function cheapr_which = cpp11::package("cheapr")["which_"];
    out = Rf_protect(cheapr_which(p_x[0])); ++NP;
  } else {
    SEXP lgl = Rf_protect(Rf_allocVector(LGLSXP, n_rows)); ++NP;
    int *p_lgl = LOGICAL(lgl);
    memset(p_lgl, 0, n_rows * sizeof(int));

    bool is_true = false;
    for (int i = 0; i < n_rows; ++i){
      is_true = true;
      int j = 0;
      while (j < n_cols && is_true){
        is_true = LOGICAL(p_x[j++])[i] == TRUE;
      }
      n_true += is_true;
      p_lgl[i] = is_true;
    }
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

  SEXP temp = Rf_protect(neg_count > 0 ? cheapr::exclude_locs(indices, xn) : indices); ++NP;
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
      if ( ( (unsigned)(xi - 1) ) <= rng) p_out[k++] = p_x[xi - 1];
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

SEXP cpp_run_id(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  if (cheapr::is_compact_seq(x)){
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
    if (Rf_inherits(x, "integer64")){
    long long *p_x = INTEGER64_PTR(x);
    FP_RUN_ID;
  } else {
    // The above statement works almost always for
    // doubles, except for +0.0 == -0.0
    bool diff;
    double *p_x = REAL(x);
    for (R_xlen_t i = 1; i < n; ++i){
      diff = !(
        ((p_x[i] != p_x[i]) && (p_x[i - 1] != p_x[i - 1])) ||
          (p_x[i] == p_x[i - 1])
      );
      p_out[i] = p_out[i - 1] + diff;
    }
  }
    break;
  }
  case STRSXP: {
    const SEXP *p_x = STRING_PTR_RO(x);
    FP_RUN_ID;
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    for (R_xlen_t i = 1; i < n; ++i){
      p_out[i] = memcmp(&p_x[i - 1], &p_x[i], sizeof(Rcomplex)) == 0 ?
      p_out[i - 1] : p_out[i - 1] + 1;
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

SEXP cpp_df_run_id(cpp11::writable::list x){
  int NP = 0;
  int n_cols = Rf_length(x);
  int n_rows = Rf_length(Rf_getAttrib(x, R_RowNamesSymbol));

  const SEXP *p_x = VECTOR_PTR_RO(x);

  for (int l = n_cols - 1; l >= 0; --l){
    if (cheapr::is_compact_seq(p_x[l])){
      SEXP compact_seq = Rf_protect(p_x[l]); ++NP;
      SEXP out = Rf_protect(cpp_run_id(compact_seq)); ++NP;
      Rf_unprotect(NP);
      return out;
    }
    if (cpp_is_exotic(p_x[l])){
      SEXP group_ids = Rf_protect(fp_group_id(p_x[l], cpp11::named_arg("order") = false));
      x[l] = group_ids;
      Rf_unprotect(1);
    }
  }

  if (n_cols == 1){
    SEXP x1 = Rf_protect(VECTOR_ELT(x, 0)); ++NP;
    SEXP out = Rf_protect(cpp_run_id(x1)); ++NP;
    Rf_unprotect(NP);
    return out;
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
        if (Rf_inherits(x, "integer64")){
        long long *p_xj = INTEGER64_PTR(p_x[j]);
        diff = (p_xj[i] != p_xj[i - 1]);
        p_out[i] = (k += diff);
      } else {
        double *p_xj = REAL(p_x[j]);
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
        Rcomplex *p_xj = COMPLEX(p_x[j]);
        diff = memcmp(&p_xj[i], &p_xj[i - 1], sizeof(Rcomplex)) != 0;
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

#define FASTPLYR_GROUP_RESET                                   \
                                                               \
group_size = p_group_sizes[i];                                 \
total_group_size += group_size;                                \
if (total_group_size > n){                                     \
  Rf_unprotect(NP);                                            \
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
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  int n_groups = Rf_length(group_sizes);
  int k = 0;
  int oi, oi2, group_size;
  int total_group_size = 0;
  for (int i = 0; i < n_groups; ++i){
    group_size = p_group_sizes[i];
    total_group_size += group_size;
    if (total_group_size > n){
      Rf_unprotect(1);
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
    Rf_unprotect(1);
    Rf_error("sum(group_sizes) must equal length(x)");
  }
  Rf_unprotect(1);
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
  int NP = 0;
  int oi;
  int group_size, nfill;
  int total_group_size = 0;
  int k = 0;

  SEXP out;

  switch(TYPEOF(x)){
  case NILSXP: {
    out = Rf_protect(R_NilValue); ++NP;
    break;
  }
  case LGLSXP:
  case INTSXP: {
    if (o_size != n){
    Rf_unprotect(NP);
    Rf_error("x and order must both be the same length");
  }
    int last_obs;
    out = Rf_protect(Rf_duplicate(x)); ++NP;
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
      Rf_unprotect(NP);
      Rf_error("`sum(group_sizes)` must equal `length(x)`");
    }
    break;
  }
  case REALSXP: {
    if (Rf_inherits(x, "integer64")){
    if (o_size != n){
      Rf_unprotect(NP);
      Rf_error("x and order must both be the same length");
    }
    long long int last_obs;
    out = Rf_protect(Rf_duplicate(x)); ++NP;
    long long int *p_x = INTEGER64_PTR(x);
    long long int *p_out = INTEGER64_PTR(out);
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
      Rf_unprotect(NP);
      Rf_error("x and order must both be the same length");
    }
    double last_obs;
    out = Rf_protect(Rf_duplicate(x)); ++NP;
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
    Rf_unprotect(NP);
    Rf_error("`sum(group_sizes)` must equal `length(x)`");
  }
  break;
  }
  case STRSXP: {
    if (o_size != n){
    Rf_unprotect(NP);
    Rf_error("x and order must both be the same length");
  }
    SEXP last_obs;
    out = Rf_protect(Rf_duplicate(x)); ++NP;
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
      Rf_unprotect(NP);
      Rf_error("`sum(group_sizes)` must equal `length(x)`");
    }
    break;
  }
    // No NA to fill here
  case RAWSXP: {
    out = Rf_protect(Rf_duplicate(x)); ++NP;
    break;

  }
  case VECSXP: {
    const SEXP *p_x = VECTOR_PTR_RO(x);
    out = Rf_protect(Rf_allocVector(VECSXP, n)); ++NP;
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (int i = 0; i < n; ++i){
      SET_VECTOR_ELT(out, i, cpp_fill_grouped(p_x[i], order, group_sizes, fill_limit));
    }
    break;
  }
  default: {
    Rf_unprotect(NP);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  Rf_unprotect(NP);
  return out;
}

// unlist `group_data(data)$.rows` quickly

[[cpp11::register]]
SEXP cpp_unlist_group_locs(SEXP x, SEXP group_sizes){
  if (!Rf_isVectorList(x)){
   return x;
  }
  int n = Rf_length(x);
  int m, k = 0,  out_size = 0;
  const SEXP *p_x = VECTOR_PTR_RO(x);

  if (Rf_isNull(group_sizes)){
    // Figure out unlisted length
    for (int i = 0; i < n; ++i) out_size += Rf_length(p_x[i]);

    SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
    int *p_out = INTEGER(out);

    for (int i = 0; i < n; k += m, ++i){
      int *p_int = INTEGER(p_x[i]);
      m = Rf_length(p_x[i]);
      memcpy(&p_out[k], &p_int[0], m * sizeof(int));
    }
    Rf_unprotect(1);
    return out;
  } else {
    if (Rf_length(group_sizes) != n){
     Rf_error("`length(x)` must match `length(group_sizes)`");
    }
    int *p_gs = INTEGER(group_sizes);
    // Figure out unlisted length
    for (int i = 0; i < n; ++i) out_size += p_gs[i];

    SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
    int *p_out = INTEGER(out);

    for (int i = 0; i < n; k += m, ++i){
      int *p_int = INTEGER(p_x[i]);
      m = p_gs[i];
      memcpy(&p_out[k], &p_int[0], m * sizeof(int));
    }
    Rf_unprotect(1);
    return out;
  }
}

[[cpp11::register]]
SEXP cpp_reconstruct(SEXP data, SEXP from, bool keep_attrs){
  if (!Rf_inherits(data, "data.frame")){
    Rf_error("`data` must be a `data.frame`");
  }
  if (!Rf_inherits(from, "data.frame")){
    Rf_error("`from` must be a `data.frame`");
  }

  // Create shallow copies so that we can manipulate attributes freely

  SEXP target = Rf_protect(cheapr::shallow_copy(data));
  SEXP source = Rf_protect(cheapr::shallow_copy(from));

  // The below strips any leftover attributes from `data`,
  // I wonder if these should be kept

  cheapr::set_rm_attrs(target);

  if (keep_attrs){
    Rf_setAttrib(source, R_NamesSymbol, R_NilValue);
    Rf_setAttrib(source, R_ClassSymbol, R_NilValue);
    Rf_setAttrib(source, R_RowNamesSymbol, R_NilValue);
    SHALLOW_DUPLICATE_ATTRIB(target, source);
  }

  // Re-add original data attributes as these cannot be changed
  Rf_setAttrib(target, R_NamesSymbol, Rf_getAttrib(data, R_NamesSymbol));
  Rf_setAttrib(target, R_ClassSymbol, Rf_getAttrib(from, R_ClassSymbol));
  Rf_setAttrib(target, R_RowNamesSymbol, cheapr::create_df_row_names(Rf_length(Rf_getAttrib(data, R_RowNamesSymbol))));
  Rf_unprotect(2);
  return target;
}

[[cpp11::register]]
SEXP cpp_df_transform_exotic(SEXP x, bool order, bool as_qg){
  if (!Rf_inherits(x, "data.frame")){
    Rf_error("x must be a data frame");
  }
  SEXP out = Rf_protect(cheapr::shallow_copy(x));
  for (int i = 0; i < Rf_length(x); ++i){
    if (cpp_is_exotic(VECTOR_ELT(x, i))){
     SET_VECTOR_ELT(out, i, fp_group_id(
         VECTOR_ELT(x, i), cpp11::named_arg("order") = order,
         cpp11::named_arg("as_qg") = as_qg
     ));
    }
  }
  Rf_unprotect(1);
  return out;
}


// Low-level add cols to data frame
// SEXP cpp_df_add_cols(SEXP x, SEXP cols) {
//   if (TYPEOF(cols) != VECSXP){
//     Rf_error("cols must be a list");
//   }
//   R_xlen_t ncol = Rf_xlength(x);
//   R_xlen_t out_size = ncol + Rf_xlength(cols);
//   if (Rf_length(cols) == 0){
//     SEXP out = Rf_protect(Rf_allocVector(VECSXP, Rf_xlength(x)));
//     for (R_xlen_t i = 0; i < ncol; ++i){
//       SET_VECTOR_ELT(out, i, VECTOR_ELT(x, i));
//     }
//     Rf_unprotect(1);
//     return out;
//   }
//   SEXP col_names = Rf_protect(Rf_getAttrib(cols, R_NamesSymbol));
//   if (Rf_isNull(col_names)){
//     Rf_unprotect(2);
//     Rf_error("cols must be a named list");
//   }
//   SEXP names = Rf_protect(Rf_getAttrib(x, R_NamesSymbol));
//   SEXP out = Rf_protect(Rf_allocVector(VECSXP, out_size));
//   SEXP out_names = Rf_protect(Rf_allocVector(STRSXP, out_size));
//   for (R_xlen_t i = 0; i < ncol; ++i){
//     SET_VECTOR_ELT(out, i, VECTOR_ELT(x, i));
//     SET_STRING_ELT(out_names, i, STRING_ELT(names, i));
//   }
//   for (R_xlen_t i = ncol, j = 0; i < out_size; ++i, ++j){
//     SET_VECTOR_ELT(out, i, VECTOR_ELT(cols, j));
//     SET_STRING_ELT(out_names, i, STRING_ELT(col_names, j));
//   }
//   SHALLOW_DUPLICATE_ATTRIB(out, x);
//   Rf_setAttrib(out, R_NamesSymbol, out_names);
//   Rf_unprotect(4);
//   return out;
// }
