#include "fastplyr.h"
#include "cheapr_api.h"
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

  // SEXP r_int_ptrs = SHIELD(get_loc_ptrs(x)); ++NP;

  // if (Rf_isNull(r_int_ptrs)){
    for (int j = 0; j < n; ++j) {
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        p_out[j] = INTEGER_RO(p_x[j])[k - 1];
      } else {
        p_out[j] = replace;
      }
    }
  // } else {
  //   auto& int_ptrs_vec = *static_cast<std::vector<int*>*>(R_ExternalPtrAddr(r_int_ptrs));
  //   for (int j = 0; j < n; ++j) {
  //     k = (i_n == 1 ? p_i[0] : p_i[j]);
  //     if (k <= Rf_length(p_x[j]) && k > 0){
  //       p_out[j] = int_ptrs_vec[j][k - 1];
  //     } else {
  //       p_out[j] = replace;
  //     }
  //   }
  // }

  YIELD(NP);
  return out;
}

// Take a vector of group sizes (sorted by group)
// And this will return a vector of the start indices of each group (in sorted order)

[[cpp11::register]]
SEXP cpp_sorted_group_starts(SEXP group_sizes, int init_loc = 1){
  const int* __restrict__ p_gsizes = INTEGER(group_sizes);
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

// Finaliser for external pointer to vector of int ptrs
static void int_ptrs_finaliser(SEXP ext) {

  void *addr = R_ExternalPtrAddr(ext);

  if (!addr) return;

  INT_LOC_PTRS *loc_ptrs = (INT_LOC_PTRS *) R_ExternalPtrAddr(ext);
  R_Free(loc_ptrs->PTRS);
  std::free(loc_ptrs);
  R_ClearExternalPtr(ext);
}

SEXP new_ext_int_ptrs(int** ptrs, int n_ptrs, SEXP list_of_ints){

  INT_LOC_PTRS *loc_ptrs = (INT_LOC_PTRS *) calloc(1, sizeof *loc_ptrs);

  if (!loc_ptrs){
    R_Free(ptrs);
    std::free(loc_ptrs);
    Rf_error("Internal error, out of memory");
  }

  loc_ptrs->N_PTRS = n_ptrs;
  loc_ptrs->PTRS = ptrs;

  if (!loc_ptrs->PTRS){
    R_Free(ptrs);
    std::free(loc_ptrs);
    Rf_error("Internal error, out of memory");
  }
  SEXP r_int_ptrs = SHIELD(R_MakeExternalPtr(loc_ptrs, R_NilValue, list_of_ints));
  R_RegisterCFinalizerEx(r_int_ptrs, int_ptrs_finaliser, TRUE);
  YIELD(1);
  return r_int_ptrs;
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

  int **loc_ptrs = (int **) R_Calloc(n_groups, int*);
  int *int_ptr;

  for (unsigned int i = 0; i < n_groups; ++i, k += group_size){
    group_size = p_gs[i];
    SET_VECTOR_ELT(group_locs, i, new_vec(INTSXP, group_size));
    int_ptr = INTEGER(p_out[i]);
    safe_memcpy(&int_ptr[0], &p_o[k], group_size * sizeof(int));
    loc_ptrs[i] = int_ptr;
  }
  // SEXP r_int_ptrs = SHIELD(new_ext_int_ptrs(loc_ptrs, n_groups, group_locs)); ++NP;
  // SEXP out = SHIELD(new_vec(VECSXP, 2)); ++NP;
  // SET_VECTOR_ELT(out, 0, group_locs);
  // SET_VECTOR_ELT(out, 1, r_int_ptrs);
  // SEXP out_names = SHIELD(new_vec(STRSXP, 2)); ++NP;
  // SET_STRING_ELT(out_names, 0, Rf_mkCharCE("group_locs", CE_UTF8));
  // SET_STRING_ELT(out_names, 1, Rf_mkCharCE("loc_ptrs", CE_UTF8));
  // set_names(out, out_names);
  R_Free(loc_ptrs);
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

  // SEXP r_int_ptrs = SHIELD(new_ext_int_ptrs(loc_ptrs)); ++NP;

  // Initialise a vector of group location indices

  SEXP loc_indices = SHIELD(new_vec(INTSXP, n_groups)); ++NP;
  int* __restrict__ p_loc_indices = INTEGER(loc_indices);
  safe_memset(p_loc_indices, 0, n_groups * sizeof(int));

  int n = Rf_length(group_id);
  int cur_group;
  int cur_group_loc;

  // int *loc_ptr;

  for (int i = 0; i < n; ++i){
    cur_group = p_group_id[i] - 1;
    cur_group_loc = p_loc_indices[cur_group]++;
    loc_ptrs[cur_group][cur_group_loc] = i + 1;
  }
  // SEXP r_int_ptrs = SHIELD(new_ext_int_ptrs(loc_ptrs, n_groups, group_locs)); ++NP;
  // SEXP out = SHIELD(new_vec(VECSXP, 2)); ++NP;
  // SET_VECTOR_ELT(out, 0, group_locs);
  // SET_VECTOR_ELT(out, 1, r_int_ptrs);
  // SEXP out_names = SHIELD(new_vec(STRSXP, 2)); ++NP;
  // SET_STRING_ELT(out_names, 0, Rf_mkCharCE("group_locs", CE_UTF8));
  // SET_STRING_ELT(out_names, 1, Rf_mkCharCE("loc_ptrs", CE_UTF8));
  // set_names(out, out_names);
  R_Free(loc_ptrs);
  YIELD(NP);
  return group_locs;
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

[[cpp11::register]]
SEXP cpp_row_id(SEXP order, SEXP group_sizes, bool ascending){
  int n = Rf_length(order);
  SEXP out = SHIELD(new_vec(INTSXP, n));
  int* __restrict__ p_out = INTEGER(out);
  const int* __restrict__ p_o = INTEGER(order);
  int* __restrict__ p_group_sizes = INTEGER(group_sizes);
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
  YIELD(1);
  return out;
}

// Takes a data frame of logical vectors
// And reduces them to a single logical vector
// Applying the && (AND) condition for each row
// Then uses the cheapr::cpp_which algorithm
// To find which rows are TRUE

// Alternative that goes col-wise

// SEXP cpp_which_all(SEXP x){
//   if (!Rf_inherits(x, "data.frame")){
//     Rf_error("x must be a data frame");
//   }
//   const SEXP *p_x = VECTOR_PTR_RO(x);
//   int NP = 0, n_true = 0;
//
//   int n_cols = Rf_length(x);
//   int n_rows = df_nrow(x);
//
//   // Store pointers for fast access
//   std::vector<const int*> col_ptrs(n_cols);
//
//   for (int i = 0; i < n_cols; ++i) {
//     col_ptrs[i] = INTEGER_RO(p_x[i]);
//   }
//
//   SEXP out = R_NilValue;
//
//   if (n_cols == 0){
//     out = SHIELD(new_vec(INTSXP, 0)); ++NP;
//   } else if (n_cols == 1){
//     SEXP r_true = SHIELD(new_vec(LGLSXP, 1)); ++NP;
//     LOGICAL(r_true)[0] = 1;
//     out = SHIELD(cheapr::val_find(p_x[0], r_true, false)); ++NP;
//   } else {
//     SEXP lgl = SHIELD(new_vec(LGLSXP, n_rows)); ++NP;
//     int* __restrict__ p_lgl = INTEGER(lgl);
//
//     // Copy values of x[[1]] into our vector
//
//     const int *p_first = col_ptrs[0];
//     safe_memmove(p_lgl, &p_first[0], sizeof(int) * n_rows);
//
//     const int *p_temp;
//
//     // Starting from 2nd col to 2nd last col
//
//     for (int i = 1; i < (n_cols - 1); ++i) {
//       p_temp = col_ptrs[i];
//       for (int j = 0; j < n_rows; ++j){
//         p_lgl[j] = p_lgl[j] == 1 && p_temp[j] == 1;
//       }
//     }
//
//     // Last col
//     // This is where we count how many true values
//     // are returned in the final vector
//
//     p_temp = col_ptrs[n_cols - 1];
//     for (int j = 0; j < n_rows; ++j){
//       p_lgl[j] = (p_lgl[j] == 1) && (p_temp[j] == 1);
//       n_true += p_lgl[j];
//     }
//
//     // WHICH algo
//
//     out = SHIELD(new_vec(INTSXP, n_true)); ++NP;
//     int* __restrict__ p_out = INTEGER(out);
//     int whichi = 0;
//     int i = 0;
//     while (whichi < n_true){
//       p_out[whichi] = i + 1;
//       whichi += (p_lgl[i++] == 1);
//     }
//   }
//   YIELD(NP);
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
    safe_memset(p_lgl, 0, n_rows * sizeof(int));

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
//
// SEXP cpp_slice_locs(SEXP group_locs, SEXP locs){
//   int n_groups = Rf_length(group_locs);
//
//   const SEXP *p_groups = VECTOR_PTR_RO(group_locs);
//
//   SEXP out = SHIELD(new_vec(VECSXP, n_groups));
//
//   const int *p_locs = INTEGER_RO(locs);
//   int loc_size = Rf_length(locs);
//
//   // SEXP r_int_ptrs = SHIELD(get_loc_ptrs(group_locs));
//
//   SEXP elem = R_NilValue;
//
//   // if (Rf_isNull(r_int_ptrs)){
//     for (int i = 0; i < n_groups; ++i){
//       elem = p_groups[i];
//       SET_VECTOR_ELT(out, i, int_slice(elem, locs, INTEGER_RO(elem), Rf_length(elem), p_locs, loc_size));
//     }
//   // } else {
//   //   const auto& int_ptrs_vec = *static_cast<std::vector<int*>*>(R_ExternalPtrAddr(r_int_ptrs));
//   //   for (int i = 0; i < n_groups; ++i){
//   //     elem = p_groups[i];
//   //     SET_VECTOR_ELT(out, i, int_slice(elem, locs, int_ptrs_vec[i], Rf_length(elem), p_locs, loc_size));
//   //   }
//   //   // const auto int_ptrs_vec = static_cast<std::vector<int*>*>(R_ExternalPtrAddr(r_int_ptrs));
//   //   // for (int i = 0; i < n_groups; ++i){
//   //   //   elem = p_groups[i];
//   //   //   SET_VECTOR_ELT(out, i, int_slice(elem, locs, (*int_ptrs_vec)[i], Rf_length(elem), p_locs, loc_size));
//   //   // }
//   // }
//   YIELD(1);
//   return out;
// }

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

  // SEXP r_loc_ptrs = SHIELD(get_loc_ptrs(group_locs)); ++NP;

  // if (Rf_isNull(r_loc_ptrs)){
    for (int i = 0; i < n_groups; ++i){
      elem = p_group_locs[i];
      SET_VECTOR_ELT(
        out, i,
        int_slice(
          elem, locs, INTEGER_RO(elem), Rf_length(elem), p_locs, locs_size
        )
      );
    }
  // } else {
  //   int **locs_ptrs = static_cast<int**>(R_ExternalPtrAddr(r_loc_ptrs));
  //   for (int i = 0; i < n_groups; ++i){
  //     elem = p_group_locs[i];
  //     SET_VECTOR_ELT(
  //       out, i,
  //       int_slice(
  //         elem, locs, locs_ptrs[i], Rf_length(elem), p_locs, locs_size
  //       )
  //     );
  //   }
  // }
  YIELD(NP);
  return out;
}

SEXP cpp_run_id(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  if (cheapr::is_compact_seq(x)){
    return cpp11::package("base")[":"](1, n);
  }
  SEXP out = SHIELD(new_vec(INTSXP, n));
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
    int64_t *p_x = INTEGER64_PTR(x);
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
    YIELD(1);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  YIELD(1);
  return out;
}

SEXP cpp_df_run_id(cpp11::writable::list x){
  int32_t NP = 0;
  int n_cols = Rf_length(x);
  int n_rows = df_nrow(x);

  const SEXP *p_x = VECTOR_PTR_RO(x);

  for (int l = n_cols - 1; l >= 0; --l){
    if (cheapr::is_compact_seq(p_x[l])){
      SEXP compact_seq = SHIELD(p_x[l]); ++NP;
      SEXP out = SHIELD(cpp_run_id(compact_seq)); ++NP;
      YIELD(NP);
      return out;
    }
    if (cpp_is_exotic(p_x[l])){
      SEXP group_ids = SHIELD(fp_group_id(p_x[l], cpp11::named_arg("order") = false));
      x[l] = group_ids;
      YIELD(1);
    }
  }

  if (n_cols == 1){
    SEXP x1 = SHIELD(VECTOR_ELT(x, 0)); ++NP;
    SEXP out = SHIELD(cpp_run_id(x1)); ++NP;
    YIELD(NP);
    return out;
  }

  SEXP out = SHIELD(new_vec(INTSXP, n_rows)); ++NP;
  int *p_out = INTEGER(out);

  if (n_cols < 1){
    for (int i = 0; i < n_rows; ++i) p_out[i] = 1;
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
        int *p_xj = INTEGER(p_x[j]);
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
  } else if (cpp_is_exotic(x)){
    cpp11::function fastplyr_group_id = cpp11::package("fastplyr")["group_id"];
    SEXP group_ids = SHIELD(fastplyr_group_id(x, cpp11::named_arg("order") = false));
    SEXP out = SHIELD(cpp_run_id(group_ids));
    YIELD(2);
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
SEXP cpp_new_loc_ptrs(SEXP x){
  if (TYPEOF(x) != VECSXP){
    Rf_error("`x` must be a list of integer vectors in %s", __func__);
  }
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);

  int **loc_ptrs = (int **) R_Calloc(n, int*);

  for (int i = 0; i < n; ++i){
    loc_ptrs[i] = INTEGER(p_x[i]);
  }
  // Registers finaliser that will free loc_ptrs
  return new_ext_int_ptrs(loc_ptrs, n, x);
}
