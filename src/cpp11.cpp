// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// fastplyr.cpp
SEXP cpp_address_equal(SEXP x, SEXP y);
extern "C" SEXP _fastplyr_cpp_address_equal(SEXP x, SEXP y) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_address_equal(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<SEXP>>(y)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_nrows(SEXP x);
extern "C" SEXP _fastplyr_cpp_nrows(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_nrows(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_list_subset(SEXP x, SEXP ptype, SEXP i, SEXP default_value);
extern "C" SEXP _fastplyr_cpp_list_subset(SEXP x, SEXP ptype, SEXP i, SEXP default_value) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_list_subset(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<SEXP>>(ptype), cpp11::as_cpp<cpp11::decay_t<SEXP>>(i), cpp11::as_cpp<cpp11::decay_t<SEXP>>(default_value)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_sorted_group_starts(SEXP group_sizes, int init_loc);
extern "C" SEXP _fastplyr_cpp_sorted_group_starts(SEXP group_sizes, SEXP init_loc) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_sorted_group_starts(cpp11::as_cpp<cpp11::decay_t<SEXP>>(group_sizes), cpp11::as_cpp<cpp11::decay_t<int>>(init_loc)));
  END_CPP11
}
// fastplyr.cpp
SEXP is_s3_atomic(SEXP x);
extern "C" SEXP _fastplyr_is_s3_atomic(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(is_s3_atomic(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_group_locs(SEXP order, SEXP group_sizes);
extern "C" SEXP _fastplyr_cpp_group_locs(SEXP order, SEXP group_sizes) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_group_locs(cpp11::as_cpp<cpp11::decay_t<SEXP>>(order), cpp11::as_cpp<cpp11::decay_t<SEXP>>(group_sizes)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_row_id(SEXP order, SEXP group_sizes, bool ascending);
extern "C" SEXP _fastplyr_cpp_row_id(SEXP order, SEXP group_sizes, SEXP ascending) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_row_id(cpp11::as_cpp<cpp11::decay_t<SEXP>>(order), cpp11::as_cpp<cpp11::decay_t<SEXP>>(group_sizes), cpp11::as_cpp<cpp11::decay_t<bool>>(ascending)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_reduce_logicals(SEXP x);
extern "C" SEXP _fastplyr_cpp_reduce_logicals(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_reduce_logicals(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_which_all(SEXP x);
extern "C" SEXP _fastplyr_cpp_which_all(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_which_all(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_df_group_indices(SEXP rows, int size);
extern "C" SEXP _fastplyr_cpp_df_group_indices(SEXP rows, SEXP size) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_df_group_indices(cpp11::as_cpp<cpp11::decay_t<SEXP>>(rows), cpp11::as_cpp<cpp11::decay_t<int>>(size)));
  END_CPP11
}
// fastplyr.cpp
SEXP cpp_slice_locs(SEXP group_locs, SEXP locs);
extern "C" SEXP _fastplyr_cpp_slice_locs(SEXP group_locs, SEXP locs) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_slice_locs(cpp11::as_cpp<cpp11::decay_t<SEXP>>(group_locs), cpp11::as_cpp<cpp11::decay_t<SEXP>>(locs)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_fastplyr_cpp_address_equal",       (DL_FUNC) &_fastplyr_cpp_address_equal,       2},
    {"_fastplyr_cpp_df_group_indices",    (DL_FUNC) &_fastplyr_cpp_df_group_indices,    2},
    {"_fastplyr_cpp_group_locs",          (DL_FUNC) &_fastplyr_cpp_group_locs,          2},
    {"_fastplyr_cpp_list_subset",         (DL_FUNC) &_fastplyr_cpp_list_subset,         4},
    {"_fastplyr_cpp_nrows",               (DL_FUNC) &_fastplyr_cpp_nrows,               1},
    {"_fastplyr_cpp_reduce_logicals",     (DL_FUNC) &_fastplyr_cpp_reduce_logicals,     1},
    {"_fastplyr_cpp_row_id",              (DL_FUNC) &_fastplyr_cpp_row_id,              3},
    {"_fastplyr_cpp_slice_locs",          (DL_FUNC) &_fastplyr_cpp_slice_locs,          2},
    {"_fastplyr_cpp_sorted_group_starts", (DL_FUNC) &_fastplyr_cpp_sorted_group_starts, 2},
    {"_fastplyr_cpp_which_all",           (DL_FUNC) &_fastplyr_cpp_which_all,           1},
    {"_fastplyr_is_s3_atomic",            (DL_FUNC) &_fastplyr_is_s3_atomic,            1},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_fastplyr(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
