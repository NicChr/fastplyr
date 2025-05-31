
get_from_package <- function(x, package){
  get(x, asNamespace(package), inherits = FALSE)
}

# cheapr ------------------------------------------------------------------

set_add_attr <- function(x, which, value){
  cheapr::attrs_add(x, .args = `names<-`(list(value), which), .set = TRUE)
}
which <- cheapr::which_
which_not_in <- function(x, table){
  cheapr::na_find(
    collapse::fmatch(x, table, overid = 2L, nomatch = NA_integer_)
  )
}
which_in <- function(x, table){
  cheapr::na_find(
    collapse::fmatch(x, table, overid = 2L, nomatch = NA_integer_), invert = TRUE
  )
}

na_init <- function(x, n = 1L){
  cheapr::cheapr_rep_len(cheapr::sset(x, 0L), n)
}

cpp_int64_to_numeric <- get_from_package("cpp_int64_to_numeric", "cheapr")
cpp_loc_set_replace <- get_from_package("cpp_loc_set_replace", "cheapr")
cpp_is_simple_atomic_vec <- get_from_package("cpp_is_simple_atomic_vec", "cheapr")
cheapr_cpp_rev <- get_from_package("cpp_rev", "cheapr")
cpp_rebuild <- get_from_package("cpp_rebuild", "cheapr")
vec_setdiff <- get_from_package("vec_setdiff", "cheapr")
vec_intersect <- get_from_package("vec_intersect", "cheapr")
`%in_%` <- cheapr::`%in_%`


# dplyr -------------------------------------------------------------------

mutate_cols <- get_from_package("mutate_cols", "dplyr")
dplyr_quosures <- get_from_package("dplyr_quosures", "dplyr")
compute_by <- get_from_package("compute_by", "dplyr")

