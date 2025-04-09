
get_from_package <- function(x, package){
  get(x, asNamespace(package), inherits = FALSE)
}

# cheapr ------------------------------------------------------------------

val_rm <- get_from_package("val_rm", "cheapr")
list_as_df <- get_from_package("list_as_df", "cheapr")
set_add_attr <- get_from_package("cpp_set_add_attr", "cheapr")
set_add_attributes <- get_from_package("cpp_set_add_attributes", "cheapr")
set_rm_attr <- get_from_package("cpp_set_rm_attr", "cheapr")
set_rm_attributes <- get_from_package("cpp_set_rm_attributes", "cheapr")
which <- cheapr::which_
which_not_in <- get_from_package("which_not_in", "cheapr")
which_in <- get_from_package("which_in", "cheapr")
cpp_int64_to_numeric <- get_from_package("cpp_int64_to_numeric", "cheapr")
cpp_loc_set_replace <- get_from_package("cpp_loc_set_replace", "cheapr")
named_list <- get_from_package("named_list", "cheapr")
as_list_of <- get_from_package("as_list_of", "cheapr")
cpp_is_simple_atomic_vec <- get_from_package("cpp_is_simple_atomic_vec", "cheapr")
cpp_recycle <- get_from_package("cpp_recycle", "cheapr")
cpp_c <- get_from_package("cpp_c", "cheapr")
cheapr_add_cols <- get_from_package("df_add_cols", "cheapr")
cheapr_cpp_rev <- get_from_package("cpp_rev", "cheapr")
na_init <- get_from_package("cpp_na_init", "cheapr")
df_add_cols <- get_from_package("cpp_df_assign_cols", "cheapr")
cpp_new_df <- get_from_package("cpp_new_df", "cheapr")
cpp_df_col_c <- get_from_package("cpp_df_col_c", "cheapr")
cpp_str_coalesce <- get_from_package("cpp_str_coalesce", "cheapr")
cpp_sset <- get_from_package("cpp_sset", "cheapr")
cpp_reconstruct <- get_from_package("cpp_reconstruct", "cheapr")
`%in_%` <- cheapr::`%in_%`


# dplyr -------------------------------------------------------------------

mutate_cols <- get_from_package("mutate_cols", "dplyr")
dplyr_quosures <- get_from_package("dplyr_quosures", "dplyr")
compute_by <- get_from_package("compute_by", "dplyr")
