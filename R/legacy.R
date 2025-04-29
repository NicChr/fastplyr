#' @noRd

# Legacy functions to not break dependencies ------------------------------

# fp_reconstruct <- function(template, data, copy_extra_attributes = TRUE){
#   UseMethod("fp_reconstruct")
# }
# @export
# fp_reconstruct.default <- function(template, data, copy_extra_attributes = TRUE){
#   cheapr::reconstruct(data, template)
# }
# reconstruct <- fp_reconstruct
#
# df_rep <- cheapr::cheapr_rep
# df_rep_each <- cheapr::cheapr_rep_each
# df_init <- na_init
# df_row_slice <- cheapr::sset
