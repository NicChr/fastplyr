
# To-be-continued
# Currently fastplyr can't handle rowwise data frames

# f_rowwise <- function(data, ..., ascending = TRUE,
#                       .by = NULL, .cols = NULL,
#                       .name = NULL){
#   # data %>%
#   #   add_row_id(..., .cols = .cols, .name = .name,
#   #              ascending = ascending, .by = {{ .by }}) %>%
#   #   f_group_by(.cols = df_ncol(data) + 1L, order = FALSE)
#   # groups <- f_select(data, ..., .cols = .cols)
#   #
#   # row_ids <- row_id(groups)
#   #
#   # row_id_tbl <- new_tbl(.row_id = row_ids)
#   #
#   # out <- f_bind_cols(data, row_id_tbl)
#   # groups <- f_bind_cols(groups, row_id_tbl)
#   #
#   # row_id_list <- vctrs_new_list_of(as.list(row_ids), integer())
#   # group_data <- f_bind_cols(f_select(out, .cols = length(out)), new_tbl(.rows = row_id_list))
#   # # group_data <- as_tbl(f_bind_cols(groups, new_tbl(.rows = row_id_list)))
#   #
#   # attr(group_data, "ordered") <- TRUE
#   # attr(out, "groups") <- group_data
#   # class(out) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
#   # out
# }
