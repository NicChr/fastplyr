f_consecutive_id <- function(x){
  if (inherits(x, "data.frame")){
    .Call(`_fastplyr_cpp_df_run_id`, x)
  } else if (cpp_is_exotic(x)){
    .Call(`_fastplyr_cpp_run_id`, group_id(x, order = FALSE))
  } else {
    .Call(`_fastplyr_cpp_run_id`, x)
  }
}
