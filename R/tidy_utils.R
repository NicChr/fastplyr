
# Somewhat safer check of the .by arg
# e.g mutate(group_by(iris, Species), .by = any_of("okay"))
# Should not produce an error with this check
check_by <- function(data, .by){
  if (!rlang::quo_is_null(rlang::enquo(.by))){
    if (inherits(data, "grouped_df")){
      by_nms <- tidy_select_names(data, {{ .by }})
      if (length(by_nms) > 0L){
        stop(".by cannot be used on a grouped_df")
      }
    }
  }
}
check_cols <- function(n_dots, .cols = NULL){
  if (n_dots > 0 && !is.null(.cols)){
    stop("Cannot supply variables through ... and .cols, use one argument.")
  }
}

get_from_package <- function(x, package){
  get(x, asNamespace(package), inherits = FALSE)
}

# Quosure text/var check for select()
# NULL is removed.
quo_select_info <- function(quos, data){
  quo_nms <- names(quos)
  quo_text <- add_names(character(length(quos)), quo_nms)
  quo_is_null <- add_names(logical(length(quos)), quo_nms)
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    quo_text[[i]] <- deparse2(rlang::quo_get_expr(quo))
    # quo_text[[i]] <- rlang::expr_name(rlang::quo_get_expr(quo))
    quo_is_null[[i]] <- rlang::quo_is_null(quo)
  }
  quo_text <- quo_text[!quo_is_null]
  quo_nms <- quo_nms[!quo_is_null]
  is_char_var <- quo_text %in% names(data)
  is_num_var <- quo_text %in% as.character(df_seq_along(data, "cols"))
  list(quo_nms = quo_nms,
       quo_text = quo_text,
       is_num_var = is_num_var,
       is_char_var = is_char_var)
}

# Tidyselect col positions with names
tidy_select_pos <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  check_cols(dots_length(...), .cols = .cols)
  # Method for when cols is supplied
  if (!is.null(.cols)){
    out <- col_select_pos(data, .cols)
  } else {
    # If exact cols are specified, faster to use
    # col_select_pos()
    quo_select_info <- quo_select_info(rlang::enquos(...), data)
    quo_text <- quo_select_info[["quo_text"]]
    all_char <- all(quo_select_info[["is_char_var"]])
    all_num <- all(quo_select_info[["is_num_var"]])
    if (all_char){
      out <- col_select_pos(data, quo_text)
    } else if (all_num){
      pos <- as.double(quo_text)
      names(pos) <- names(quo_text)
      out <- col_select_pos(data, pos)
      # Otherwise we use tidyselect
    } else {
      out <- tidyselect::eval_select(rlang::expr(c(...)), data = data)
    }
    if (all_char || all_num){
      is_dup <- collapse::fduplicated(list(names(out), unname(out)))
      out <- out[!is_dup]
      if (anyDuplicated(names(out))){
        # Use tidyselect for error
        tidyselect::eval_select(rlang::expr(c(...)), data = data)
      }
    }
  }
  out
}
# Select variables utilising tidyselect notation
tidy_select_names <- function(data, ..., .cols = NULL){
  names(tidy_select_pos(data, ..., .cols = .cols))
}
# Basic tidyselect information for further manipulation
# Includes output and input names which might be useful
tidy_select_info <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  out_nms <- names(pos)
  pos <- unname(pos)
  in_nms <- data_nms[pos]
  renamed <- is.na(match(out_nms, data_nms) != pos)
  list("pos" = pos,
       "out_nms" = out_nms,
       "in_nms" = in_nms,
       "renamed" = renamed)
}

mutate_cols <- get_from_package("mutate_cols", "dplyr")
dplyr_quosures <- get_from_package("dplyr_quosures", "dplyr")
compute_by <- get_from_package("compute_by", "dplyr")

mutate_summary_ungrouped <- function(.data, ...,
                                     .keep = c("all", "used", "unused", "none"),
                                     error_call = rlang::caller_env()){
  .keep <- rlang::arg_match(.keep)
  original_cols <- names(.data)
  bare_data <- df_ungroup(.data)
  group_data <- new_tbl(".rows" = add_attr(list(seq_len(df_nrow(bare_data))),
                                           "class",
                                           c("vctrs_list_of", "vctrs_vctr", "list")))
  by <- add_attr(
    list(
      type = "ungrouped",
      names = character(),
      data = group_data
    ),
    "class",
    "dplyr_by"
  )
  dplyr_quos <- dplyr_quosures(...)
  # names(dplyr_quos) <- dot_expr_names(...)
  cols <- mutate_cols(bare_data, dplyr_quos,
                      by = by, error_call = error_call)
  out_data <- dplyr::dplyr_col_modify(bare_data, cols)
  final_cols <- names(cols)
  used <- attr(cols, "used")
  keep_cols <- switch(.keep,
                      all = names(used),
                      none = final_cols,
                      used = names(used)[which(used)],
                      unused = names(used)[which(used, invert = TRUE)])
  out_data <- f_select(out_data, .cols = keep_cols)
  out <- list(data = out_data, cols = final_cols)
  out
}

mutate_summary_grouped <- function(.data, ...,
                                   .keep = c("all", "used", "unused", "none"),
                                   .by = NULL,
                                   error_call = rlang::caller_env()){
  .keep <- rlang::arg_match(.keep)
  original_cols <- names(.data)
  by <- compute_by(by = {{ .by }}, data = .data,
                   by_arg = ".by", data_arg = ".data")
  group_vars <- get_groups(.data, .by = {{ .by }})
  dplyr_quos <- dplyr_quosures(...)
  # names(dplyr_quos) <- dot_expr_names(...)
  cols <- mutate_cols(.data, dplyr_quos,
                      by = by, error_call = error_call)
  out_data <- dplyr::dplyr_col_modify(.data, cols)
  final_cols <- names(cols)
  used <- attr(cols, "used")
  keep_cols <- switch(.keep,
                      all = names(used),
                      none = final_cols,
                      used = names(used)[which(used)],
                      unused = names(used)[which(used, invert = TRUE)])
  # Add missed group vars
  keep_cols <- c(group_vars, keep_cols[match(keep_cols, group_vars, 0L) == 0L])
  # Match the original ordering of columns
  keep_cols <- keep_cols[order(match(keep_cols, original_cols))]
  out_data <- f_select(out_data, .cols = keep_cols)
  out <- list(data = out_data, cols = final_cols)
  out
}

tidy_group_info_tidyselect <- function(data, ..., .by = NULL, .cols = NULL,
                                       ungroup = TRUE, rename = TRUE,
                                       unique_groups = TRUE){
  n_dots <- dots_length(...)
  group_vars <- get_groups(data, {{ .by }})
  group_pos <- match(group_vars, names(data))
  extra_groups <- character()
  if (ungroup){
    out <- df_ungroup(data)
  } else {
    out <- data
  }
  extra_group_pos <- tidy_select_pos(out, ..., .cols = .cols)
  if (!rename){
    names(extra_group_pos) <- names(data)[extra_group_pos]
  }
  out <- f_rename(out, .cols = extra_group_pos)
  extra_groups <- names(extra_group_pos)
  # Recalculate group vars in case they were renamed
  group_vars <- names(out)[group_pos]
  address_equal <- rep_len(TRUE, df_ncol(data))
  address_equal[extra_group_pos] <-
    names(data)[extra_group_pos] == names(extra_group_pos)
  names(address_equal) <- names(data)
  any_groups_changed <- !all(address_equal[group_vars])
  if (unique_groups){
    extra_groups <- setdiff(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, setdiff(extra_groups, group_vars))
  }
  list("data" = out,
       "dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups,
       "groups_changed" = any_groups_changed,
       "address_equal" = address_equal)
}

tidy_group_info_datamask <- function(data, ..., .by = NULL,
                                     ungroup = TRUE,
                                     unique_groups = TRUE){
  n_dots <- dots_length(...)
  group_vars <- get_groups(data, {{ .by }})
  group_pos <- match(group_vars, names(data))
  extra_groups <- character()
  if (ungroup){
    out <- df_ungroup(data)
  } else {
    out <- data
  }
  # Data-masking for dots expressions
  if (n_dots > 0){
    if (ungroup){
      out_info <- mutate_summary_ungrouped(out, ...)
    } else {
      out_info <- mutate_summary_grouped(out, ..., .by = {{ .by }})
    }
    out <- out_info[["data"]]
    extra_groups <- out_info[["cols"]]
  }
  if (unique_groups){
    extra_groups <- setdiff(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, setdiff(extra_groups, group_vars))
  }
  address_equal <- add_names(cpp_address_equal(
    data, cheapr::sset(df_ungroup(out), j = names(data))
  ), names(data))
  any_groups_changed <- !all(address_equal[group_vars])
  list("data" = out,
       "dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups,
       "groups_changed" = any_groups_changed,
       "address_equal" = address_equal)
}

tidy_group_info <- function(data, ..., .by = NULL, .cols = NULL,
                            ungroup = TRUE, rename = TRUE,
                            dots_type = "data-mask",
                            unique_groups = TRUE){
  check_cols(n_dots = dots_length(...), .cols = .cols)
  if (is.null(.cols) && dots_type == "data-mask"){
    tidy_group_info_datamask(data, ..., .by = {{ .by }},
                             ungroup = ungroup,
                             unique_groups = unique_groups)

  } else {
    tidy_group_info_tidyselect(data, ..., .by = {{ .by }},
                               .cols = .cols,
                               ungroup = ungroup,
                               rename = rename,
                               unique_groups = unique_groups)
  }
}

unique_count_col <- function(data, col = "n"){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  if (col %in% data_nms){
    unique_count_col(data, col = paste0(col, "n"))
  } else {
    col
  }
}
# Checks if dataset has a variable name and returns unique name
unique_col_name <- function(data, col){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  i <- 1L
  grp_nm <- col
  while (col %in% data_nms){
    i <- i + 1L
    col <- paste0(grp_nm, i)
  }
  col
}
vctrs_new_list_of <- function(x = list(), ptype){
  structure(x,
            ptype = ptype,
            class = c("vctrs_list_of",
                      "vctrs_vctr",
                      "list"))
}
