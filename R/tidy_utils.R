
# Somewhat safer check of the .by arg
# e.g mutate(group_by(iris, Species), .by = any_of("okay"))
# Should not produce an error with this check
check_by <- function(data, .by){
  if (!rlang::quo_is_null(rlang::enquo(.by))){
    if (inherits(data, "grouped_df")){
      by_nms <- tidy_select_pos(data, {{ .by }})
      if (length(by_nms) > 0L){
        cli::cli_abort("{.arg .by} cannot be used on a grouped_df")
      }
    }
  }
}
check_cols <- function(n_dots, .cols = NULL){
  if (n_dots > 0 && !is.null(.cols)){
    cli::cli_abort("Cannot supply variables through {.arg ...} and {.arg .cols}, use one argument")
  }
}

# This function returns the groups of a data frame
get_groups <- function(data, .by = NULL){
  check_rowwise(data)
  dplyr_groups <- group_vars(data)
  if (rlang::quo_is_null(rlang::enquo(.by))){
    by_groups <- NULL
  } else {
    by_groups <- names(tidy_select_names(data, {{ .by }}))
  }
  if (length(by_groups) > 0L){
    if (length(dplyr_groups) > 0L){
      cli::cli_abort("{.arg .by} cannot be used on a grouped_df")
    }
    by_groups
  } else {
    dplyr_groups
  }
}

# Turn character vector into named character vector
as_named <- function(x){
  nms <- names(x)
  if (is.null(nms)){
    nms <- x
  } else {
    nms <- str_coalesce(nms, x)
  }
  names(x) <- nms
  x
}

tidy_as_list_of <- function (...){
  dots <- list_tidy(..., .keep_null = FALSE)
  if (length(dots) == 1 && !is.object(dots[[1L]]) && is.list(dots[[1L]])) {
    dots[[1L]]
  }
  else {
    dots
  }
}

quo_labels <- function(quos, named = TRUE){
  out <- vapply(quos, function(x) deparse2(rlang::quo_get_expr(x)), "", USE.NAMES = FALSE)
  if (named){
    names(out) <- names(quos)
    out <- as_named(out)
  }
  out
}

# rlang::enquos() but quosures are always named
named_quos <- function(...){
  exprs <- rlang::quos(..., .ignore_empty = "all")
  nms <- names(exprs)

  # Fix empty names
  if (is.null(nms)){
    nms <- quo_labels(exprs, named = FALSE)
  } else if (!all(nzchar(nms))){
    nms <- str_coalesce(nms, quo_labels(exprs, named = FALSE))
  }
  names(exprs) <- nms
  exprs
}

fastplyr_quos <- function(..., .named = FALSE, .data = NULL){
  if (.named){
    out <- named_quos(...)
  } else {
    out <- rlang::quos(..., .ignore_empty = "all")
  }
  if (!is.null(.data)){
    for (i in seq_along(out)){
      quo <- out[[i]]
      if (rlang::quo_is_call(quo, "across")){
        left <- out[seq_len(i - 1L)]
        unpacked_quos <- unpack_across(quo, .data)
        if (i < length(out)){
          right <- out[seq.int(i + 1L, length(out), 1L)]
        } else {
          right <- list()
        }
        out[[i]] <- NULL
        out <- c(left, unpacked_quos, right)
      }
    }
  }
  out
}

unpack_across <- function(quo, data){

  expr <- rlang::quo_get_expr(quo)
  quo_env <- rlang::quo_get_env(quo)

  clean_expr <- match.call(
    definition = dplyr::across,
    call = expr,
    expand.dots = FALSE,
    envir = quo_env
  )

  if (!".cols" %in% names(clean_expr)){
    cli::cli_abort("{.arg .cols} must be supplied in {.fn across}")
  }
  unused_args <- fast_setdiff(names(clean_expr)[-1], c(".cols", ".fns", ".names"))

  if (length(unused_args) > 0){
    cli::cli_abort("{.arg ...} must be unused")
  }

  across_vars <- clean_expr[[".cols"]]
  across_fns <- clean_expr[[".fns"]]
  across_nms <- clean_expr[[".names"]]
  across_unpack <- clean_expr[[".unpack"]]

  fn_names <- NULL
  unpack <- FALSE

  if (is.atomic(across_vars)){
    cols <- names(col_select_pos(data, across_vars))
  } else if (rlang::is_call(across_vars, ":")){
    args <- as.list(across_vars[-1L])
    if (length(args) == 2 && is.atomic(args[[1L]]) && is.atomic(args[[2L]])){
      cols <- names(col_select_pos(data, eval(across_vars, envir = quo_env)))
    } else {
      cols <- names(tidyselect::eval_select(across_vars, data))
    }
  } else {
    cols <- names(tidyselect::eval_select(across_vars, data))
  }

  if (rlang::is_call(across_fns, "list")){
    fn_tree <- as.list(across_fns)[-1L]
    fn_names <- names(fn_tree) %||% character(length(fn_tree))
  } else if (!".fns" %in% names(clean_expr)){
    fn_tree <- list(identity)
  } else {
    fn_tree <- list(across_fns)
  }

  # Evaluate functions

  for (i in seq_along(fn_tree)){
    fn <- eval(fn_tree[[i]], envir = quo_env)
    if (!is.function(fn)){
      fn <- rlang::as_function(fn)
    }
    fn_tree[[i]] <- fn
  }

  if (".unpack" %in% names(expr)){
    unpack <- eval(across_unpack, envir = quo_env)
  }
  if (unpack){
    return(list(quo))
  }

  out_names <- across_col_names(cols, .names = across_nms, .fns = fn_names)
  out_size <- length(out_names)

  # Recycle cols/fns
  cols <- rep_len(cols, out_size)
  fn_tree <- rep_len(fn_tree, out_size)

  out <- cheapr::new_list(out_size)
  names(out) <- out_names

  for (i in seq_along(out)){
    fn <- fn_tree[[i]]
    col <- cols[[i]]
    out[[i]] <- rlang::new_quosure(rlang::call2(fn, as.symbol(col)), quo_env)
  }
  out
}

# Recursively checks call tree for a function call from a specified namespace
# We use it to check for any dplyr functions in call tree in `eval_all_tidy`
# call_contains_ns <- function(expr, ns, env = rlang::caller_env()){
#   if (rlang::is_quosure(expr)){
#     expr <- rlang::quo_get_expr(expr)
#   }
#   if (!is.call(expr)){
#     return(FALSE)
#   }
#   if (rlang::is_call(expr, ns = ns)){
#     return(TRUE)
#   }
#   out <- FALSE
#   tree <- as.list(expr)
#   for (branch in tree){
#     if (is.call(branch)){
#       # return(call_contains_ns(branch, ns, env)) # Old version
#       if (call_contains_ns(branch, ns, env = env)){
#         out <- TRUE
#         break
#       }
#     }
#     if (is.symbol(branch) && fun_ns(rlang::as_string(branch), env = env) == ns){
#       out <- TRUE
#       break
#     }
#   }
#   out
# }

# Tidyselect col positions with names
tidy_select_pos <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  check_cols(dots_length(...), .cols = .cols)
  # Method for when cols is supplied
  if (!is.null(.cols)){
    out <- col_select_pos(data, .cols)
  } else {
    dots <- rlang::quos(...)
    labels <- quo_labels(dots, named = TRUE)
    if (all(labels %in_% data_nms)){
      out <- add_names(match(labels, data_nms), names(labels))
      is_dup <- collapse::fduplicated(list(names(out), unname(out)))
      out <- out[!is_dup]
      if (anyDuplicated(names(out))){
        # Use tidyselect for error
        tidyselect::eval_select(rlang::expr(c(...)), data = data)
      }
    } else {
      out <- tidyselect::eval_select(rlang::expr(c(...)), data = data)
    }
  }
  out
}
# Select variables utilising tidyselect notation
tidy_select_names <- function(data, ..., .cols = NULL){
  pos <- tidy_select_pos(data, ..., .cols = .cols)
  add_names(names(data)[match(pos, seq_along(data))], names(pos))
}

mutate_summary <- function(.data, ...,
                            .keep = c("all", "used", "unused", "none"),
                            .by = NULL){
  .keep <- rlang::arg_match(.keep)
  original_cols <- names(.data)
  by_expr <- rlang::enquo(.by)
  if (rlang::quo_is_null(by_expr)){
    data <- .data
  } else {
    data <- f_group_by(.data, .by = !!by_expr, .add = TRUE)
  }
  group_vars <- group_vars(data)
  quos <- fastplyr_quos(..., .named = TRUE, .data = data)

  if (cpp_any_quo_contains_ns(quos, "dplyr")){
    new_data <- as.list(dplyr::mutate(data, !!!quos, .keep = "none"))[names(quos)]
  } else {
    new_data <- as.list(cpp_grouped_eval_mutate(data, quos))
  }

  new_data <- list_rm_null(new_data)

  data <- cheapr::reconstruct(data, .data)
  out_data <- df_add_cols(data, new_data)
  new_cols <- names(new_data)
  all_cols <- names(out_data)
  common_cols <- fast_intersect(original_cols, new_cols)
  changed <- cpp_frame_addresses_equal(
    cheapr::sset_col(data, common_cols),
    new_data[common_cols]
  )
  changed_cols <- common_cols[cheapr::val_find(changed, FALSE)]
  used_cols <- cpp_quo_data_vars(quos, data)
  used_cols <- c(used_cols, fast_setdiff(new_cols, used_cols))
  unused_cols <- fast_setdiff(original_cols, new_cols)

  keep_cols <- switch(.keep,
                      all = all_cols,
                      none = new_cols,
                      used = used_cols,
                      unused = unused_cols)

  # Add missed group vars and keep original ordering
  keep_cols <- fast_intersect(all_cols, c(group_vars, keep_cols))
  out_data <- cheapr::sset_df(out_data, j = keep_cols)
  list(
    data = out_data,
    new_cols = new_cols,
    used_cols = used_cols,
    unused_cols = unused_cols,
    changed_cols = changed_cols
  )
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
    extra_groups <- fast_setdiff(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, fast_setdiff(extra_groups, group_vars))
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
  group_vars <- get_groups(data, {{ .by }})
  extra_groups <- character()
  if (ungroup){
    out <- df_ungroup(data)
  } else {
    out <- data
  }
  # Data-masking for dots expressions
  if (dots_length(...) > 0){
    out_info <- mutate_summary(out, ..., .by = {{ .by }})
    out <- out_info[["data"]]
    extra_groups <- out_info[["new_cols"]]
  } else {
    out_info <- NULL
  }
  if (unique_groups){
    extra_groups <- fast_setdiff(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, fast_setdiff(extra_groups, group_vars))
  }
  if (is.null(out_info)){
    address_equal <- add_names(logical(length(names(data))), names(data))
  } else {
    address_equal <- add_names(is.na(match(names(data), out_info[["changed_cols"]])), names(data))
  }
  list("data" = out,
       "dplyr_groups" = group_vars,
       "extra_groups" = extra_groups,
       "all_groups" = all_groups,
       "groups_changed" = !all(address_equal[group_vars]),
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

check_rowwise <- function(data){
  if (inherits(data, "rowwise_df")){
    cli::cli_abort("fastplyr cannot handle {.arg rowwise_df}, please use {.fn f_rowwise}")
  }
}

# A fastplyr version of reframe
# About half-way there (unfortunately not super fast)

# fast_reframe <- function(data, ..., .by = NULL, .order = df_group_by_order_default(data)){
#   quos <- fastplyr_quos(..., .named = TRUE)
#   by_quo <- rlang::enquo(.by)
#   temp <- data
#   if (!rlang::quo_is_null(by_quo)){
#     temp <- f_group_by(
#       temp, .by = !!by_quo, .add = TRUE, .order = .order
#     )
#   }
#
#   if (any(
#     vapply(
#       quos, \(x)
#       cpp_call_contains_ns(x, "dplyr", rlang::quo_get_env(x)), FALSE
#     )
#   )){
#     return(
#       as_tbl(dplyr::reframe(data, ..., .by = {{  .by }}))
#     )
#   }
#
#   cpp_grouped_eval_tidy(
#     temp, quos, as_df = FALSE, check_size = FALSE
#   )
#
# }

# fast_reframe <- function(data, ..., .by = NULL, .order = df_group_by_order_default(data)){
#   quos <- fastplyr_quos(..., .named = TRUE)
#   by_quo <- rlang::enquo(.by)
#   temp <- data
#   if (!rlang::quo_is_null(by_quo)){
#     temp <- f_group_by(
#       temp, .by = !!by_quo, .add = TRUE, .order = .order
#     )
#   }
#
#   if (any(
#     vapply(
#       quos, \(x)
#       cpp_call_contains_ns(x, "dplyr", rlang::quo_get_env(x)), FALSE
#     )
#   )){
#     return(
#       as_tbl(dplyr::reframe(data, ..., .by = {{  .by }}))
#     )
#   }
#
#   groups <- group_data(temp)
#
#   results <- cpp_grouped_eval_tidy(
#     groups, data, quos, as_df = TRUE, check_size = FALSE
#   )
#   out <- f_bind_rows(results)
#
#   if (df_ncol(groups) > 1){
#     reframed_groups <- df_rep(
#       cheapr::sset_col(groups, seq_len(df_ncol(groups) - 1L)),
#       cpp_frame_dims(results, FALSE, FALSE)[[1L]]
#     )
#     out <- f_bind_cols(reframed_groups, out)
#   }
#   as_tbl(out)
# }
fast_mutate <- function(data, ...,  .by = NULL, .keep = "all"){
  out <- data %>%
    f_group_by(.by = {{ .by }}, .add = TRUE) %>%
    mutate_summary(..., .keep = .keep)
  out[["data"]]
}

