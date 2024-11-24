
# Somewhat safer check of the .by arg
# e.g mutate(group_by(iris, Species), .by = any_of("okay"))
# Should not produce an error with this check
check_by <- function(data, .by){
  if (!rlang::quo_is_null(rlang::enquo(.by))){
    if (inherits(data, "grouped_df")){
      by_nms <- tidy_select_pos(data, {{ .by }})
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
      stop(".by cannot be used on a grouped_df")
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
    names(x) <- x
  } else {
    empty <- empty_str_locs(nms)
    nms[empty] <- x[empty]
    names(x) <- nms
  }
  x
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
tidy_enquos <- function(..., .dplyr = FALSE, .ignore_null = "none"){
  if (.dplyr){
    exprs <- dplyr_quosures(...)
  } else {
    exprs <- rlang::enquos(..., .ignore_empty = "all",
                           .ignore_null = .ignore_null)
  }
  nms <- names(exprs)

  # Fix empty names
  if (is.null(nms)){
    nms <- quo_labels(exprs, named = FALSE)
  } else {
    empty <- empty_str_locs(nms)
    if (length(empty)){
      nms[empty] <- quo_labels(exprs[empty], named = FALSE)
    }
  }
  names(exprs) <- nms
  exprs
}
# rlang::enquos() but quosures are always named
tidy_quos <- function(...){
  exprs <- rlang::quos(..., .ignore_empty = "all")
  nms <- names(exprs)

  # Fix empty names
  if (is.null(nms)){
    nms <- quo_labels(exprs, named = FALSE)
  } else {
    empty <- empty_str_locs(nms)
    if (length(empty)){
      nms[empty] <- quo_labels(exprs[empty], named = FALSE)
    }
  }
  names(exprs) <- nms
  exprs
}
# Recursively checks call tree for a function call from a specified namespace
# We use it to check for any dplyr functions in call tree in `eval_all_tidy`
call_contains_ns <- function(expr, ns, env = rlang::caller_env()){
  if (rlang::is_quosure(expr)){
    expr <- rlang::quo_get_expr(expr)
  }
  is_call  <- rlang::is_call(expr)
  if (!is_call){
    return(FALSE)
  }
  if (rlang::is_call(expr, ns = ns)){
    return(TRUE)
  }
  out <- FALSE
  tree <- as.list(expr)
  for (branch in tree){
    if (rlang::is_call(branch)){
      return(call_contains_ns(branch, ns, env = env))
    }
    if (is.symbol(branch) && fun_ns(rlang::as_string(branch), env = env) == ns){
      out <- TRUE
      break
    }
  }
  out
}

# Tidyselect col positions with names
tidy_select_pos <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  check_cols(dots_length(...), .cols = .cols)
  # Method for when cols is supplied
  if (!is.null(.cols)){
    out <- col_select_pos(data, .cols)
  } else {
    dots <- rlang::enquos(...)
    labels <- quo_labels(dots)
    if (all(labels %in% names(data))){
      out <- add_names(match(labels, names(data)), names(labels))
      # if (is.null(names(out))){
      #   names(out) <- labels
      # }
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
  add_names(names(data)[match(unname(pos), seq_along(data))], names(pos))
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
                      unused = names(used)[cheapr::which_(used, invert = TRUE)])
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
                      unused = names(used)[cheapr::which_(used, invert = TRUE)])
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
  address_equal <- add_names(cpp_frame_addresses_equal(
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

check_rowwise <- function(data){
  if (inherits(data, "rowwise_df")){
    stop("fastplyr cannot handle `rowwise_df`, please use `f_rowwise()`")
  }
}

# `eval_tidy()` that works in a dplyr context for data frames
# which means that calls to dplyr functions like `across`, `pick`, `n()`
# can be supplied directly here
# `eval_all_tidy` is like `reframe` except the output is a list of
# arbitrary size expressions which makes it very flexible

# To do this properly a data mask is created with each expression result
# being added to the data mask, while using that data mask to evaluate the
# expressions, allowing for sequential dependent evaluation
# so that e.g. `eval_all_tidy(iris, x = 1, y = x)` can work

# To get this to work, eval_tidy must be supplied inside dplyr::reframe
# But we can use eval_tidy directly, so long as data is not grouped and
# None of the expressions are dplyr calls
# We use `call_contains_ns()` to check if any of the calls are to dplyr
# functions

eval_all_tidy <- function(data, ...){
  quos <- tidy_enquos(...)
  # quos <- tidy_quos(...)
  expr_names <- names(quos)
  group_vars <- group_vars(data)
  n_groups <- length(group_vars)

  data_env <- rlang::as_environment(data)
  data_mask <- rlang::new_data_mask(data_env)
  data_mask$.data <- rlang::as_data_pronoun(data_env)
  out <- cheapr::new_list(length(quos))

  # Loop over the expressions
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    expr <- rlang::quo_get_expr(quo)
    expr_name <- expr_names[i]
    env <- rlang::quo_get_env(quo)
    if (n_groups == 0 && !call_contains_ns(expr, "dplyr", env = env)){
      result <- rlang::eval_tidy(expr, data_mask, env)
      data_env[[expr_name]] <- result
    } else {
      if (n_groups == 0){
        result <- dplyr::reframe(
          data,
          !!expr_name := rlang::eval_tidy(expr, data_mask, env)
        )
        result <- .subset2(result, df_ncol(result))
        data_env[[expr_name]] <- result
      } else {
        #  Fix this later as new objects don't take precedence over data variables here
        # e.g. eval_all_tidy(group_by(data, x), v1 = 1, v2 = v1)
        result <- dplyr::reframe(
          data, !!expr_name := !!quo
        )
        # result <- dplyr::reframe(
        #   data,
        #   !!expr_name := rlang::eval_tidy(expr, data_mask, env)
        # )
      }
    }
    if (!is.null(result)){
      out[[i]] <- result
    }
    names(out)[i] <- expr_name
  }
  out
}

# We can get functions like `f_expand` to work  using this recursively
# e.g. `reframe(data, f_expand(data = pick(everything()), ...))`
# That is unfortunately very slow so basically unuseable
eval_all_tidy_ungrouped <- function(data, ...){
  quos <- tidy_enquos(...)
  # quos <- tidy_quos(...)
  expr_names <- names(quos)

  data_env <- rlang::as_environment(data)
  data_mask <- rlang::new_data_mask(data_env)
  data_mask$.data <- rlang::as_data_pronoun(data_env)
  out <- cheapr::new_list(length(quos))

  # Loop over the expressions
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    expr <- rlang::quo_get_expr(quo)
    expr_name <- expr_names[i]
    env <- rlang::quo_get_env(quo)
    if (!call_contains_ns(expr, "dplyr", env = env)){
      result <- rlang::eval_tidy(expr, data_mask, env)
    } else {
      result <- dplyr::reframe(
        data,
        !!expr_name := rlang::eval_tidy(expr, data_mask, env)
      )
      result <- .subset2(result, df_ncol(result))
    }
    data_env[[expr_name]] <- result

    if (!is.null(result)){
      out[[i]] <- result
    }
    names(out)[i] <- expr_name
  }
  out
}

as_list_of_frames <- function(x){
  for (i in seq_along(x)){
    if (!inherits(x[[i]], "data.frame")){
      x[[i]] <- list_as_tbl(x[i])
    }
  }
  x
}

dynamic_list <- function(..., .keep_null = TRUE, .named = FALSE){
  quos <- rlang::enquos(..., .ignore_empty = "all")
  # quos <- rlang::quos(..., .ignore_empty = "all")
  quo_nms <- names(quos)
  out <- cheapr::new_list(length(quos))

  # quo_nms2 is for assigning objs to our new environment and so
  # they can't be empty strings
  quo_nms2 <- quo_nms

  if (is.null(quo_nms2)){
    quo_nms2 <- quo_labels(quos, named = FALSE)
  } else {
    empty <- empty_str_locs(quo_nms2)
    if (length(empty) > 0){
      quo_nms2[empty] <- quo_labels(quos[empty], named = FALSE)
    }
  }
  if (.named){
    names(out) <- quo_nms2
  } else {
    names(out) <- quo_nms
  }
  new_env <- list2env(list(), parent = emptyenv())
  mask <- rlang::new_data_mask(new_env)
  mask$.data <- rlang::as_data_pronoun(new_env)

  for (i in seq_along(quos)){
    result <- rlang::eval_tidy(quos[[i]], mask)
    new_env[[quo_nms2[[i]]]] <- result
    if (!is.null(result)){
      out[[i]] <- result
    }
  }
  if (!.keep_null){
    out <- list_rm_null(out)
  }
  out
}
