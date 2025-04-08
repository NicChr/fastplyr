
# Somewhat safer check of the .by arg
# e.g mutate(group_by(iris, Species), .by = any_of("okay"))
# Should not produce an error with this check
check_by <- function(data, .by){
  if (inherits(data, "grouped_df") && !rlang::quo_is_null(rlang::enquo(.by))){
    cli::cli_abort("{.arg .by} cannot be used on a {.cls grouped_df}")
  }
}
check_cols <- function(n_dots, .cols = NULL){
  if (n_dots > 0 && !is.null(.cols)){
    cli::cli_abort("Cannot supply variables through {.arg ...} and {.arg .cols}, use one argument")
  }
}

# This function returns the groups of a data frame
get_groups <- function(data, .by = NULL, named = FALSE){
  check_rowwise(data)
  dplyr_groups <- group_vars(data)
  if (named){
    names(dplyr_groups) <- dplyr_groups
  }
  if (rlang::quo_is_null(rlang::enquo(.by))){
    by_groups <- character()
  } else {
    by_groups <- tidy_select_names(data, {{ .by }})
    if (any(names(by_groups) != by_groups)){
      cli::cli_abort("Can't rename groups through {.arg .by}")
    }
    if (!named){
      attr(by_groups, "names") <- NULL
    }
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
  `names<-`(x, str_coalesce(names(x), as.character(x)))
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

fix_quo_names <- function(quos){

  nms <- names(quos)

  if (is.null(nms)){
    nms <- quo_labels(exprs, named = FALSE)
  } else {
    for (i in seq_along(quos)){
      if (!nzchar(nms[[i]])){
        expr <- rlang::quo_get_expr(quos[[i]])
        if (is.symbol(expr)){
          nms[i] <- rlang::as_string(expr)
        } else {
          nms[i] <- deparse2(rlang::quo_get_expr(quos[[i]]))
        }
      }
    }
  }
  names(quos) <- nms
  quos
}


is_fn_call <- function(quo, fn, ns = NULL){
  cpp_is_fn_call(
    rlang::quo_get_expr(quo), fn, ns,
    rlang::quo_get_env(quo)
  )
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
  unused_args <- fast_setdiff(names(clean_expr)[-1], c(".cols", ".fns", ".names", ".unpack"))

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
    cols <- unname(col_select_names(data, across_vars))
  } else if (rlang::is_call(across_vars, ":")){
    args <- as.list(across_vars[-1L])
    if (length(args) == 2 && is.atomic(args[[1L]]) && is.atomic(args[[2L]])){
      cols <- unname(col_select_names(data, eval(across_vars, envir = quo_env)))
    } else {
      cols <- names(tidyselect::eval_select(across_vars, data))
    }
  } else if (is.symbol(across_vars) && (rlang::as_string(across_vars) %in% names(data))){
    cols <- rlang::as_string(across_vars)
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
  } else {
    unpack <- FALSE
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
    new_quo <- rlang::new_quosure(rlang::call2(fn, as.symbol(col)), quo_env)
    set_add_attr(new_quo, ".unpack", unpack)
    out[[i]] <- new_quo

  }
  out
}

fastplyr_quos <- function(..., .named = TRUE, .data = NULL, .drop_null = FALSE,
                          .unpack_default = FALSE){

  out <- rlang::quos(..., .ignore_empty = "all")
  quo_nms <- attr(out, "names", TRUE)

  if (is.null(.data)){
    for (i in seq_along(out)){
      attr(out[[i]], ".unpack", TRUE) %||% set_add_attr(out[[i]], ".unpack", .unpack_default)
      if (.named && !nzchar(quo_nms[[i]])){
        quo_nms[[i]] <- deparse2(rlang::quo_get_expr(out[[i]]))
      }
    }
  } else {
    k <- 1L
    for (i in seq_along(out)){
      quo <- out[[k]]
      attr(quo, ".unpack", TRUE) %||% set_add_attr(quo, ".unpack", .unpack_default)
      if (!nzchar(quo_nms[[k]]) && is_fn_call(quo, "across", ns = "dplyr")){
        left <- out[seq_len(k - 1L)]
        unpacked_quos <- unpack_across(quo, .data)
        if (k < length(out)){
          right <- out[seq.int(k + 1L, length(out), 1L)]
        } else {
          right <- list()
        }
        out[[k]] <- NULL
        out <- c(left, unpacked_quos, right)
        quo_nms <- names(out)
        k <- k + length(right)
      } else if (.named && !nzchar(quo_nms[[k]])){
        quo_nms[[k]] <- deparse2(rlang::quo_get_expr(quo))
        k <- k + 1L
      } else {
       k <- k + 1L
      }
    }
  }
  names(out) <- quo_nms
  if (.drop_null){
    out <- cpp_quos_drop_null(out)
  }
  set_add_attr(out, ".fastplyr_quos", TRUE)
  out
}

are_fastplyr_quos <- function(quos){
  isTRUE(attr(quos, ".fastplyr_quos", TRUE))
}
check_fastplyr_quos <- function(quos){
  if (!are_fastplyr_quos(quos)){
    cli::cli_abort("{.arg quos} must be built using {.fn fastplyr_quos}")
  }
}


# Tidyselect col positions with names
tidy_select_pos <- function(data, ..., .cols = NULL){
  data_nms <- names(data)
  check_cols(dots_length(...), .cols = .cols)
  # Method for when cols is supplied
  if (!is.null(.cols)){
    out <- col_select_pos(data, .cols)
  } else {
    dots <- rlang::quos(...)
    select_cols <- character(length(dots))
    for (i in seq_along(dots)){
      expr <- rlang::quo_get_expr(dots[[i]])
      if (is.symbol(expr)){
        select_cols[i] <- rlang::as_string(expr)
      }
    }
    out <- match(select_cols, data_nms)
    if (!anyNA(out)){
     names(out) <- names(dots)
     empty <- cheapr::val_find(nzchar(names(out)), FALSE)
     names(out)[empty] <- data_nms[out[empty]]
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
                           .keep = "all",
                           .by = NULL,
                           .order = df_group_by_order_default(.data),
                           .add_collapse_grp = FALSE){
  original_cols <- names(.data)
  if (rlang::quo_is_null(rlang::enquo(.by))){
    all_groups <- group_vars(.data)
    data <- .data
  } else {
    all_groups <- get_groups(.data, .by = {{ .by }})
    data <- f_group_by(.data, .cols = all_groups, .add = TRUE, .order = .order)
  }
  GRP <- NULL
  if (length(all_groups) > 0L && .add_collapse_grp){
    GRP <- grouped_df_as_GRP(data)
  }
  if (dots_length(...) == 0L){
    out_data <- .data
    new_cols <- character()
    used_cols <- character()
    unused_cols <- original_cols
    changed_cols <- character()
  } else {
    quos <- fastplyr_quos(..., .named = TRUE, .data = data)

    if (cpp_any_quo_contains_ns(quos, "dplyr")){
      new_data <- as.list(dplyr::mutate(data, !!!quos, .keep = "none"))[names(quos)]
    } else {
      new_data <- as.list(cpp_grouped_eval_mutate(data, quos))
    }

    # Removing duplicate named results
    new_data <- new_data[!duplicated(names(quos), fromLast = TRUE)]
    new_data <- cheapr::list_drop_null(new_data)
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
    keep_cols <- fast_intersect(all_cols, c(all_groups, keep_cols))
    out_data <- cheapr::sset_df(out_data, j = keep_cols)
  }
  list(
    data = out_data,
    new_cols = new_cols,
    used_cols = used_cols,
    unused_cols = unused_cols,
    changed_cols = changed_cols,
    all_groups = all_groups,
    GRP = GRP
  )
}
tidy_group_info_tidyselect <- function(data, ..., .by = NULL, .cols = NULL,
                                       ungroup = TRUE, rename = TRUE,
                                       unique_groups = TRUE){
  group_vars <- get_groups(data, {{ .by }})
  group_pos <- match(group_vars, names(data))
  extra_groups <- character()
  if (ungroup){
    out <- cpp_ungroup(data)
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

# tidy_group_info_tidyselect <- function(data, ..., .by = NULL, .cols = NULL,
#                                        ungroup = TRUE, rename = TRUE,
#                                        unique_groups = TRUE){
#   data_names <- names(data)
#   group_vars <- get_groups(data, {{ .by }}, named = TRUE)
#   if (ungroup){
#     out <- cpp_ungroup(data)
#   } else {
#     out <- data
#   }
#   extra_groups <- tidy_select_names(out, ..., .cols = .cols)
#   if (!rename){
#     names(extra_groups) <- drop_names(extra_groups)
#     names(group_vars) <- drop_names(group_vars)
#     any_groups_changed <- FALSE
#   } else {
#     out <- f_rename(out, .cols = extra_groups)
#     group_vars <- names(group_vars)
#     any_groups_changed <- any(names(group_vars) != group_vars)
#   }
#
#   renamed <- names(extra_groups) != extra_groups
#   address_equal <- rep_len(TRUE, length(data_names))
#   address_equal[match(extra_groups[renamed], data_names)] <- FALSE
#   names(address_equal) <- data_names
#   extra_groups <- drop_names(extra_groups)
#   group_vars <- fast_intersect(extra_groups, group_vars)
#   if (unique_groups){
#     extra_groups <- fast_setdiff(extra_groups, group_vars)
#     all_groups <- c(group_vars, extra_groups)
#   } else {
#     all_groups <- c(group_vars, fast_setdiff(extra_groups, group_vars))
#   }
#   list(data = out,
#        dplyr_groups = group_vars,
#        extra_groups = extra_groups,
#        all_groups = all_groups,
#        groups_changed = any_groups_changed,
#        address_equal = address_equal)
# }

tidy_group_info_datamask <- function(data, ..., .by = NULL,
                                     ungroup = TRUE,
                                     unique_groups = TRUE){
  group_vars <- get_groups(data, {{ .by }})
  extra_groups <- character()
  if (ungroup){
    out <- cpp_ungroup(data)
  } else {
    out <- data
  }
  # Data-masking for dots expressions
  if (dots_length(...) > 0){
    out_info <- mutate_summary(out, ..., .by = {{ .by }})
    out <- out_info[["data"]]
    extra_groups <- out_info[["new_cols"]]
    GRP <- out_info[["GRP"]]
  } else {
    out_info <- NULL
    GRP <- NULL
  }
  if (unique_groups){
    extra_groups <- fast_setdiff(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, fast_setdiff(extra_groups, group_vars))
  }
  if (is.null(out_info)){
    changed_groups <- character()
    address_equal <- add_names(logical(length(names(data))), names(data))
  } else {
    changed_groups <- fast_intersect(names(data), out_info[["changed_cols"]])
    address_equal <- add_names(is.na(match(names(data), out_info[["changed_cols"]])), names(data))
  }
  list(
    data = out,
    dplyr_groups = group_vars,
    extra_groups = extra_groups,
    all_groups = all_groups,
    changed_groups = changed_groups,
    groups_changed = !all(address_equal[group_vars]),
    address_equal = address_equal,
    GRP = GRP
  )
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

eval_all_tidy <- function(.data, quos, recycle = FALSE, unique_names = FALSE){
  check_fastplyr_quos(quos)
  quo_names <- names(quos)
  all_results <- cpp_grouped_eval_tidy(.data, quos, recycle = recycle)
  groups <- all_results[[1L]]
  results <- all_results[[2L]]
  n_group_vars <- length(group_vars(.data))

  k <- 1L
  for (i in seq_along(results)){
    # Unpack
    if (attr(quos[[i]], ".unpack", TRUE) && is_df(results[[k]])){
      results_to_append <- as.list(results[[k]])
      # if (nzchar(quo_names[[i]])){
      #   names(results_to_append) <- paste(quo_names[[i]], names(results_to_append), sep = "_")
      # }
      results <- append(results, results_to_append, after = k - 1L)
      k <- k + length(results_to_append)
      results[[k]] <- NULL
    } else {
      k <- k + 1L
    }
  }
  if (unique_names){
    # Removing duplicate named results
    keep <- cheapr::val_find(duplicated(quo_names, fromLast = TRUE), FALSE)
    groups <- groups[keep]
    results <- results[keep]
  }
  list(groups = groups, results = results)
}

# A fastplyr version of reframe
# About half-way there (unfortunately not super fast)
f_reframe <- function(.data, ..., .by = NULL, .order = df_group_by_order_default(.data)){

  if (missing(.by)){
    data <- .data
  } else {
    data <- f_group_by(.data, .by = {{ .by }}, .add = TRUE, .order = .order)
  }
  quos <- fastplyr_quos(..., .data = data, .drop_null = TRUE, .named = TRUE,
                        .unpack_default = TRUE)

  if (length(quos) == 0){
    return(cheapr::reconstruct(group_keys(data), cpp_ungroup(.data)))
  }
  if (cpp_any_quo_contains_ns(quos, "dplyr")){
    out <- dplyr::reframe(data, ...)
  } else {
    results <- eval_all_tidy(data, quos, recycle = TRUE, unique = TRUE)
    groups <- results[["groups"]]
    results <- results[["results"]]
    n_group_vars <- length(group_vars(data))
    if (n_group_vars == 0){
      groups <- cheapr::new_df(.nrows = cheapr::vector_length(results[[1L]]))
    } else {
      groups <- list_as_df(groups[[1L]])
    }
    out <- cheapr::col_c(groups, results, .name_repair = TRUE, .recycle = FALSE)
  }
  cheapr::reconstruct(out, cpp_ungroup(.data))
}

f_mutate <- function(.data, ...,  .by = NULL, .order = df_group_by_order_default(.data), .keep = "all"){
  out <- .data %>%
    mutate_summary(..., .keep = .keep, .order = .order, .by = {{ .by }})
  out[["data"]]
}

