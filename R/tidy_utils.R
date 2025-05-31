
.optimised_fn_list <- list(

  input_fns = list(
    base::sum, base::prod, base::mean, stats::median,
    base::min, base::max,
    dplyr::first, dplyr::last,
    stats::sd, stats::var,
    dplyr::n_distinct, collapse::fndistinct,
    collapse::fsum, collapse::fprod, collapse::fmean,
    collapse::fmedian,
    collapse::fmin, collapse::fmax,
    collapse::ffirst, collapse::flast,
    collapse::fsd, collapse::fvar,
    collapse::fndistinct
  ),
  input_fn_nms = c(
    "sum", "prod", "mean", "median", "min", "max", "first", "last",
    "sd", "var", "n_distinct", "ndistinct", "fsum", "fprod", "fmean",
    "fmedian", "fmin", "fmax", "ffirst", "flast", "fsd", "fvar",
    "fndistinct"
  ),
  input_ns = c(
    "", "", "base", "stats", "", "", "dplyr", "dplyr",
    "stats", "stats", "dplyr", "collapse", "collapse", "collapse", "collapse",
    "collapse", "collapse", "collapse", "collapse", "collapse", "collapse", "collapse",
    "collapse"
  ),
  target_fns = list(
    collapse::fsum, collapse::fprod, collapse::fmean, collapse::fmedian,
    collapse::fmin, collapse::fmax,
    grouped_first, grouped_last,
    collapse::fsd, collapse::fvar,
    collapse::fndistinct, collapse::fndistinct,
    collapse::fsum, collapse::fprod, collapse::fmean,
    collapse::fmedian,
    collapse::fmin, collapse::fmax,
    grouped_first, grouped_last,
    collapse::fsd, collapse::fvar,
    collapse::fndistinct
  )
)
.optimised_fns_inform <- c(
  "sum", "prod", "mean", "median", "min", "max", "sd", "var",
  "dplyr::n", "dplyr::first", "dplyr::last", "dplyr::n_distinct",
  "dplyr::row_number", "dplyr::lag", "dplyr::lead",
  "dplyr::cur_group", "dplyr::cur_group_id", "dplyr::cur_group_rows"
)

is_optimised_call <- function(expr, env = rlang::caller_env()){
  cpp_is_fn_call(expr, .optimised_fn_list[["input_fn_nms"]],  NULL, env)
}

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
  dplyr_groups <- f_group_vars(data)
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
  `names<-`(x, cheapr::str_coalesce(names(x), as.character(x)))
}

call_args <- function(x){
  if (typeof(x) != "language"){
    cli::cli_abort("{.arg x} must be a call")
  }
  out <- as.list(x)[-1L]
  names(out) <- names(out) %||% character(length(out))
  out
}

tidy_as_list_of <- function (..., .keep_null = FALSE){
  dots <- list_tidy(...)
  if (length(dots) == 1 && !is.object(dots[[1L]]) && is.list(dots[[1L]])) {
      out <- dots[[1L]]
  }
  else {
    out <- dots
  }
  if (!.keep_null){
    out <- cheapr::list_drop_null(out)
  }
  out
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
    nms <- quo_labels(quos, named = FALSE)
  } else {
    for (i in seq_along(quos)){
      if (!nzchar(nms[[i]])){
        expr <- rlang::quo_get_expr(quos[[i]])
        if (is.symbol(expr)){
          nms[i] <- rlang::as_string(expr)
        } else {
          nms[i] <- deparse2(expr)
        }
      }
    }
  }
  names(quos) <- nms
  quos
}

unpack_across <- function(quo, data, unpack_default = FALSE){

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
  unused_args <- vec_setdiff(names(clean_expr)[-1], c(".cols", ".fns", ".names", ".unpack"))

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
    args <- as.list(across_vars)[-1L]
    if (length(args) == 2 && is.atomic(args[[1L]]) && is.atomic(args[[2L]])){
      cols <- unname(col_select_names(data, eval(across_vars, envir = quo_env)))
    } else {
      cols <- names(tidyselect::eval_select(across_vars, data, env = quo_env))
    }
  } else if (is.symbol(across_vars) && (rlang::as_string(across_vars) %in% names(data))){
    cols <- rlang::as_string(across_vars)
  } else {
    cols <- names(tidyselect::eval_select(across_vars, data, env = quo_env))
  }

  if (rlang::is_call(across_fns, "list")){
    fn_tree <- as.list(across_fns)[-1L]
    fn_names <- names(fn_tree) %||% character(length(fn_tree))

    # This line is to not break timeplyr unit tests
    if (!all(nzchar(fn_names))){
      fn_names <- cheapr::str_coalesce(
        fn_names,
        vapply(fn_tree, deparse2, "", USE.NAMES = FALSE)
      )
    }
  } else if (!".fns" %in% names(clean_expr)){
    # fn_tree <- list(identity)
    fn_tree <- list(as.symbol("identity"))
  } else {
    fn_tree <- list(across_fns)
  }

  # Evaluate functions

  for (i in seq_along(fn_tree)){
    evaluated_fn <- eval(fn_tree[[i]], envir = quo_env)
    if (!is.function(evaluated_fn)){
      fn_tree[[i]] <- rlang::as_function(evaluated_fn)
    }
  }

  if (".unpack" %in% names(expr)){
    unpack <- eval(across_unpack, envir = quo_env)
  } else {
    unpack <- unpack_default
  }

  out_names <- across_col_names(cols, .names = across_nms, .fns = fn_names)
  out_size <- length(out_names)

  # Recycle cols/fns
  cols <- cheapr::cheapr_rep_each(cols, out_size / length(cols))
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

fastplyr_quos <- function(..., .data, .groups = NULL, .named = TRUE, .drop_null = FALSE,
                          .unpack_default = FALSE, .optimise = FALSE,
                          .optimise_expand = FALSE){

  out <- rlang::quos(..., .ignore_empty = "all")
  quo_nms <- attr(out, "names", TRUE)
  k <- 1L
  for (i in seq_along(out)){
    quo <- out[[k]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    attr(quo, ".unpack", TRUE) %||% set_add_attr(quo, ".unpack", .unpack_default)
    if (!nzchar(quo_nms[[k]]) && cpp_is_fn_call(expr, "across", ns = "dplyr", env)){
      left <- out[seq_len(k - 1L)]
      left_nms <- quo_nms[seq_len(k - 1L)]
      unpacked_quos <- unpack_across(quo, .data, unpack_default = .unpack_default)
      if (k < length(out)){
        right <- out[seq.int(k + 1L, length(out), 1L)]
        right_nms <- quo_nms[seq.int(k + 1L, length(quo_nms), 1L)]
      } else {
        right <- list()
        right_nms <- character()
      }
      out[[k]] <- NULL
      out <- c(left, unpacked_quos, right)
      quo_nms <- c(left_nms, names(unpacked_quos), right_nms)
      k <- k + length(unpacked_quos)
    } else if (.named && !nzchar(quo_nms[[k]])){
      quo_nms[[k]] <- deparse2(rlang::quo_get_expr(quo))
      k <- k + 1L
    } else {
      k <- k + 1L
    }
  }
  names(out) <- quo_nms
  if (.drop_null){
    out <- cpp_quos_drop_null(out)
  }

  optimised <- logical(length(out))

  if (is.null(.groups)){
    n_groups <- 1L
    group_vars <- character()
  } else {
    n_groups <- GRP_n_groups(.groups)
    group_vars <- GRP_group_vars(.groups)
  }

  if (n_groups <= 1){
    .fastplyr.g <- NULL
  } else {
    .fastplyr.g <- .groups
  }
  .original_out <- out

  # Second pass to check for optimised calls
  if (.optimise && getOption("fastplyr.optimise", TRUE)){

    inform <- getOption("fastplyr.inform")
    if (inform %||% TRUE){
      cli::cli_inform(
        c(
          "!" = "The following functions will be optimised package-wide:",
          paste0("`", paste(.optimised_fns_inform, sep = "``"), "`"),
          "",
          "Optimised expressions are independent from each other and typical data-masking rules may not apply",
          "",
          "Run {.run options(fastplyr.optimise = FALSE)} to disable optimisations globally",
          "",
          "Run {.run options(fastplyr.inform = FALSE)} to disable this message"
        ),
        .frequency = "once", .frequency_id = ".optimise_inform"
      )
    }

    if (is.null(inform)){
      options(fastplyr.inform = FALSE)
    }

    if (.optimise_expand){
      TRA <- "replace_fill"
    } else {
      TRA <- NULL
    }
    for (i in seq_along(out)){
      quo <- out[[i]]
      expr <- rlang::quo_get_expr(quo)
      env <- rlang::quo_get_env(quo)

      if (is_nested_call(expr)) next

      ### Cases when we are just selecting columns
      ### We can just optimise it away by leaving the expression as is
      ### and then running it through `eval_optimised_quos()`
      if (is.name(expr) && rlang::as_string(expr) == quo_nms[[i]]){

      } else if (.optimise_expand && cpp_is_fn_call(expr, "identity", "base", env)){

      } else if (cpp_is_fn_call(expr, "n", "dplyr", env)){
        expr <- rlang::call2(\(){
          if (is.null(.fastplyr.g)){
            out <- df_nrow(.data)
            if (.optimise_expand){
              out <- cheapr::cheapr_rep_len(out, out)
            }
          } else {
            out <- GRP_group_sizes(.fastplyr.g)
            if (.optimise_expand){
              out <- out[GRP_group_id(.fastplyr.g)]
            }
          }
          out
        })
      } else if (.optimise_expand && cpp_is_fn_call(expr, "row_number", "dplyr", env)){
        expr <- rlang::call2(\(){
          if (is.null(.fastplyr.g)){
            seq_len(df_nrow(.data))
          } else {
            cpp_row_id(GRP_order(.fastplyr.g), GRP_group_sizes(.fastplyr.g), TRUE)
          }
        })
      } else if (cpp_is_fn_call(expr, "cur_group_id", "dplyr", env)){
        expr <- rlang::call2(\(){
          if (is.null(.fastplyr.g)){
            if (.optimise_expand){
              cheapr::cheapr_rep_len(1L, df_nrow(.data))
            } else {
              if (df_nrow(.data) == 0L){
                integer()
              } else {
                1L
              }
            }
          } else {
            if (.optimise_expand){
              GRP_group_id(.fastplyr.g)
            } else {
              GRP_group_id(.fastplyr.g)[GRP_starts(.fastplyr.g)]
            }
          }
        })
      } else if (cpp_is_fn_call(expr, "cur_group", "dplyr", env)){
        expr <- rlang::call2(\(){
          if (is.null(.fastplyr.g)){
            if (.optimise_expand){
              new_tbl(.nrows = df_nrow(.data))
            } else {
              if (df_nrow(.data) == 0L){
                new_tbl()
              } else {
                f_group_keys(.data)
              }
            }
          } else {
            if (.optimise_expand){
              as_tbl(
                cheapr::sset_row(GRP_groups(.fastplyr.g), GRP_group_id(.fastplyr.g))
              )
            } else {
              GRP_groups(.fastplyr.g)
            }
          }
        })
      } else if (.optimise_expand && cpp_is_fn_call(expr, "cur_group_rows", "dplyr", env)){
        expr <- rlang::call2(\(){
          if (is.null(.fastplyr.g)){
            seq_len(df_nrow(.data))
          } else {
            locs <- GRP_loc(.fastplyr.g)
            out <- cpp_unlist_group_locs(locs, GRP_group_sizes(.fastplyr.g))
            order <- cpp_orig_order(GRP_group_id(.fastplyr.g),
                                    GRP_group_sizes(.fastplyr.g))
            out[order]
          }
        })
      } else if (.optimise_expand && cpp_is_fn_call(expr, "lag", "dplyr", env)){
        if (!all_blank(vec_setdiff(names(args), c("x", "n", "default", "order_by")))){
          next
        }
        args <- call_args(expr)
        names(args)[names(args) == "default"] <- "fill"
        expr <- rlang::call2("grouped_lag", !!!args, g = .fastplyr.g)
      } else if (.optimise_expand && cpp_is_fn_call(expr, "lead", "dplyr", env)){
        if (!all_blank(vec_setdiff(names(args), c("x", "n", "default", "order_by")))){
          next
        }
        args <- call_args(expr)
        names(args)[names(args) == "default"] <- "fill"
        expr <- rlang::call2("grouped_lead", !!!args, g = .fastplyr.g)
      } else if (is_optimised_call(expr, env)){
        args <- call_args(expr)
        unsupported_args <- vec_setdiff(names(args), c("x", "na.rm", "nthreads"))
        if (!all_blank(unsupported_args)){
          cli::cli_warn(c("Unsupported args: {paste(
                          unsupported_args[nzchar(unsupported_args)],
                          collapse = ', '
                          )}",
                          "Reverting to un-optimised method"))
          next
        }
        if (call_is_namespaced(expr)){
          fn <- expr[[1]][[3]]
          ns <- rlang::as_string(expr[[1]][[2]])
        } else {
          fn <- expr[[1]]
          ns <- fun_ns(fn, env)
        }
        fn <- rlang::as_string(fn)
        match_loc <- match(fn, .optimised_fn_list[["input_fn_nms"]])
        input_ns <- .optimised_fn_list[["input_ns"]][[match_loc]]
        if (input_ns != ns){
          next
        }
        fn <- .optimised_fn_list[["target_fns"]][[match_loc]]
        expr <- rlang::call2(fn, !!!args, g = .fastplyr.g, TRA = TRA, use.g.names = FALSE)
      } else {
        next
      }
      quo <- rlang::new_quosure(expr, env)
      set_add_attr(quo, ".unpack", attr(out[[i]], ".unpack", TRUE))
      optimised[i] <- TRUE
      out[[i]] <- quo
    }
  }
  if (!all(optimised)){
    ### Calculate group locations here so that
    ### tidy evaluation can be done
    ### eval_all_tidy needs a plain or grouped data frame
    if (!is.null(.groups)){
      if (identical(f_group_vars(.data), group_vars) &&
          GRP_is_ordered(.groups) == group_by_order_default(.data)){
        .groups[["locs"]] <- f_group_rows(.data)
      } else {
        .groups[["locs"]] <- GRP_loc(.groups)
      }
    }
  }
  cheapr::attrs_add(
    out,
    .optimised = optimised,
    .GRP = .groups,
    .fastplyr_quos = TRUE,
    .set = TRUE
  )
}

should_optimise <- function(GRP){
  TRUE
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

# To replace tidy_group_info_datamask in the future
mutate_summary <- function(.data, ...,
                           .keep = "all",
                           .by = NULL,
                           .order = group_by_order_default(.data),
                           .drop = df_group_by_drop_default(.data)){
  original_cols <- names(.data)
  all_groups <- get_groups(.data, .by = {{ .by }})
  if (length(all_groups) == 0){
    GRP <- NULL
    group_keys <- f_group_keys(.data)
    n_groups <- 1L
  } else {
    GRP <- df_to_GRP(.data, all_groups, order = .order)
    group_keys <- GRP_groups(GRP)
    n_groups <- GRP_n_groups(GRP)
  }
  quos <- fastplyr_quos(..., .data = .data, .drop_null = FALSE,
                        .unpack_default = FALSE,
                        .optimise = should_optimise(GRP),
                        .optimise_expand = TRUE,
                        .groups = GRP)
  if (dots_length(...) == 0L){
    out_data <- .data
    new_cols <- character()
    used_cols <- character()
    unused_cols <- original_cols
    changed_cols <- character()
  } else {
    new_data <- eval_mutate(.data, quos)
    # Removing duplicate named results
    new_data <- new_data[!duplicated(names(quos), fromLast = TRUE)]
    out_data <- cheapr::df_modify(.data, new_data)
    new_data <- cheapr::list_drop_null(new_data)
    new_cols <- names(new_data)
    all_cols <- names(out_data)
    common_cols <- vec_intersect(original_cols, new_cols)
    changed <- cpp_frame_addresses_equal(
      cheapr::sset_col(.data, common_cols),
      new_data[common_cols]
    )
    changed_cols <- common_cols[cheapr::val_find(changed, FALSE)]
    used_cols <- cpp_quo_data_vars(quos, .data)
    used_cols <- c(used_cols, vec_setdiff(new_cols, used_cols))
    unused_cols <- vec_setdiff(original_cols, new_cols)

    keep_cols <- switch(.keep,
                        all = all_cols,
                        none = new_cols,
                        used = used_cols,
                        unused = unused_cols)

    # Add missed group vars and keep original ordering
    keep_cols <- vec_intersect(all_cols, c(all_groups, keep_cols))
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

# To replace tidy_group_info_tidyselect in the future
select_summary <- function(.data, ..., .by = NULL, .cols = NULL, .order = group_by_order_default(.data)){

  all_groups <- get_groups(.data, .by = {{ .by }})
  if (length(all_groups) == 0){
    GRP <- NULL
  } else {
    GRP <- df_to_GRP(.data, all_groups, order = .order)
  }

  selected_cols <- tidy_select_names(.data, ..., .cols = .cols)
  out <- col_rename(f_ungroup(.data), .cols = selected_cols)
  if (anyDuplicated(names(out))){
    cli::cli_abort("Can't rename columns to names that already exist")
  }
  # Recalculate group vars in case they were renamed
  group_pos <- match(all_groups, names(.data))
  new_group_vars <- names(out)[group_pos]

  if (!identical(new_group_vars, all_groups)){
    cli::cli_abort("Can't rename group vars in this context")
  }
  new_cols <- names(selected_cols)
  used_cols <- new_cols
  unused_cols <- vec_setdiff(names(out), used_cols)
  changed_cols <- character()
  renamed_cols <- used_cols[used_cols != selected_cols]

  list(
    data = cheapr::rebuild(out, .data),
    new_cols = new_cols,
    used_cols = used_cols, # All selected cols
    unused_cols = unused_cols, # Unselected cols
    changed_cols = changed_cols,
    renamed_cols = renamed_cols,
    all_groups = all_groups,
    GRP = GRP
  )
}

tidy_dots_info <- function(.data, ..., .by = NULL, .cols = NULL,
                           .order = group_by_order_default(.data),
                           .type = "data-mask"){
  check_cols(n_dots = dots_length(...), .cols = .cols)
  if (is.null(.cols) && .type == "data-mask"){
    mutate_summary(.data, ..., .by = {{ .by }}, .order = .order)
  } else {
    select_summary(.data, ..., .by = {{ .by }}, .order = .order, .cols = .cols)
  }
}

# tidy_GRP applies expressions supplied through `...` or selects cols
# if either .cols is supplied or type isnt "data-mask"
# After that it calculates the grouping structure of these variables

tidy_eval_groups <- function(.data, ..., .by = NULL, .cols = NULL,
                             .order = group_by_order_default(.data),
                             .type = "data-mask",
                             return_order = FALSE){

  info <- tidy_dots_info(.data, ..., .by = {{ .by }}, .cols = .cols,
                         .order = .order, .type = .type)
  data <- info[["data"]]
  groups <- info[["all_groups"]]
  new_cols <- info[["new_cols"]]
  all_groups <- c(vec_setdiff(groups, new_cols), new_cols)
  GRP <- info[["GRP"]]
  # if (length(all_groups) == 0){
  #   GRP <- NULL
  # } else
  if (is.null(GRP) || !identical(groups, all_groups) ||
      length(vec_intersect(info[["changed_cols"]], all_groups)) > 0L){
    GRP <- df_to_GRP(data, all_groups, order = .order, return.order = return_order)
  }
  list(data = data, GRP = GRP)
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

sset_quos <- function(quos, i){
  out <- quos[i]
  set_add_attr(out, ".optimised", attr(quos, ".optimised", TRUE)[i])
  cpp_rebuild(
    out, quos, c("names", "class", ".optimised"),
    vec_setdiff(names(attributes(quos)), c("names", "class", ".optimised")),
    FALSE
  )
}

eval_optimised_quos <- function(data, quos, add_groups = TRUE){

  quo_names <- names(quos)
  quo_groups <- attr(quos, ".GRP", TRUE)
  GRP <- attr(quos, ".GRP", TRUE)
  if (!all(attr(quos, ".optimised", TRUE))){
    cli::cli_abort("All quosures must be tagged as '.optimised'")
  }

  mask <- rlang::as_data_mask(data)
  results <- lapply(quos, \(x) rlang::eval_tidy(rlang::quo_get_expr(x), mask))

  groups <- NULL

  if (add_groups){
    if (length(quos) == 0){
      groups <- list()
      names(groups) <- character()
    } else {
      groups <- cheapr::cheapr_rep_len(
        list(
          cheapr::cheapr_rep_len(GRP_groups(GRP), vector_length(results[[1]]))
        ),
        length(quos)
      )
      names(groups) <- quo_names
    }
  }
  list(groups = groups, results = results)
}

eval_all_tidy <- function(data, quos, recycle = FALSE){
  check_fastplyr_quos(quos)

  if (length(quos) == 0L){
    return(
      list(groups = `names<-`(list(), character()),
           results = `names<-`(list(), character()))
    )
  }

  GRP <- attr(quos, ".GRP", TRUE)

  quo_names <- names(quos)
  which_optimised <- cheapr::val_find(attr(quos, ".optimised", TRUE), TRUE)

  if (length(which_optimised) > 0L){
    which_regular <- cheapr::val_find(attr(quos, ".optimised", FALSE), FALSE)
    regular_quos <- sset_quos(quos, which_regular)
    optimised_quos <- sset_quos(quos, which_optimised)
    regular_results <- eval_all_tidy(data, regular_quos, recycle = FALSE)
    optimised_results <- eval_optimised_quos(data, optimised_quos, add_groups = TRUE)

    groups <- cheapr::new_list(length(quos))
    results <- cheapr::new_list(length(quos))

    groups[which_regular] <- regular_results[[1L]]
    results[which_regular] <- regular_results[[2L]]

    groups[which_optimised] <- optimised_results[[1L]]
    results[which_optimised] <- optimised_results[[2L]]

    if (recycle){
      results <- cheapr::recycle(.args = results)
      groups <- cheapr::cheapr_rep_len(
        list(cheapr::cheapr_rep_len(groups[[1]], vector_length(results[[1]]))),
        length(results)
      )
    }

    names(groups) <- quo_names
    names(results) <- quo_names

    return(list(groups = groups, results = results))
  }

  if (cpp_any_quo_contains_dplyr_mask_call(quos)){
    return(dplyr_eval_all_tidy(
      construct_fastplyr_grouped_df(data, GRP), !!!quos
    ))
  }
  all_results <- cpp_grouped_eval_tidy(
    construct_fastplyr_grouped_df(data, GRP), quos,
    recycle = recycle, add_groups = TRUE
  )
  groups <- all_results[[1L]]
  results <- all_results[[2L]]
  n_group_vars <- length(GRP_group_vars(GRP))

  k <- 1L
  for (i in seq_along(results)){
    # Unpack
    if (isTRUE(attr(quos[[i]], ".unpack", TRUE)) && is_df(results[[k]])){
      results_to_append <- as.list(results[[k]])
      if (nzchar(quo_names[[i]])){
        names(results_to_append) <- paste(quo_names[[i]], names(results_to_append), sep = "_")
      }
      results <- append(results, results_to_append, after = k - 1L)
      k <- k + length(results_to_append)
      results[[k]] <- NULL
    } else {
      k <- k + 1L
    }
  }
  list(groups = groups, results = results)
}

### In the future we hope to not have to use this
### But right now I'm not sure how to evaluate
### data-mask specific functions like `n()` and friends
### so I'm relying to dplyr to do it
### Unfortunately I can't figure out a way to
### return a structure similar to the one
### eval_all_tidy() produces while keeping the expressions
### sequentially dependent
dplyr_eval_all_tidy <- function(data, ...){

  # if (getOption("fastplyr.inform", TRUE)){
  #   rlang::warn(
  #     c(
  #       paste(cli::col_blue("dplyr"), "mask function detected, results will be independent of each other"),
  #       "Run `options(fastplyr.inform = FALSE)` to turn this msg off"
  #     )
  #   )
  # }


  quos <- rlang::enquos(...)
  expr_names <- names(quos)
  group_vars <- group_vars(data)
  n_groups <- length(group_vars)

  results <- list()
  groups <- list()

  # Loop over the expressions
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    expr <- rlang::quo_get_expr(quo)
    expr_name <- expr_names[i]
    env <- rlang::quo_get_env(quo)
    #  Fix this later as new objects don't take precedence over data variables here
    if (nzchar(expr_name)){
      result <- dplyr::reframe(data, !!expr_name := !!quo)
    } else {
      result <- dplyr::reframe(data, !!quo)
    }
    result2 <- as.list(result)[seq_len(df_ncol(result) - n_groups) + n_groups]
    results <- c(results, result2)
    groups <- cheapr::list_combine(groups, rep(list(df_as_tbl(cheapr::sset_col(result, group_vars))), length(result2)))
  }
  names(groups) <- names(results)
  list(groups = groups, results = results)
}

eval_mutate <- function(data, quos){
  check_fastplyr_quos(quos)

  if (length(quos) == 0){
    return(`names<-`(list(), character()))
  }

  GRP <- attr(quos, ".GRP", TRUE)
  quo_names <- names(quos)
  which_optimised <- cheapr::val_find(attr(quos, ".optimised", TRUE), TRUE)

  if (length(which_optimised) > 0L){
    which_regular <- cheapr::val_find(attr(quos, ".optimised", FALSE), FALSE)
    regular_quos <- sset_quos(quos, which_regular)
    optimised_quos <- sset_quos(quos, which_optimised)
    optimised_results <- eval_optimised_quos(data, optimised_quos, add_groups = FALSE)
    regular_results <- eval_mutate(data, regular_quos)
    results <- cheapr::new_list(length(quos))
    results[which_regular] <- regular_results
    results[which_optimised] <- optimised_results[[2L]]
    names(results) <- quo_names
    return(results)
  }

  if (cpp_any_quo_contains_dplyr_mask_call(quos)){
    return(as.list(dplyr::mutate(
      construct_fastplyr_grouped_df(data, GRP), !!!quos
    ))[quo_names])
  }
  results <- cpp_grouped_eval_mutate(
    construct_fastplyr_grouped_df(data, GRP), quos
    )

  k <- 1L
  for (i in seq_along(results)){
    # Unpack
    if (isTRUE(attr(quos[[i]], ".unpack", TRUE)) && is_df(results[[k]])){
      results_to_append <- as.list(results[[k]])
      if (nzchar(quo_names[[i]])){
        names(results_to_append) <- paste(quo_names[[i]], names(results_to_append), sep = "_")
      }
      results <- append(results, results_to_append, after = k - 1L)
      k <- k + length(results_to_append)
      results[[k]] <- NULL
    } else {
      k <- k + 1L
    }
  }
  results
}

mutate_summary_ungrouped <- function(.data, ..., .keep = c("all", "used", "unused", "none"),
          error_call = rlang::caller_env())
{
  .keep <- rlang::arg_match(.keep)
  original_cols <- names(.data)
  bare_data <- df_ungroup(.data)
  group_data <- new_tbl(.rows = add_attr(list(seq_len(df_nrow(bare_data))),
                                         "class", c("vctrs_list_of", "vctrs_vctr", "list")))
  by <- add_attr(list(type = "ungrouped", names = character(),
                      data = group_data), "class", "dplyr_by")
  dplyr_quos <- dplyr_quosures(...)
  cols <- mutate_cols(bare_data, dplyr_quos, by = by, error_call = error_call)
  out_data <- dplyr::dplyr_col_modify(bare_data, cols)
  final_cols <- names(cols)
  used <- attr(cols, "used")
  keep_cols <- switch(.keep, all = names(used), none = final_cols,
                      used = names(used)[which(used)], unused = names(used)[cheapr::which_(used,
                                                                                           invert = TRUE)])
  out_data <- f_select(out_data, .cols = keep_cols)
  out <- list(data = out_data, cols = final_cols)
  out
}

mutate_summary_grouped <- function (.data, ..., .keep = c("all", "used", "unused", "none"),
          .by = NULL, error_call = rlang::caller_env())
{
  .keep <- rlang::arg_match(.keep)
  original_cols <- names(.data)
  by <- compute_by(by = {
    {
      .by
    }
  }, data = .data, by_arg = ".by", data_arg = ".data")
  group_vars <- get_groups(.data, .by = {
    {
      .by
    }
  })
  dplyr_quos <- dplyr_quosures(...)
  cols <- mutate_cols(.data, dplyr_quos, by = by, error_call = error_call)
  out_data <- dplyr::dplyr_col_modify(.data, cols)
  final_cols <- names(cols)
  used <- attr(cols, "used")
  keep_cols <- switch(.keep, all = names(used), none = final_cols,
                      used = names(used)[which(used)], unused = names(used)[cheapr::which_(used,
                                                                                           invert = TRUE)])
  keep_cols <- c(group_vars, keep_cols[match(keep_cols, group_vars,
                                             0L) == 0L])
  keep_cols <- keep_cols[order(match(keep_cols, original_cols))]
  out_data <- f_select(out_data, .cols = keep_cols)
  out <- list(data = out_data, cols = final_cols)
  out
}

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
    extra_groups <- vec_setdiff(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, vec_setdiff(extra_groups, group_vars))
  }
  if (is.null(out_info)){
    changed_groups <- character()
    address_equal <- add_names(logical(length(names(data))), names(data))
  } else {
    changed_groups <- vec_intersect(names(data), out_info[["changed_cols"]])
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
    extra_groups <- vec_setdiff(extra_groups, group_vars)
    all_groups <- c(group_vars, extra_groups)
  } else {
    all_groups <- c(group_vars, vec_setdiff(extra_groups, group_vars))
  }
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
