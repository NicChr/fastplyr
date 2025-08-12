
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
    "base", "base", "base", "stats", "base", "base", "dplyr", "dplyr",
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
  is_fn_call(expr, .optimised_fn_list[["input_fn_nms"]], NULL, env)
}

# is_group_unaware_call <- function(expr, env){
#   maybe <- is_fn_call(expr, names(.group_unaware_fns), NULL, env)
#
#   if (!maybe) return(FALSE)
#
#   # Get fn name as a symbol
#   if (call_is_namespaced(expr)){
#     fn <- expr[[1]][[3]]
#   } else {
#     fn <- expr[[1]]
#   }
#
#   actual_ns <- fun_ns(fn, env)
#   target_ns <- .group_unaware_fns[[fn]]
#   # target_ns <- get0(fn, envir = .group_unaware_fns)
#
#   if (is.null(target_ns)) return(FALSE)
#
#   target_ns == actual_ns
# }


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

unpack_across <- function(quo, data, groups = character(), unpack_default = FALSE){

  expr <- rlang::quo_get_expr(quo)
  quo_env <- rlang::quo_get_env(quo)

  # Remove group variables from data
  data <- cheapr::sset_col(data, vec_setdiff(names(data), groups))

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

  if (is.null(.groups)){
    n_groups <- 1L
    group_vars <- character()
  } else {
    n_groups <- GRP_n_groups(.groups)
    group_vars <- GRP_group_vars(.groups)
  }

  k <- 1L
  for (i in seq_along(out)){
    quo <- out[[k]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    attr(quo, ".unpack", TRUE) %||% set_add_attr(quo, ".unpack", .unpack_default)
    if (!nzchar(quo_nms[[k]]) &&
        (is_fn_call(expr, "across", ns = "dplyr", env) ||
         is_fn_call(expr, "across", ns = "fastplyr", env))){
      left <- out[seq_len(k - 1L)]
      left_nms <- quo_nms[seq_len(k - 1L)]
      unpacked_quos <- unpack_across(
        quo,
        data = .data,
        groups = group_vars,
        unpack_default = .unpack_default
      )
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
  group_unaware <- logical(length(out))

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

      group_unaware_expr <- is_group_unaware_call(expr, env)

      ### Group-unaware calls CAN BE NESTED
      ### But currently other optimised calls must not be nested

      if (!group_unaware_expr && is_nested_call(expr)) next


      if (group_unaware_expr){
        group_unaware[i] <- TRUE
      }
      ### Cases when we are just selecting columns
      ### We can just optimise it away by leaving the expression as is
      ### and then running it through `eval_optimised_quos()`
      else if (is.name(expr) && rlang::as_string(expr) == quo_nms[[i]]){

      } else if (is_fn_call(expr, "n", "dplyr", env)){
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
      } else if (.optimise_expand && is_fn_call(expr, "row_number", "dplyr", env)){
        expr <- rlang::call2(\(){
          if (is.null(.fastplyr.g)){
            seq_len(df_nrow(.data))
          } else {
            cpp_row_id(GRP_order(.fastplyr.g), GRP_group_sizes(.fastplyr.g), TRUE)
          }
        })
      } else if (is_fn_call(expr, "cur_group_id", "dplyr", env)){
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
      } else if (is_fn_call(expr, "cur_group", "dplyr", env)){
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
      } else if (.optimise_expand && is_fn_call(expr, "cur_group_rows", "dplyr", env)){
        expr <- rlang::call2(\(){
          if (is.null(.fastplyr.g)){
            seq_len(df_nrow(.data))
          } else {
            locs <- GRP_loc(.fastplyr.g)
            out <- cpp_unlist_group_locs(
              locs, GRP_group_sizes(.fastplyr.g)
            )
            order <- cpp_orig_order(GRP_group_id(.fastplyr.g),
                                    GRP_group_sizes(.fastplyr.g))
            cheapr::sset(out, order)
          }
        })
      } else if (.optimise_expand && is_fn_call(expr, "lag", "dplyr", env)){
        if (!all_blank(vec_setdiff(names(args), c("x", "n", "default", "order_by")))){
          next
        }
        args <- call_args(expr)
        names(args)[names(args) == "default"] <- "fill"
        expr <- rlang::call2("grouped_lag", !!!args, g = .fastplyr.g)
      } else if (.optimise_expand && is_fn_call(expr, "lead", "dplyr", env)){
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
    if (!is.null(.fastplyr.g)){
      if (identical(f_group_vars(.data), group_vars) &&
          GRP_is_ordered(.fastplyr.g) == group_by_order_default(.data)){
        .fastplyr.g[["locs"]] <- f_group_rows(.data)
      } else {
        .fastplyr.g[["locs"]] <- GRP_loc(.groups)
      }
    }
  }
  cheapr::attrs_modify(
    out,
    .optimised = optimised,
    .GRP = .fastplyr.g,
    .group_unaware = group_unaware,
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

sset_quos <- function(quos, i){
  out <- quos[i]
  cheapr::attrs_modify(
    out,
    .optimised = attr(quos, ".optimised", TRUE)[i],
    .group_unaware = attr(quos, ".group_unaware", TRUE)[i],
    .set = TRUE
  )
  cpp_rebuild(
    out, quos, c("names", "class", ".optimised", ".group_unaware"),
    vec_setdiff(names(attributes(quos)), c("names", "class", ".optimised", ".group_unaware")),
    FALSE
  )
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

unpack_results <- function(x, quos){

  quo_names <- names(quos)

  k <- 1L
  for (i in seq_along(x)){
    # Unpack
    if (isTRUE(attr(quos[[i]], ".unpack", TRUE)) && is_df(x[[k]])){
      results_to_append <- as.list(x[[k]])
      if (nzchar(quo_names[[i]])){
        names(results_to_append) <- paste(quo_names[[i]], names(results_to_append), sep = "_")
      }
      x <- append(x, results_to_append, after = k - 1L)
      k <- k + length(results_to_append)
      x[[k]] <- NULL
    } else {
      k <- k + 1L
    }
  }
  x
}

eval_all_tidy_optimised_quos <- function(data, quos){

  quo_names <- names(quos)
  quo_groups <- attr(quos, ".GRP", TRUE)
  GRP <- attr(quos, ".GRP", TRUE)
  group_unaware <- attr(quos, ".group_unaware", TRUE)
  if (!all(attr(quos, ".optimised", TRUE))){
    cli::cli_abort("All quosures must be tagged as '.optimised'")
  }

  n_quos <- length(quos)

  mask <- rlang::as_data_mask(data)
  results <- lapply(quos, \(x) rlang::eval_tidy(x, mask))

  group_order <- NULL
  reorder <- FALSE

  if (any(group_unaware)){

    # This is for expressions like `+`
    # where we re-order it to match the order of sorted groups
    # which is the way `reframe()` returns its output

    if (!is.null(GRP) && GRP_n_groups(GRP) > 1){
      group_order <- GRP_order(GRP)
      reorder <- !isTRUE(attr(group_order, "sorted"))
    }

    if (reorder){
      results[group_unaware] <- lapply(
        results[group_unaware], \(x) cheapr::sset(x, group_order)
      )
    }
  }

  groups <- cheapr::new_list(n_quos)

  if (any(group_unaware)){
    reordered_groups <- cheapr::sset_col(data, GRP_group_vars(GRP))
    if (reorder){
      reordered_groups <- cheapr::sset(reordered_groups, group_order)
    }

    reordered_groups <- cheapr::cheapr_rep_len(
      list(reordered_groups), cheapr::val_count(group_unaware, TRUE)
    )

  } else {
    reordered_groups <- list()
  }

  if (any(!group_unaware)){
    reframed_groups <- cheapr::cheapr_rep_len(
      list(
        cheapr::cheapr_rep_len(
          GRP_groups(GRP), vector_length(results[cheapr::which_(group_unaware, invert = TRUE)[1]][[1]])
        )
      ),
      cheapr::val_count(group_unaware, FALSE)
    )
  } else {
    reframed_groups <- list()
  }

  groups[group_unaware] <- reordered_groups
  groups[!group_unaware] <- reframed_groups
  names(groups) <- quo_names
  list(groups = groups, results = results)
}

eval_summarise_optimised_quos <- function(data, quos){

  if (!all(attr(quos, ".optimised", TRUE))){
    cli::cli_abort("All quosures must be tagged as '.optimised'")
  }

  n_groups <- GRP_n_groups(attr(quos, ".GRP")) %||% 1

  out <- cpp_eval_all_tidy(quos, rlang::as_data_mask(data))
  nrows <- df_nrow(data)

  for (res in out){
    size <- vector_length(res)
    if ( !(size == 0 && nrows == 0 || size == n_groups) ){
     cli::cli_abort("All optimised expressions should return 1 row per-group")
    }
  }

  out
}

eval_mutate_optimised_quos <- function(data, quos){

  if (!all(attr(quos, ".optimised", TRUE))){
    cli::cli_abort("All quosures must be tagged as '.optimised'")
  }
  cpp_eval_all_tidy(quos, rlang::as_data_mask(data))
}

### In the future we hope to not have to use this
### But right now I'm not sure how to evaluate
### data-mask specific functions like `n()` and friends
### so I'm relying to dplyr to do it
### Unfortunately I can't figure out a way to
### return a structure similar to the one
### eval_all_tidy() produces while keeping the expressions
### sequentially dependent
dplyr_eval_reframe <- function(data, ...){

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
    groups <- cheapr::list_combine(groups, cheapr::cheapr_rep(list(df_as_tbl(cheapr::sset_col(result, group_vars))), length(result2)))
  }
  names(groups) <- names(results)
  list(groups = groups, results = results)
}

dplyr_eval_summarise <- function(data, ...){

  quos <- rlang::enquos(...)
  expr_names <- names(quos)
  group_vars <- group_vars(data)
  n_groups <- length(group_vars)

  results <- list()

  # Loop over the expressions
  for (i in seq_along(quos)){
    quo <- quos[[i]]
    expr <- rlang::quo_get_expr(quo)
    expr_name <- expr_names[i]
    env <- rlang::quo_get_env(quo)
    #  Fix this later as new objects don't take precedence over data variables here
    if (nzchar(expr_name)){
      result <- dplyr::summarise(data, !!expr_name := !!quo)
    } else {
      result <- dplyr::summarise(data, !!quo)
    }
    result2 <- as.list(result)[seq_len(df_ncol(result) - n_groups) + n_groups]
    results <- c(results, result2)
  }
  results
  # list(groups = groups, results = results)
}

eval_all_tidy <- function(data, quos, recycle = FALSE){

  check_fastplyr_quos(quos)
  quo_names <- names(quos)

  GRP <- attr(quos, ".GRP", TRUE)
  optimised <- attr(quos, ".optimised", TRUE)

  if (cpp_any_quo_contains_dplyr_mask_call(quos)){
    return(dplyr_eval_reframe(
      construct_fastplyr_grouped_df(data, GRP), !!!quos
    ))
  }

  which_optimised <- cheapr::val_find(optimised, TRUE)
  which_regular <- cheapr::val_find(optimised, FALSE)

  # If slow_recycle is true then we have to do a manual recycling of results
  slow_recycle <- recycle && length(which_optimised) > 0 && length(which_regular) > 0

  if (slow_recycle){
    backup_GRP <- GRP

    GRP[["groups"]] <- cheapr::new_df(
      .internal.x = cheapr::sset(
        GRP_group_id(GRP), GRP_starts(GRP)
      )
    )
    GRP[["group.vars"]] <- ".internal.x"

    cheapr::attrs_modify(
      quos, .GRP = GRP, .set = TRUE
    )
  }

  regular_quos <- sset_quos(quos, which_regular)
  optimised_quos <- sset_quos(quos, which_optimised)
  optimised_results <- eval_all_tidy_optimised_quos(data, optimised_quos)

  # Doing this check to potentially avoid calculating group locations
  # through a call to `construct_fastplyr_grouped_df()`
  # which will happen in e.g. `f_reframe(df, new = mean(x), .by = g)`
  if (length(regular_quos) == 0){
    regular_results <- list(
      groups = `names<-`(list(), character()),
      results = `names<-`(list(), character())
    )
  } else if (length(optimised_quos) == 0){
    regular_results <- cpp_grouped_eval_tidy(
      construct_fastplyr_grouped_df(data, GRP), regular_quos,
      recycle = recycle, add_groups = TRUE
    )
  } else {
    # Don't recycle in this case as we will manually recycle later
    regular_results <- cpp_grouped_eval_tidy(
      construct_fastplyr_grouped_df(data, GRP), regular_quos,
      recycle = FALSE, add_groups = TRUE
    )
  }

  groups <- cheapr::new_list(length(quos))
  groups[which_regular] <- regular_results[[1]]
  groups[which_optimised] <- optimised_results[[1]]
  names(groups) <- quo_names

  results <- cheapr::new_list(length(quos))
  results[which_regular] <- regular_results[[2]]
  results[which_optimised] <- optimised_results[[2]]
  names(results) <- quo_names

  # If the user provides expressions that can be optimised alongside
  # ones which can't, then special by-group recycling needs to occur
  # to ensure that each within-group result is recycled correctly

  if (slow_recycle){

    group_keys <- construct_group_keys(data, backup_GRP)
    grps <- lapply(groups, \(df) GRP2(df[[1L]], return.locs = FALSE, sort = FALSE))

    sizes <- lapply(grps, GRP_group_sizes)

    all_sizes_the_same <- TRUE

    for (i in 2:length(sizes)){
      all_sizes_the_same <- all_sizes_the_same &&
        identical(sizes[[i]], sizes[[i - 1L]])
    }

    if (!all_sizes_the_same){

      # Split results by group

      # Add group locations
      grps <- lapply(grps, \(x) x[["locs"]] <- GRP_loc(x))

      split_results <- purrr::map2(results, grps, \(x, g) vec_group_split(x, g = g))
      split_results <- transpose_eval_results(split_results)
      split_results <- recycle_eval_results(split_results)
      split_results <- transpose_eval_results(split_results)
      recycled_sizes <- cheapr::list_lengths(split_results[[1]])
      results <- lapply(split_results, \(x) cheapr::cheapr_c(.args = x))
    } else {
      recycled_sizes <- sizes[[1]]
    }
    # These are group IDs of our recycled groups
    out_groups <- cheapr::seq_id(recycled_sizes)
    groups <- cheapr::sset(group_keys, out_groups)
    groups <- cheapr::cheapr_rep_len(
      list(groups), length(results)
    )
    names(groups) <- quo_names
  }

  # Unpack results

  k <- 1L
  for (i in seq_along(results)){
    # Unpack
    if (isTRUE(attr(quos[[i]], ".unpack", TRUE)) && is_df(results[[k]])){
      results_to_append <- as.list(results[[k]])
      if (nzchar(quo_names[[i]])){
        names(results_to_append) <- paste(quo_names[[i]], names(results_to_append), sep = "_")
      }
      results <- append(results, results_to_append, after = k - 1L)
      groups <- append(groups, cheapr::cheapr_rep_len(groups[k], length(results_to_append)), after = k - 1L)
      names(groups) <- names(results)
      k <- k + length(results_to_append)
      results[[k]] <- NULL
      groups[[k]] <- NULL
    } else {
      k <- k + 1L
    }
  }

  list(groups = groups, results = results)
}

eval_summarise <- function(data, quos){

  check_fastplyr_quos(quos)
  quo_names <- names(quos)

  optimised <- attr(quos, ".optimised", TRUE)
  GRP <- attr(quos, ".GRP", TRUE)

  if (cpp_any_quo_contains_dplyr_mask_call(quos)){
    return(dplyr_eval_reframe(
      construct_fastplyr_grouped_df(data, GRP), !!!quos
    ))
  }

  which_optimised <- cheapr::val_find(optimised, TRUE)
  which_regular <- cheapr::val_find(optimised, FALSE)
  regular_quos <- sset_quos(quos, which_regular)
  optimised_quos <- sset_quos(quos, which_optimised)
  optimised_results <- eval_summarise_optimised_quos(data, optimised_quos)

  # Doing this check to potentially avoid calculating group locations
  # through a call to `construct_fastplyr_grouped_df()`
  # which will happen in e.g. `f_summarise(df, new = mean(x), .by = g)`
  if (length(regular_quos) == 0){
    regular_results <- list(
      groups = construct_group_keys(data, GRP),
      results = `names<-`(list(), character())
    )
  } else {
    regular_results <- cpp_grouped_eval_summarise(
      construct_fastplyr_grouped_df(data, GRP), regular_quos
    )
  }

  groups <- regular_results[[1]]

  results <- cheapr::new_list(length(quos))
  results[which_regular] <- regular_results[[2]]
  results[which_optimised] <- optimised_results
  names(results) <- quo_names
  results <- unpack_results(results, quos)
  list(groups = groups, results = results)
}

eval_mutate <- function(data, quos){

  check_fastplyr_quos(quos)
  quo_names <- names(quos)

  optimised <- attr(quos, ".optimised", TRUE)
  GRP <- attr(quos, ".GRP", TRUE)

  if (cpp_any_quo_contains_dplyr_mask_call(quos)){
    return(as.list(dplyr::mutate(
      construct_fastplyr_grouped_df(data, GRP), !!!quos
    ))[quo_names])
  }

  which_optimised <- cheapr::val_find(optimised, TRUE)
  which_regular <- cheapr::val_find(optimised, FALSE)
  regular_quos <- sset_quos(quos, which_regular)
  optimised_quos <- sset_quos(quos, which_optimised)
  optimised_results <- eval_mutate_optimised_quos(data, optimised_quos)

  # Doing this check to potentially avoid calculating group locations
  # through a call to `construct_fastplyr_grouped_df()`
  # which will happen in e.g. `f_mutate(df, new = mean(x), .by = g)`
  if (length(regular_quos) == 0){
    regular_results <- `names<-`(list(), character())
  } else {
    regular_results <- cpp_grouped_eval_mutate(
      construct_fastplyr_grouped_df(data, GRP), regular_quos
    )
  }

  results <- cheapr::new_list(length(quos))
  results[which_regular] <- regular_results
  results[which_optimised] <- optimised_results
  names(results) <- quo_names
  results <- unpack_results(results, quos)
  results
}

mutate_summary_ungrouped <- function(.data, ..., .keep = c("all", "used", "unused", "none"),
          error_call = rlang::caller_env()){
  .data |>
    f_ungroup() |>
    mutate_summary(..., .keep = rlang::arg_match(.keep)) |>
    vec_rename(cols = "new_cols") |>
    cheapr::sset(c("data", "cols"))
}

mutate_summary_grouped <- function (.data, ..., .keep = c("all", "used", "unused", "none"),
          .by = NULL, error_call = rlang::caller_env()){
  .data |>
    mutate_summary(..., .keep = rlang::arg_match(.keep), .by = {{ .by }}) |>
    vec_rename(cols = "new_cols") |>
    cheapr::sset(c("data", "cols"))
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

# tidy_overview <- function(x, ...){
#   ovrvw <- overview(x, ...)
#   for (i in seq_along(ovrvw)){
#     item <- ovrvw[[i]]
#     if (is_df(item)){
#       ovrvw[[i]] <- as_tbl(item)
#     }
#   }
#   class(ovrvw) <- c("tidy_overview", class(ovrvw))
#   ovrvw
# }
# print.tidy_overview <- function (x, max = NULL, ...) {
#   temp <- unclass(x)
#   for (i in seq_along(temp)) {
#     if (inherits(temp[[i]], "data.frame")) {
#       temp[[i]][["class"]] <- NULL
#     }
#   }
#   cat(paste("obs:", temp$obs, "\ncols:", temp$cols), "\n")
#   if (nrow(temp$logical)) {
#     cat("\n----- Logical -----\n")
#     print(temp$logical)
#   }
#   if (nrow(temp$numeric)) {
#     cat("\n----- Numeric -----\n")
#     print(temp$numeric)
#   }
#   if (nrow(temp$date)) {
#     cat("\n----- Dates -----\n")
#     print(temp$date)
#   }
#   if (nrow(temp$datetime)) {
#     datetime_chr_min <- character(nrow(temp$datetime))
#     datetime_chr_max <- character(nrow(temp$datetime))
#     mins <- temp[["datetime"]][["min"]]
#     maxs <- temp[["datetime"]][["max"]]
#     tzones <- temp[["datetime"]][["tzone"]]
#     for (i in seq_len(nrow(temp$datetime))) {
#       datetime_chr_min[i] <- format(mins[i], tz = tzones[i])
#       datetime_chr_max[i] <- format(maxs[i], tz = tzones[i])
#     }
#     temp$datetime$min <- datetime_chr_min
#     temp$datetime$max <- datetime_chr_max
#     cat("\n----- Date-Times -----\n")
#     print(temp$datetime)
#   }
#   if (nrow(temp$time_series)) {
#     cat("\n----- Time-Series -----\n")
#     temp$time_series$growth_rate <- paste0(pretty_num((x$time_series$growth_rate -
#                                                          1) * 100), "%")
#     print(temp$time_series)
#   }
#   if (nrow(temp$categorical)) {
#     cat("\n----- Categorical -----\n")
#     print(temp$categorical)
#   }
#   if (nrow(temp$other)) {
#     cat("\n----- Other -----\n")
#     print(temp$other)
#   }
#   invisible(x)
# }

as_list_of_ints <- function(x){
  vctrs_new_list_of(x, integer())
}
