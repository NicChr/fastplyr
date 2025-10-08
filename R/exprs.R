
.optimised_fn_list <- list(

  input_fns = list(
    sum = base::sum,
    prod = base::prod,
    mean = base::mean,
    median = stats::median,
    min = base::min,
    max = base::max,
    first = dplyr::first,
    last = dplyr::last,
    sd = stats::sd,
    var = stats::var,
    n_distinct = dplyr::n_distinct,
    fsum = collapse::fsum,
    fprod = collapse::fprod,
    fmean = collapse::fmean,
    fmedian = collapse::fmedian,
    fmin = collapse::fmin,
    fmax = collapse::fmax,
    ffirst = collapse::ffirst,
    flast = collapse::flast,
    fsd = collapse::fsd,
    fvar = collapse::fvar,
    fndistinct = collapse::fndistinct
  ),
  target_fns = list(
    fsum = collapse::fsum,
    fprod = collapse::fprod,
    fmean = collapse::fmean,
    fmedian = collapse::fmedian,
    fmin = collapse::fmin,
    fmax = collapse::fmax,
    grouped_first = grouped_first,
    grouped_last = grouped_last,
    fsd = collapse::fsd,
    fvar = collapse::fvar,
    fndistinct = collapse::fndistinct,
    fsum = collapse::fsum,
    fprod = collapse::fprod,
    fmean = collapse::fmean,
    fmedian = collapse::fmedian,
    fmin = collapse::fmin,
    fmax = collapse::fmax,
    grouped_first = grouped_first,
    grouped_last = grouped_last,
    fsd = collapse::fsd,
    fvar = collapse::fvar,
    fndistinct = collapse::fndistinct
  )
)
.optimised_fns_inform <- c(
  "sum", "prod", "mean", "median", "min", "max", "sd", "var",
  "dplyr::n", "dplyr::first", "dplyr::last", "dplyr::n_distinct",
  "dplyr::row_number", "dplyr::lag", "dplyr::lead",
  "dplyr::cur_group", "dplyr::cur_group_id", "dplyr::cur_group_rows"
)

is_optimised_call <- function(expr, env = rlang::caller_env()){
  is_fn_call(expr, names(.optimised_fn_list[["input_fns"]]), NULL, env)
}

# Turn character vector into named character vector
as_named <- function(x){
  `names<-`(x, cheapr::str_coalesce(names(x), as.character(x)))
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

inform_user_on_eval_split <- function(regular_quos, optimised_quos){

  inform <- getOption("fastplyr.inform")

  if (inform %||% TRUE &&
      length(regular_quos) &&
      length(optimised_quos)){
    cli::cli_inform(c(
      "Expressions will be evaluated in separate masks",
      paste("Normal exprs:", cli::col_blue("{names(regular_quos)}")),
      paste("Optimised exprs:", cli::col_red("{names(optimised_quos)}")),
      "",
      "To always evaluate everything in the same mask run {.run fastplyr::fastplyr_disable_optimisations()}",
      "It is advised to run these exprs in separate e.g. `f_mutate/f_reframe/f_summarise` statements",
      "Run {.run fastplyr::fastplyr_disable_informative_msgs()} to disable this and other informative messages"
    ))
  }
}

should_optimise <- function(GRP){
  TRUE
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
    args <- call_args(across_vars)
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

  evaluated_across_fns <- eval(across_fns, envir = quo_env)

  if (is.list(evaluated_across_fns)){
    fn_tree <- evaluated_across_fns
    fn_names <- names(fn_tree) %||% character(length(fn_tree))

    # This line is to not break timeplyr unit tests
    if (!all(nzchar(fn_names)) && rlang::is_call(across_fns, "list")){
      fn_names <- cheapr::str_coalesce(
        fn_names,
        vapply(call_args(across_fns), deparse2, "", USE.NAMES = FALSE)
      )
    }
  } else if (!".fns" %in% names(clean_expr)){
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
  fn_tree <- cheapr::cheapr_rep_len(fn_tree, out_size)

  out <- cheapr::new_list(out_size)
  names(out) <- out_names

  for (i in seq_along(out)){
    fn <- fn_tree[[i]]
    col <- cols[[i]]
    new_quo <- rlang::new_quosure(
      rlang::call2(fn, as.symbol(col)),
      # rlang::call2(fn, call("$", quote(.data), as.symbol(col))),
      quo_env
    )
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

  # .fastplyr.g is used internally for optimised calculations
  # where setting `g = NULL` is more efficient when N groups is 1
  # .groups is still needed to supply the group metadata to later functions
  if (n_groups == 1){
    .fastplyr.g <- NULL
  } else {
    .fastplyr.g <- .groups
  }
  .original_out <- out

  # Second pass to check for optimised calls
  if (.optimise && getOption("fastplyr.optimise", TRUE)){

    if (is.null(getOption("fastplyr.inform"))){
      cli::cli_inform(
        c(
          "!" = "Expressions will be optimised where possible.",
          "",
          "Optimised expressions are independent from unoptimised ones and typical data-masking rules may not apply",
          "",
          "Run {.run fastplyr::fastplyr_disable_optimisations()} to disable optimisations globally",
          "",
          "Run {.run fastplyr::fastplyr_disable_informative_msgs()} to disable this and other informative messages"
        ),
        .frequency = "once", .frequency_id = ".optimise_inform"
      )
      options(fastplyr.inform = TRUE)
    }

    if (.optimise_expand){
      TRA <- "replace_fill"
    } else {
      TRA <- NULL
    }

    data_mask <- rlang::as_data_mask(.data)

    for (i in seq_along(out)){
      quo <- out[[i]]
      expr <- rlang::quo_get_expr(quo)
      env <- rlang::quo_get_env(quo)

      group_unaware_expr <- is_group_unaware_call(expr, env, data_mask)

      ### Group-unaware calls CAN BE NESTED
      ### But currently other optimised calls must not be nested

      if (!group_unaware_expr && is_nested_call(expr)) next


      if (group_unaware_expr){
        group_unaware[i] <- TRUE
      } else if (rlang::is_scalar_atomic(expr)){
        if (is.null(.fastplyr.g)){
          next
        } else {
          if (.optimise_expand){
            expr <- cheapr::cheapr_rep_len(expr, GRP_data_size(.fastplyr.g))
          } else {
            expr <- cheapr::cheapr_rep_len(expr, GRP_n_groups(.fastplyr.g))
          }
        }
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
              out <- cheapr::sset(out, GRP_group_id(.fastplyr.g))
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
              cheapr::sset(GRP_group_id(.fastplyr.g), GRP_starts(.fastplyr.g))
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
        expr <- rlang::call2(grouped_lag, !!!args, g = .fastplyr.g)
      } else if (.optimise_expand && is_fn_call(expr, "lead", "dplyr", env)){
        if (!all_blank(vec_setdiff(names(args), c("x", "n", "default", "order_by")))){
          next
        }
        args <- call_args(expr)
        names(args)[names(args) == "default"] <- "fill"
        expr <- rlang::call2(grouped_lead, !!!args, g = .fastplyr.g)
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

        # If fn is inlined into the expr
        if (is.function(fn)){
          match_loc <- match_fun(fn, .optimised_fn_list[["input_fns"]])
        } else {
          fn <- rlang::as_string(fn)
          match_loc <- match(fn, names(.optimised_fn_list[["input_fns"]]))
          input_ns <- fun_ns(.optimised_fn_list[["input_fns"]][[match_loc]], env)
          if (input_ns != ns){
            next
          }
        }
        new_fn <- .optimised_fn_list[["target_fns"]][[match_loc]]
        expr <- rlang::call2(new_fn, !!!args, g = .fastplyr.g, TRA = TRA, use.g.names = FALSE)
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
        if (is.null(.groups[["locs"]])){
          .groups[["locs"]] <- f_group_rows(.data)
        }
      } else {
        .groups[["locs"]] <- GRP_loc(.groups)
      }
    }
  }

  cheapr::attrs_modify(
    out,
    .optimised = optimised,
    .GRP = .groups,
    .group_unaware = group_unaware,
    .fastplyr_quos = TRUE,
    .set = TRUE
  )
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
  out <- unclass(quos)[i]
  cheapr::attrs_modify(
    out,
    class = class(quos),
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

