#' Summarise each group down to one row
#'
#' @description Like `dplyr::summarise()` but with some internal optimisations
#' for common statistical functions.
#'
#' @param data A data frame.
#' @param ... Name-value pairs of summary functions. Expressions with
#' `across()` are also accepted.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#'
#' @returns
#' An un-grouped data frame of summaries by group.
#'
#' @examples
#' library(fastplyr)
#' library(nycflights13)
#'
#' # Number of flights per month, including first and last day
#' flights |>
#'   f_group_by(year, month) |>
#'   f_summarise(first_day = first(day),
#'               last_day = last(day),
#'               num_flights = n())
#'
#' ## Fast mean summary using `across()`
#'
#' flights |>
#'   f_summarise(
#'     across(where(is.double), mean),
#'     .by = tailnum
#'   )
#'
#' # To ignore or keep NAs, use collapse::set_collapse(na.rm)
#' collapse::set_collapse(na.rm = FALSE)
#' flights |>
#'   f_summarise(
#'     across(where(is.double), mean),
#'     .by = origin
#'   )
#' collapse::set_collapse(na.rm = TRUE)
#' @export
f_summarise <- function(data, ..., .by = NULL){
  base_fns <- c("sum", "mean", "min", "max", "first", "last",
                "sd", "var")
  collapse_fns <- paste0("f", base_fns)
  optimised_fns <- c(base_fns, collapse_fns)
  group_vars <- get_groups(data, {{ .by }})
  data2 <- f_group_by(data, .by = {{ .by }}, .add = TRUE)

  groups <- df_to_GRP(data2)

  dots <- rlang::enquos(...)
  dot_nms <- names(dots)

  is_n_call <- function(x) {
    rlang::quo_is_call(x, "n", ns = c("", "dplyr"))
  }
  is_optimised_call <- function(x) {
    rlang::quo_is_call(x, optimised_fns, ns = c("", "base", "collapse", "dplyr"))
  }
  is_across_call <- function(x) {
    rlang::quo_is_call(x, "across", ns = c("", "dplyr"))
  }

  out <- f_select(group_data(data2), .cols = group_vars)
  out <- reconstruct(new_tbl(), out)
  for (i in seq_along(dots)){
    dot <- dots[[i]]
    dot_label <- rlang::as_label(dot)
    dot_nm <- dot_nms[i]
    if (!nzchar(dot_nm)){
      dot_nm <- dot_label
    }
    dot_env <- rlang::quo_get_env(dot)
    dot_args <- rlang::call_args(dot)
    if (is_n_call(dot)){
      out[[dot_nm]] <- GRP_group_sizes(groups)
    } else if (is_optimised_call(dot)){
      var <- as.character(dot_args[[1]])
      fun_name <- unlist(stringr::str_match_all(dot_label, base_fns))
      fun_name <- collapse_fns[match(fun_name, base_fns)]
      fun <- get_from_package(fun_name, "collapse")
      fun_args <- dot_args[-1]
      if (length(fun) == 1 && var %in% names(data)){
        res <- do.call(
          fun, c(list(data[[var]],
                      g = groups,
                      use.g.names = FALSE),
                 fun_args)
        )
        out[[dot_nm]] <- res

      } else {
        temp <- dplyr::summarise(data2, !!!dots[i])
        out[[dot_nm]] <- f_select(temp, .cols = df_ncol(temp))
      }
    } else if (is_across_call(dot)){
      across_expr <- match.call(
        definition = dplyr::across,
        call = rlang::quo_get_expr(dot),
        expand.dots = FALSE,
        envir = rlang::quo_get_env(dot)
      )

      if (!".cols" %in% names(across_expr)){
        stop(".cols must be supplied in `across()`")
      }
      if (!".fns" %in% names(across_expr)){
        stop(".fns must be supplied in `across()`")
      }
      unused_args <- setdiff(names(across_expr)[-1],
                             c(".cols", ".fns", ".names"))
      if (length(unused_args) > 0){
        stop(paste("These arguments must not be used in `across()`:",
                   paste(unused_args, collapse = ", ")))
      }

      across_vars <- across_expr[[".cols"]]
      across_fns <- across_expr[[".fns"]]
      across_nms <- across_expr[[".names"]]

      across_fns_as_list <- rlang::is_call(across_fns, "list")
      vars <- names(tidyselect::eval_select(across_vars, data, env = dot_env))

      vars <- setdiff(vars, group_vars)

      if (any(group_vars %in% vars)){
        stop("can't supply any `group_vars(data)` as cols to `across()`")
      }
      if (across_fns_as_list) {
        fns <- vapply(rlang::call_args(across_fns), rlang::as_label, "")
        fn_names <- names(fns)
        fn_names[fn_names == ""] <- fns[fn_names == ""]
      } else {
        fns <- rlang::as_label(across_fns)
        fn_names <- fns
      }
      fn_matches <- cheapr::na_rm(sort(c(match(fns, base_fns),
                                         match(fns, collapse_fns))))
      which_fns <- which(fns %in% base_fns | fns %in% collapse_fns)
      which_other_fns <- which(fns %in% base_fns | fns %in% collapse_fns,
                               invert = TRUE)
      fast_fn_names <- collapse_fns[fn_matches]
      full_res <- vector("list", length(vars) * length(fns))
      col_matrix <- matrix(logical( length(vars) * length(fns)),
                           nrow = length(vars),
                           ncol = length(fns))
      col_matrix[, which_fns] <- TRUE
      across_res <- eval_across(data2, groups, vars, fast_fn_names, dot_env)

      if (length(which_other_fns) > 0){
        if (across_fns_as_list){
          dplyr_res <- dplyr::summarise(
            data2, dplyr::across(
              dplyr::all_of(vars),
              rlang::eval_tidy(across_fns, env = dot_env)[which_other_fns]
            ), .groups = "drop"
          )
        } else {
          dplyr_res <- dplyr::summarise(
            data2, dplyr::across(
              dplyr::all_of(vars),
              rlang::eval_tidy(across_fns, env = dot_env)
            ), .groups = "drop"
          )
        }
        dplyr_res <- as.list(dplyr_res)[setdiff(names(dplyr_res), group_vars)]
        full_res[which(col_matrix, invert = TRUE)] <- dplyr_res
      }


      full_res[which(col_matrix)] <- across_res
      out_var_names <- across_col_names(vars, fn_names, across_nms)
      names(full_res) <- out_var_names

      res_sizes <- cheapr::lengths_(full_res)
      if (any(res_sizes != df_nrow(out))){
        stop("Expressions must return exactly 1 row per `f_summarise()` group")
      }
      if (length(full_res) > 0){
        out <- df_cbind(out, list_as_df(full_res))
      }
    } else {
      temp <- dplyr::summarise(data2, !!!dots[i], .groups = "drop")
      out <- df_cbind(out, f_select(temp, .cols = setdiff(names(temp), group_vars)))
    }
  }
  reconstruct(df_ungroup(data), out)
}

eval_across <- function(data, g, .cols, .fns, env, .names = NULL){
  ncols <- length(.cols)
  nfns <- length(.fns)
  out <- vector("list", ncols * nfns)
  i <- 1L
  for (f in .fns){
   for (col in .cols){
     fun <- get_from_package(f, "collapse")
     out[[i]] <- rlang::eval_tidy(
       fun(data[[col]], g = g, use.g.names = FALSE),
       env = env
     )
     i <- i + 1L
   }
  }
  out
}

across_col_names <- function (.cols = NULL, .fns = NULL, .names = NULL){
  nms_null <- is.null(.names)
  if (nms_null && length(.fns) == 1L) {
    out <- .cols
  } else if (nms_null && length(.cols) == 1L) {
    out <- .fns
  } else {
    out <- character(length(.cols) * length(.fns))
    init <- 0L
    .fn <- .fns
    if (nms_null) {
      for (.col in .cols) {
        out[seq_along(.fns) + init] <- stringr::str_c(.col,
                                                      "_", .fn)
        init <- init + length(.fns)
      }
    }
    else {
      for (.col in .cols) {
        out[seq_along(.fns) + init] <- stringr::str_glue(.names)
        init <- init + length(.fns)
      }
    }
  }
  if (anyDuplicated(out) > 0) {
    stop("Column names must be unique")
  }
  out
}
