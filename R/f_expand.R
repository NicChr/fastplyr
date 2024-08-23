#' Fast versions of `tidyr::expand()` and `tidyr::complete()`.
#'
#' @param data A data frame
#' @param ... Variables to expand
#' @param fill A named list containing value-name pairs
#' to fill the named implicit missing values.
#' @param sort Logical. If `TRUE` expanded/completed variables are sorted.
#' The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#'
#' @returns
#' A `data.frame` of expanded groups.
#'
#' @rdname f_expand
#' @export
f_expand <- function(data, ..., sort = FALSE, .by = NULL){
  group_vars <- get_groups(data, {{ .by }})
  data2 <- f_group_by(data, .by = {{ .by }}, .add = TRUE)
  dots <- rlang::enquos(...)
  dot_nms <- names(dots)
  frames <- vector("list", length(dots))

  # Repair names of dots
  for (i in seq_along(dots)){
    if (!nzchar(dot_nms[i])){
      dot_nms[i] <- rlang::as_label(dots[[i]])
    }
  }
  dot_nms <- setdiff(unique_name_repair(c(group_vars, dot_nms)), group_vars)
  for (i in seq_along(dots)){
    frames[[i]] <- f_distinct(df_ungroup(dplyr::reframe(data2, !!dots[[i]])))
    names(frames[[i]]) <- c(group_vars, dot_nms[i])
  }
  # for (i in seq_along(dots)){
  #   frames[[i]] <- f_distinct(df_ungroup(dplyr::reframe(data, !!dots[[i]])))
  #   if (nzchar(dot_nms[i])){
  #     names(frames[[i]]) <- c(group_vars, dot_nms[i])
  #   } else {
  #     names(frames[[i]]) <- c(group_vars, rlang::as_label(dots[[i]]))
  #   }
  # }
  # frames <- lapply(
  #   dots,
  #   function(dot){
  #     f_distinct(dplyr::reframe(data, !!dot))
  #   }
  # )
  # dot_nms <- names(summarise_list(data, !!!dots))

  # return(reconstruct(data, Reduce(dplyr::full_join, frames)))
  # return(reconstruct(data, Reduce( function(x, y){
  #   join_on <- intersect(names(x), names(y))
  #   collapse_join(x, y, how = "full", on = join_on, multiple = TRUE)
  # }, frames)))
  if (length(group_vars) > 0){
    out <- Reduce(
      function(x, y){
        join_on <- intersect(names(x), names(y))
        collapse_join(x, y, how = "full", on = join_on, multiple = TRUE)
      },
      frames,
      simplify = FALSE
    )
  } else {
    out <- Reduce(df_cross_join, frames, simplify = FALSE)
  }
  # If just empty list
  if (length(out) == 0){
    out <- f_select(group_data(data2), .cols = group_vars)
  }
  if (sort){
    out <- f_arrange_all(out)
  }
  reconstruct(data, out)
}
# f_expand2 <- function(data, ..., sort = FALSE, .by = NULL){
#   group_vars <- get_groups(data, {{ .by }})
#   summarise_vars <- summarise_list(data, ...)
#   grps_missed <- setdiff(group_vars, names(summarise_vars))
#   # Add group vars to summary list
#   if (length(grps_missed) > 0){
#     summarise_vars <- c(add_names(
#       lapply(
#         grps_missed, function(x)
#           dplyr::reframe(
#             df_ungroup(data), across(all_of(x))
#           )[[1]]
#       ), grps_missed
#     ),
#     summarise_vars)
#   }
#   # Re-order list so that groups are first
#   summarise_vars <- summarise_vars[c(group_vars,
#                                      setdiff(names(summarise_vars),
#                                              group_vars))]
#   summarise_var_nms <- names(summarise_vars)
#   out_nms <- c(group_vars, setdiff(summarise_var_nms, group_vars))
#   # All variables minus grouped ones
#   leftover_grp_nms <- setdiff(summarise_var_nms, group_vars)
#
#   if (
#     #Special case when data is grouped but only 1 data variable is specified to expand
#     # There is no need from a speed perspective to do grouped calculation in this case
#
#     (length(group_vars) > 0L &&
#      length(leftover_grp_nms) <= 1L)
#   ){
#     out <- list_as_df(do.call(cheapr::recycle, c(summarise_vars, list(length = df_nrow(data)))))
#     if (sort){
#       out <- f_arrange_all(out)
#     }
#   } else {
#
#     # Method for grouped data which performs a separate cross-join of
#     # non-grouped variables for each group
#
#     if (length(group_vars) > 0L && length(leftover_grp_nms) >= 2L){
#       out1 <- list_as_df(do.call(cheapr::recycle, c(summarise_vars, list(length = df_nrow(data)))))
#
#       # Add group ID
#
#       grp_nm <- unique_col_name(out1, ".group.id")
#       out1[[grp_nm]] <- group_id(f_select(out1, .cols = group_vars), order = FALSE)
#       out1 <- f_arrange(out1, .cols = grp_nm)
#
#       # Add group IDs for each non-group variable
#       # This will allow us to calculate final expanded size
#
#       for (i in seq_along(leftover_grp_nms)){
#         assign(paste0("grp_nm_", i), unique_col_name(out1, ".group.id"))
#         out1[[get(paste0("grp_nm_", i))]] <- group_id(out1[[leftover_grp_nms[[i]]]], order = FALSE)
#       }
#       group_id_nms <- unlist(mget(paste0("grp_nm_",
#                                          seq_len(length(leftover_grp_nms)))),
#
#                              recursive = FALSE, use.names = FALSE)
#       # Figure out final size before expansion, to do this we can
#       # Calculate the vector product of unique expanded elements across groups.
#
#       out_temp <- collapse::fndistinct(f_select(out1, .cols = group_id_nms),
#                                        g = out1[[grp_nm]],
#                                        use.g.names = FALSE, na.rm = FALSE)
#       sizes <- round(exp(rowSums(log(out_temp))))
#       expanded_nrow <- sum(sizes)
#       out2 <- out1 %>%
#         dplyr::reframe(
#           across(
#             all_of(group_id_nms),
#             function(x) list(collapse::funique(x))
#           ),
#           .by = all_of(grp_nm)
#         )
#       temp <- out2 %>%
#         dplyr::reframe(cj = CJ2(unlist(dplyr::pick(all_of(group_id_nms)), recursive = FALSE)),
#                        .by = all_of(grp_nm))
#       out <- new_tbl(..N = expanded_nrow)
#       out[[grp_nm]] <- rep(df_seq_along(out2), sizes)
#       for (i in seq_along(leftover_grp_nms)){
#         out[[get(paste0("grp_nm_", i))]] <- unlist(temp[["cj"]][seq(i, df_nrow(temp), length(leftover_grp_nms))])
#       }
#
#       ## Alternative (but when sort is FALSE it doesn't give the same sorted output)
#       # out <- out %>%
#       #   collapse_join(out1, how = "left", on = c(grp_nm, group_id_nms)) %>%
#       #   f_select(.cols = out_nms)
#       # out <- f_rename(out, .cols = add_names(names(out), c(grp_nm, leftover_grp_nms)))
#       for (i in seq_along(group_id_nms)){
#         grp_to_modify <- leftover_grp_nms[[i]]
#         grp_to_match_on <- group_id_nms[[i]]
#         out[[grp_to_modify]] <- out1[[grp_to_modify]][
#           collapse::fmatch(out[[grp_to_match_on]],
#                            out1[[grp_to_match_on]],
#                            overid = 2L)
#         ]
#       }
#       for (i in seq_along(group_vars)){
#         out[[group_vars[[i]]]] <- out1[[group_vars[[i]]]][
#           collapse::fmatch(out[[grp_nm]],
#                            out1[[grp_nm]],
#                            overid = 2L)
#         ]
#       }
#       # out[[grp_nm]] <- NULL
#       if (sort){
#         o <- radixorderv2(f_select(out, .cols = c(group_vars, leftover_grp_nms)))
#         out <- f_select(out, .cols = out_nms)
#         if (!isTRUE(attr(o, "sorted"))){
#           out <- cheapr::sset(out, o)
#         }
#         # out <- f_arrange(out, .cols = c(group_vars, leftover_grp_nms))
#       }
#     }
#     # If no groups then cross-join everything
#     else {
#       out <- cross_join(summarise_vars, unique = TRUE, sort = sort)
#     }
#   }
#   out <- f_select(out, .cols = out_nms)
#   reconstruct(data, out)
# }
#' @rdname f_expand
#' @export
f_complete <- function(data, ...,  sort = FALSE, .by = NULL, fill = NA){
  group_vars <- get_groups(data, {{ .by }})
  expanded_df <- f_expand(data,
                         ...,
                         sort = FALSE, .by = {{ .by }})
  fill_na <- any(!is.na(fill))
  out <- data
  # Full-join
  if (df_nrow(expanded_df) > 0 && df_ncol(expanded_df) > 0){
    extra <- cheapr::setdiff_(
      expanded_df,
      cheapr::sset(out, j = names(expanded_df))
    )
    if (df_nrow(extra) > 0){
      extra <- df_cbind(
        extra,
        df_init(cheapr::sset(out, j = setdiff(names(out), names(expanded_df))),
                df_nrow(extra))
      )
      out <- collapse::rowbind(out, extra)
    }
    if (sort){
      out <- f_arrange(out, .cols = c(group_vars,
                                     setdiff(names(expanded_df), group_vars)))
    }
  }
  # Replace NA with fill
  if (fill_na){
    fill <- fill[!is.na(fill)]
    fill_nms <- names(fill)
    for (i in seq_along(fill)){
      if (length(fill[[i]]) != 1){
        stop("fill values must be of length 1")
      }
      out[[fill_nms[[i]]]][cheapr::which_na(out[[fill_nms[[i]]]])] <-
        fill[[i]]
    }
  }
  out_order <- c(names(data), setdiff(names(out), names(data)))
  out <- f_select(out, .cols = out_order)
  reconstruct(data, out)
}
#
# # Nested join, recycling newly created variables with data variables
# nested_join <- function(X, sort = FALSE, N){
#   X_nms <- names(X)
#   if (length(X_nms) == 0L){
#     X_nms <- rep_len("Var", length(X))
#     X_nms <- paste0(X_nms, seq_len(length(X)))
#   }
#   X_lens <- cheapr::lengths_(X)
#   # If N is not supplied, then calculate N iff all list lengths are equal
#   if (missing(N)){
#     N <- unique(X_lens)
#   }
#   check_length(N, 1L)
#   # Data variables
#   data_nms <- X_nms[(X_lens %% N) == 0]
#   # Newly created variables
#   other_nms <- X_nms[!X_nms %in% data_nms]
#   df <- list_as_df(X[X_nms %in% data_nms])
#   n_data <- df_nrow(df)
#   if (n_data > 0L){
#     df <- collapse::funique(df)
#     n_data <- df_nrow(df)
#   }
#   X_other <- X[X_nms %in% other_nms]
#   X_other <- lapply(X_other, function(x) collapse::funique(x, sort = FALSE))
#   n_data <- max(n_data, 1L)
#   n_other <- prod(cheapr::lengths_(X_other))
#   n_other <- max(n_other, 1, na.rm = TRUE)
#   expanded_n <- prod(c(n_data, n_other), na.rm = TRUE)
#   # Nested cross-join
#   grp_seq <- seq_len(n_data)
#   if (df_nrow(df) == 0L){
#     out <- cross_join(X_other, unique = FALSE)
#   } else {
#     out <- df_row_slice(df, rep(grp_seq, each = n_other))
#     if (length(X_other) > 0L){
#       rep_times <- df_nrow(out) / cheapr::lengths_(X_other)
#       for (i in seq_along(X_other)){
#         out[[other_nms[i]]] <- rep(X_other[[i]], rep_times[i])
#       }
#     }
#   }
#   if (sort){
#     out <- f_arrange_all(out)
#   }
#   out
# }
# cross_join <- function(X, unique = TRUE, sort = FALSE, strings_as_factors = FALSE){
#   x_nms <- names(X)
#   if (unique){
#     X <- lapply(X, collapse::funique)
#   } else {
#     X <- as.list(X)
#   }
#   expanded_n <- prod(cheapr::lengths_(X))
#   if (strings_as_factors){
#     which_chr <- which(vapply(X, is.character, FALSE, USE.NAMES = FALSE))
#     X[which_chr] <- lapply(X[which_chr],
#                            function(x) cheapr::factor_(x, order = FALSE))
#   }
#   out <- CJ2(X)
#   if (is.null(x_nms)){
#     x_nms <- paste0("V", seq_along(out))
#   }
#   names(out) <- x_nms
#   out <- list_as_tbl(out)
#   if (sort){
#     out <- f_arrange_all(out)
#   }
#   out
# }

