#' Bind data frame rows and columns
#'
#' @description
#' Faster bind rows and columns.
#'
#' @param ... Data frames to bind.
#' @param .repair_names Should duplicate column names be made unique?
#' Default is `TRUE`.
#' @param .sep Separator to use for creating unique column names.
#' @param .fill Should missing columns be filled with `NA`?
#' Default is `TRUE`.
#' @param .recycle Should inputs be recycled to a common row size?
#' Default is `TRUE`.
#'
#' @returns
#' `f_bind_rows()` performs a union of the data frames specified via `...` and
#' joins the rows of all the data frames, without removing duplicates.
#'
#' `f_bind_cols()` joins the columns, creating unique column names if there are
#' any duplicates by default.
#'
#' @rdname f_bind_rows
#' @export
f_bind_rows <- function(..., .fill = TRUE){
  dots <- list3(...)
  n_dots <- length(dots)
  ncols <- cpp_ncols(dots, check_cols_equal = !.fill)
  nrows <- cpp_nrows(dots, FALSE)
  if (n_dots == 0){
    new_df()
  } else if (n_dots == 1){
    dots[[1L]]
  } else {
    template <- dots[[1L]]
    prototype_names <- names(template)

    if (.fill){
      dots <- lapply(dots, df_ungroup)
    } else {
      dots <- lapply(
        dots, function(x) f_select(df_ungroup(x), .cols = prototype_names)
      )
    }

    if (!cpp_any_frames_exotic(dots)){

      # We can use collapse::rowbind if data frames
      # contain simple atomic vectors

      rowbind <- function(...){
        collapse::rowbind(..., return = "data.frame", fill = .fill)
      }
      reconstruct(template, do.call(rowbind, dots))
    } else {
      col_prototypes <- as.list(cheapr::sset(template, 0))

      if (.fill){
        get_prototype <- function(x) cheapr::sset(x, 0L)
        for (df in dots){
          new_prototype_names <- fast_setdiff(names(df), prototype_names)
          if (length(new_prototype_names)){
            prototype_names <- c(prototype_names, new_prototype_names)
            temp_df <- as.list(df)[new_prototype_names]
            col_prototypes <- c(
              col_prototypes, lapply(
                temp_df,
                get_prototype
              )
            )
          }
        }
        # Second pass to fill in missing cols

        for (i in seq_along(dots)){
          df <- dots[[i]]
          cols_to_add <- fast_setdiff(prototype_names, names(df))
          if (length(cols_to_add)){
            df_to_bind <- list_as_df(lapply(col_prototypes[cols_to_add], na_init, nrows[[i]]))
            df <- f_bind_cols(df, df_to_bind, .recycle = FALSE)
          }
          dots[[i]] <- df_select(df, prototype_names)
        }
      }

      out_ncols <- length(col_prototypes)
      out_nrows <- sum(nrows)
      out <- cheapr::new_list(out_ncols)
      names(out) <- names(col_prototypes)

      # Join rows

      for (j in seq_len(out_ncols)){
        temp <- cheapr::new_list(length(dots))
        for (i in seq_along(dots)){
          cpp_set_list_element(temp, i, dots[[i]][[j]])
        }
        out[[j]] <- Reduce(f_union_all, temp)
      }

      # Not using list_as_df just in case it gets the
      # nrows wrong

      attr(out, "row.names") <- .set_row_names(out_nrows)
      class(out) <- "data.frame"
      reconstruct(template, out)
    }
  }
}
#' @rdname f_bind_rows
#' @export
f_bind_cols <- function(..., .repair_names = TRUE, .recycle = TRUE, .sep = "..."){
  if (.recycle){
    dots <- cheapr::recycle(...)
  } else {
    dots <- list_rm_null(list(...))
  }
  for (i in seq_along(dots)){
    if (!inherits(dots[[i]], "data.frame")){
      dots[[i]] <- `class<-`(list_as_df(dots[i]), c("tbl_df", "tbl", "data.frame"))
    }
  }
  nrows <- cpp_nrows(dots, check_rows_equal = !.recycle)
  out <- unlist(unname(dots), recursive = FALSE)
  if (.repair_names){
    if (is.null(names(out))){
      names(out) <- paste0(.sep, seq_along(out))
    } else {
      names(out) <- unique_name_repair(names(out), .sep = .sep)
    }
  }
  out <- list_as_df(out)
  if (length(dots) == 1){
    out <- reconstruct(dots[[1L]], out)
  } else if (length(dots) > 1){
    N <- nrows[1L]
    # Adjustment for 0-column only data frames
    if (df_nrow(out) != N){
      attr(out, "row.names") <- .set_row_names(N)
    }
    template <- dots[[1L]]

    # Special method for grouped_df because
    # we don't need to recalculate groups
    # Since we're not rearranging or renaming variables
    # except in the case of duplicates.

    if (inherits(template, "grouped_df") &&
        all(group_vars(template) %in% names(out))){
      out <- reconstruct(df_ungroup(template), out)
      class(out) <- class(template)
      attr(out, "groups") <- attr(template, "groups")
    } else {
      out <- reconstruct(template, out)
    }
  }
  out
}
