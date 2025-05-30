% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_select.R
\name{f_select}
\alias{f_select}
\alias{f_rename}
\alias{f_pull}
\alias{nothing}
\title{Fast 'dplyr' \code{select()}/\code{rename()}/\code{pull()}}
\usage{
f_select(data, ..., .cols = NULL)

f_rename(data, ..., .cols = NULL)

f_pull(data, ..., .cols = NULL)

nothing()
}
\arguments{
\item{data}{A data frame.}

\item{...}{Variables to select using \code{tidy-select}.
See \code{?dplyr::select} for more info.}

\item{.cols}{(Optional) faster alternative to \code{...} that accepts
a named character vector or numeric vector. \cr
No checks on duplicates column names are done when using \code{.cols}. \cr
If speed is an expensive resource, it is recommended to use this.}
}
\value{
A \code{data.frame} of selected columns.
}
\description{
\code{f_select()} operates the exact same way as \code{dplyr::select()} and
can be used naturally with \code{tidy-select} helpers.
It uses collapse to perform the actual selecting of variables and is
considerably faster than dplyr for selecting exact columns,
and even more so when supplying the \code{.cols} argument.
}
