test_that("f_distinct", {
  flights <- nycflights13::flights
  expect_equal(
    f_distinct(f_select(iris, .cols = 0)),
    structure(list(), names = character(0),
              class = "data.frame",
              row.names = c(NA, -1L))
  )
  expect_equal(
    f_distinct(f_select(iris, .cols = 0), .sort = TRUE),
    structure(list(), names = character(0),
              class = "data.frame",
              row.names = c(NA, -1L))
  )
  expect_equal(
    f_distinct(f_select(iris, .cols = 0), .sort = FALSE, .order = TRUE),
    structure(list(), names = character(0),
              class = "data.frame",
              row.names = c(NA, -1L))
  )

  expect_equal(f_distinct(flights, .cols = 0),
                         dplyr::distinct(flights,
                                         dplyr::across(dplyr::all_of(character(0)))))
  expect_equal(f_distinct(flights, dplyr::across(dplyr::all_of(character(0)))),
                         dplyr::distinct(flights,
                                         dplyr::across(dplyr::all_of(character(0)))))
  expect_identical(flights %>%
                               dplyr::distinct(),
                             flights %>%
                               f_distinct())
  expect_identical(flights %>%
                               dplyr::distinct(carrier, origin, tailnum, dest),
                             flights %>%
                               f_distinct(carrier, origin, tailnum, dest))
  expect_identical(flights %>%
                               dplyr::group_by(flight) %>%
                               dplyr::distinct(carrier, origin, tailnum, dest),
                             flights %>%
                               dplyr::group_by(flight) %>%
                               f_distinct(carrier, origin, tailnum, dest))
  expect_identical(flights %>%
                               dplyr::distinct(carrier, origin, tailnum, dest,
                                               .keep_all = TRUE),
                             flights %>%
                               f_distinct(carrier, origin, tailnum, dest,
                                         .keep_all = TRUE))
  expect_identical(flights %>%
                               dplyr::group_by(flight) %>%
                               dplyr::distinct(carrier, origin, tailnum, dest,
                                               .keep_all = TRUE),
                             flights %>%
                               dplyr::group_by(flight) %>%
                               f_distinct(carrier, origin, tailnum, dest,
                                         .keep_all = TRUE))

  expect_identical(flights %>%
                               dplyr::slice(0) %>%
                               dplyr::distinct(),
                             flights %>%
                               dplyr::slice(0) %>%
                               f_distinct())
  expect_identical(flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               f_slice(0) %>%
                               dplyr::distinct(),
                             flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               f_slice(0) %>%
                               f_distinct())
  expect_identical(flights %>%
                               dplyr::slice(0) %>%
                               dplyr::distinct(),
                             flights %>%
                               dplyr::slice(0) %>%
                               f_distinct())
  expect_identical(flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               f_slice(0) %>%
                               dplyr::distinct(.keep_all = TRUE),
                             flights %>%
                               dplyr::group_by(dest, origin, tailnum) %>%
                               f_slice(0) %>%
                               f_distinct(.keep_all = TRUE))
  set.seed(42)
  indices <- sample.int(150)
  expect_identical(iris %>%
                     dplyr::slice(indices) %>%
                     dplyr::group_by(Species) %>%
                     dplyr::distinct(),
                   iris %>%
                     dplyr::slice(indices) %>%
                     dplyr::group_by(Species) %>%
                     f_distinct())
})
