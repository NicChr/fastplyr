test_that("f_duplicates", {
  test_df <- dplyr::tibble(x1 = c("a", "a", "b", "c", "d", "e", "a"),
                           x2 = c("a", "a", "b", "c", "a", "b", "a"),
                           x3 = c("a", "a", "b", "a", "b", "e", "a"),
                           x4 = c("a", "a", "a", "a", "b", "b", "a"),
                           group = c(1, 1, 1, 2, 2, 3, 3))

  test_df2 <- dplyr::tibble(x = c("a", NA, "c", NA, NA),
                            y = c(NA, "b", "c", NA, NA),
                            z = c(NA, "b", "c", NA, NA))
  # This doesn't work because data.table can't handle 0-columns.
  # expect_identical(f_duplicates(test_df |> dplyr::select()),
  #                            test_df |> dplyr::select() |> dplyr::slice(0))
  expect_identical(nrow(f_duplicates(test_df, .both_ways = TRUE)), 2L)
  expect_identical(nrow(f_duplicates(test_df2, .both_ways = TRUE,
                                                 .drop_empty = TRUE)), 0L)
  expect_identical(f_duplicates(iris, Sepal.Length, Species,
                                            .keep_all = TRUE,
                                            .both_ways = TRUE,
                                .order = FALSE),
                             iris |>
                               dplyr::group_by(Sepal.Length, Species) |>
                               dplyr::filter(dplyr::n() > 1) |>
                               f_ungroup() |>
                               as.data.frame())
  expect_identical(nrow(f_duplicates(test_df, .both_ways = FALSE)), 1L)
  expect_identical(test_df |>
                               f_duplicates(x1, x2, x3, x4, .both_ways = TRUE) |>
                               nrow(), 3L)
  expect_identical(test_df |>
                               f_duplicates(x1, x2, x3, x4, .both_ways = TRUE) |>
                               nrow(), 3L)
  expect_identical(test_df |>
                               dplyr::group_by(group) |>
                               f_duplicates(x2, .both_ways = TRUE) |>
                               nrow(), 2L)
  expect_identical(test_df |>
                               dplyr::group_by(group) |>
                               f_duplicates(.both_ways = TRUE) |>
                               nrow(), 2L)
  expect_identical(test_df |>
                               f_duplicates(x2, .both_ways = TRUE) |>
                               nrow(), 6L)

  expect_identical(test_df |>
                               f_duplicates(x1, x2, .both_ways = TRUE,
                                           .keep_all = TRUE,
                                           .add_count = TRUE),
                               test_df |>
                               dplyr::add_count(x1, x2) |>
                               dplyr::filter(n > 1))
  expect_identical(test_df |>
                               f_duplicates(x1, x2, .both_ways = TRUE,
                                           .keep_all = FALSE,
                                           .add_count = TRUE),
                             test_df |>
                               dplyr::select(x1, x2) |>
                               dplyr::add_count(x1, x2) |>
                               dplyr::filter(n > 1))
})
