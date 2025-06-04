test_that("Compared to tidyr", {
  flights <- nycflights13::flights
  testdf <- flights |>
    f_count(dest, origin, time_hour, tailnum, carrier, flight)
  expect_equal(
    testdf |>
      f_expand(), new_tbl(.nrows = 1)
  )
  expect_equal(
    testdf |>
      f_expand(origin, dest, time_hour, .sort = TRUE),
    testdf |>
      tidyr::expand(origin, dest, time_hour)
  )
  expect_equal(
    testdf |>
      f_expand(tidyr::nesting(origin, dest, time_hour), .sort = TRUE),
    testdf |>
      dplyr::distinct(origin, dest, time_hour) |>
      dplyr::select(origin, dest, time_hour) |>
      dplyr::arrange(dplyr::pick(dplyr::everything()))
  )
  expect_equal(
    testdf |>
      f_complete(tidyr::nesting(origin, dest, time_hour), .sort = FALSE),
    testdf
  )
  expect_equal(
    testdf |>
      f_complete(.sort = TRUE),
    testdf
  )
  # Grouped calculations
  expect_equal(
    testdf |>
      dplyr::group_by(origin, dest) |>
      f_expand(.sort = TRUE),
    testdf |>
      dplyr::group_by(origin, dest) |>
      tidyr::expand()
  )
  expect_equal(
    testdf |>
      dplyr::group_by(origin, dest) |>
      f_expand(carrier, .sort = TRUE),
    testdf |>
      dplyr::group_by(origin, dest) |>
      tidyr::expand(carrier)
  )
  expect_equal(
    testdf |>
      dplyr::group_by(origin) |>
      f_expand(carrier, tailnum, .sort = TRUE),
    testdf |>
      dplyr::group_by(origin) |>
      tidyr::expand(carrier, tailnum)
  )
  expect_equal(
    testdf |>
      dplyr::group_by(origin) |>
      f_expand(carrier, -5:5, .sort = TRUE),
    testdf |>
      dplyr::group_by(origin) |>
      tidyr::expand(carrier, -5:5)
  )
  expect_equal(
    testdf |>
      f_expand(carrier, -5:5, .by = origin,
               .sort = TRUE),
    testdf |>
      dplyr::group_by(origin) |>
      tidyr::expand(carrier, -5:5) |>
      f_ungroup()
  )
  expect_equal(
    testdf |>
      dplyr::group_by(tailnum) |>
      f_expand(carrier, flight) |>
      df_nrow(),
    185292L
  )
expect_equal(
  testdf |>
    f_expand(1:10, yes = 1:10, .sort = TRUE),
  tidyr::expand_grid(1:10, 1:10) |>
    add_names(c("1:10", "yes"))
)
res1 <- flights |>
  f_complete(origin, dest, carrier, .sort = FALSE)
res2 <- flights |>
  tidyr::complete(origin, dest, carrier)
expect_equal(nrow(dplyr::anti_join(res1, res2, by = names(res1))), 0L)
expect_equal(nrow(dplyr::anti_join(res2, res1, by = names(res1))), 0L)

res3 <- flights |>
  f_complete(origin, dest, carrier, .sort = TRUE)
expect_equal(res3, res3 |> dplyr::arrange(origin, dest, carrier))
res4 <- flights |>
  f_complete(origin, dest, carrier, .sort = FALSE, fill = list(arr_time = 0,
                                                              dep_time = 9999))
res5 <- flights |>
  dplyr::mutate(dplyr::across(c(arr_time, dep_time), as.double)) |>
  tidyr::complete(origin, dest, carrier, fill = list(arr_time = 0, dep_time = 9999))
expect_equal(nrow(dplyr::anti_join(res4, res5, by = names(res4))), 0L)
expect_equal(nrow(dplyr::anti_join(res5, res4, by = names(res4))), 0L)

})
