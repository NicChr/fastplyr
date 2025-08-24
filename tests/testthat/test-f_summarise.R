test_that("summarise", {

  set.seed(82134)
  airquality <- f_slice_sample(airquality, 2e04, TRUE)
  airquality$Month <- sample.int(2e04)

  # Cant have multiple rows per group
  expect_error(
    suppressWarnings(
      airquality |>
        f_summarise(Wind)
    )
  )

  target <- airquality |>
    dplyr::rowwise() |>
    dplyr::summarise(Wind)

  expect_equal(
    airquality |>
      f_rowwise() |>
      f_summarise(Wind) |>
      dplyr::select(Wind),
    target
  )

  target <- airquality |>
    dplyr::summarise(mean_wind = mean(Wind))

  expect_equal(
    airquality |>
      f_summarise(mean_wind = mean(Wind)),
    target
  )

  target <- airquality |>
    dplyr::summarise(mean_wind = mean(Wind),
                     .by = Month)

  expect_equal(
    airquality |>
      f_summarise(mean_wind = mean(Wind),
                  .by = Month, .order = FALSE),
    target
  )

  target <- airquality |>
    dplyr::group_by(Month) |>
    dplyr::summarise(mean_wind = mean(Wind))

  expect_equal(
    airquality |>
      dplyr::group_by(Month) |>
      f_summarise(mean_wind = mean(Wind)),
    target
  )
  expect_equal(
    airquality |>
      dplyr::group_by(Month) |>
      f_summarise(mean_wind = mean(Wind)),
    target
  )

  target <- airquality |>
    dplyr::summarise(n_nas = cheapr::na_count(Wind), .by = Month)

  expect_equal(
    airquality |>
      f_summarise(n_nas = cheapr::na_count(Wind), .by = Month, .order = FALSE),
    target
  )

  target <- airquality |>
    dplyr::group_by(Month) |>
    dplyr::summarise(n_nas = cheapr::na_count(Wind))

  expect_equal(
    airquality |>
      dplyr::group_by(Month) |>
      f_summarise(n_nas = cheapr::na_count(Wind)),
    target
  )

  target <- airquality |>
    dplyr::group_by(Month) |>
    dplyr::summarise(dplyr::across(Wind, cheapr::na_count))

  expect_equal(
    airquality |>
      dplyr::group_by(Month) |>
      f_summarise(dplyr::across(Wind, cheapr::na_count)),
    target
  )

  target <- airquality |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(mean = \(x) mean(x, na.rm = TRUE),
             median = \(x) median(x, na.rm = TRUE),
             min = \(x) min(x, na.rm = TRUE),
             max = \(x) max(x, na.rm = TRUE)),
        .names = "col_{.col}_fun_{.fn}"
      ), N = dplyr::n()
    )
  expect_equal(
    airquality |>
      f_summarise(
        dplyr::across(dplyr::everything(),
                      list(mean = \(x) mean(x, na.rm = TRUE),
                           median = \(x) median(x, na.rm = TRUE),
                           min = \(x) min(x, na.rm = TRUE),
                           max = \(x) max(x, na.rm = TRUE)),
                      .names = "col_{.col}_fun_{.fn}"),
        N = dplyr::n()
      ),
    target
  )
  expect_equal(
    airquality |>
      f_summarise(
        dplyr::across(dplyr::everything(), list_tidy(mean, median, min, max, .named = TRUE),
        .names = "col_{.col}_fun_{.fn}"),
        N = dplyr::n()
      ),
    target
  )

  target <- airquality |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        list(mean = \(x) mean(x, na.rm = TRUE),
             median = \(x) median(x, na.rm = TRUE),
             min = \(x) min(x, na.rm = TRUE),
             max = \(x) max(x, na.rm = TRUE))
      ), N = dplyr::n()
    )
  expect_equal(
    airquality |>
      f_summarise(
        dplyr::across(dplyr::everything(),
                      list(mean = \(x) mean(x, na.rm = TRUE),
                           median = \(x) median(x, na.rm = TRUE),
                           min = \(x) min(x, na.rm = TRUE),
                           max = \(x) max(x, na.rm = TRUE))),
        N = dplyr::n()),
    target
  )

  # 2 variables and a mix of optimised/non-optimised calls

  target <- airquality |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c("Wind", "Temp")),
        list(mean = \(x) mean(x, na.rm = TRUE),
             first = \(x) x[1],
             min = \(x) min(x, na.rm = TRUE),
             last_obs = \(x) x[length(x)],
             max = \(x) max(x, na.rm = TRUE))
      ), N = dplyr::n()
    )

  expect_equal(
    airquality |>
      f_summarise(
        dplyr::across(dplyr::all_of(c("Wind", "Temp")),
                      list(mean = mean,
                           first = \(x) x[1],
                           min = min,
                           last_obs = \(x) x[length(x)],
                           max = max)),
        N = dplyr::n()
      ),
    target
  )

  # 2 variables and a mix of optimised/non-optimised calls, and groups

  target <- airquality |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c("Wind", "Temp")),
        list(mean = \(x) mean(x, na.rm = TRUE),
             first = \(x) x[1],
             min = \(x) min(x, na.rm = TRUE),
             last_obs = \(x) x[length(x)],
             max = \(x) max(x, na.rm = TRUE))
      ), N = dplyr::n(),
      .by = Month
    )

  expect_equal(
    airquality |>
      f_summarise(
        dplyr::across(dplyr::all_of(c("Wind", "Temp")),
                      list(mean = mean,
                           first = \(x) x[1],
                           min = min,
                           last_obs = \(x) x[length(x)],
                           max = max)),
        N = dplyr::n(),
        .by = Month,
        .order = FALSE
      ),
    target
  )
})


test_that("summarise2", {

  cheapr::with_local_seed(
    {
      df <- new_tbl(
        x = rnorm(25),
        y = round(x, 2) + 0,
        g = sample.int(3, 25, TRUE)
      )
    }, 42
  )

  # New variables
  expect_error(
    df |>
      f_summarise(z = x)
  )

  expect_equal(
    df |>
      f_summarise(z = 0L),
    df |>
      dplyr::summarise(z = 0L)
  )

  expect_equal(
    df |>
      f_summarise(b = 0L, c = mean(x)),
    df |>
      dplyr::summarise(b = 0L, c = mean(x))
  )

  # Using .data and .env

  x <- seq_len(nrow(df))

  expect_equal(
    df |>
      f_summarise(new1 = sum(x), new2 = sum(.data$x), new3 = sum(.env$x)),
    df |>
      dplyr::summarise(new1 = sum(x), new2 = sum(.data$x), new3 = sum(.env$x))
  )

  expect_equal(
    df |>
      f_summarise(new1 = 0L, new2 = sum(.data$new1)),
    df |>
      dplyr::summarise(new1 = 0L, new2 = sum(.data$new1))
  )

  a <- 1:nrow(df)

  expect_equal(
    df |>
      f_summarise(a = 0L, b = sum(.data$a), c = sum(.env$a)),
    df |>
      dplyr::summarise(a = 0L, b = sum(.data$a), c = sum(.env$a))
  )

  expect_equal(
    df |> f_summarise(new1 = mean(x), new2 = sum(x), n = n(), .by = g, .order = FALSE),
    df |> dplyr::summarise(new1 = mean(x), new2 = sum(x), n = n(), .by = g)
  )

})


test_that("summarise + across", {
  cheapr::with_local_seed(
    {
      df <- new_tbl(
        x = rnorm(25),
        y = round(x, 2) + 0,
        g = sample.int(3, 25, TRUE)
      )
    }, 42
  )

  expect_equal(
    df |>
      f_summarise(across(x, sum)),
    df |>
      dplyr::summarise(across(x, sum))
  )

  expect_equal(
    df |>
      f_summarise(across(x, list(sum = sum), .names = "{.col}")),
    df |>
      dplyr::summarise(across(x, list(sum = sum), .names = "{.col}"))
  )

  expect_equal(
    df |>
      f_summarise(across(1:2, list(sum = sum), .names = "{.col}")),
    df |>
      dplyr::summarise(across(1:2, list(sum = sum), .names = "{.col}"))
  )

  expect_equal(
    df |>
      f_summarise(across(1:2, list_tidy(sum, .named = TRUE), .names = "{.col}")),
    df |>
      dplyr::summarise(across(1:2, list_tidy(sum, .named = TRUE), .names = "{.col}"))
  )

  expect_equal(
    df |>
      f_summarise(across(1:2, list_tidy(sum, mean, .named = TRUE), .names = "{.col}_fn_{.fn}")),
    df |>
      dplyr::summarise(across(1:2, list_tidy(sum, mean, .named = TRUE), .names = "{.col}_fn_{.fn}"))
  )

  expect_equal(
    df |>
      f_summarise(
        across(
          1:2, list_tidy(sum, mean, .named = TRUE),
          .names = "{.col}_fn_{.fn}"
        ), .by = g
      ),
    df |>
      dplyr::summarise(
        across(
          1:2, list_tidy(sum, mean, .named = TRUE), .names = "{.col}_fn_{.fn}"
        ), .by = g
      )
  )

  # .unpack
  expect_equal(
    df |>
      f_summarise(across(1:2, \(x) new_tbl(new1 = sum(x), new2 = mean(x)), .unpack = FALSE)),
    df |>
      dplyr::summarise(across(1:2, \(x) new_tbl(new1 = sum(x), new2 = mean(x)), .unpack = FALSE))
  )

  expect_equal(
    df |>
      f_summarise(across(1:2, \(x) new_tbl(new1 = sum(x), new2 = mean(x)), .unpack = TRUE)),
    df |>
      dplyr::summarise(across(1:2, \(x) new_tbl(new1 = sum(x), new2 = mean(x)), .unpack = TRUE))
  )

})
