cheapr::with_local_seed(
  {
    df <- new_tbl(
      x = rnorm(25),
      y = round(x, 2) + 0,
      g = sample.int(3, 25, TRUE)
    )
  }, 42
)

test_that("mutate", {

  # New variables
    expect_equal(
      df |>
        f_mutate(z = x),
      df |>
        dplyr::mutate(z = x)
    )

    expect_equal(
      df |>
        f_mutate(z = 0L),
      df |>
        dplyr::mutate(z = 0L)
    )

    expect_equal(
      df |>
        f_mutate(a = x, b = 0L, c = mean(x)),
      df |>
        dplyr::mutate(a = x, b = 0L, c = mean(x))
    )

    # Replacing variable
    expect_equal(
      df |>
        f_mutate(y = x),
      df |>
        dplyr::mutate(y = x)
    )

    expect_equal(
      df |>
        f_mutate(y = x, x = y),
      df |>
        dplyr::mutate(y = x, x = y)
    )

    # Using .data and .env

    x <- seq_len(nrow(df))

    expect_equal(
      df |>
        f_mutate(new1 = x, new2 = .data$x, new3 = .env$x),
      df |>
        dplyr::mutate(new1 = x, new2 = .data$x, new3 = .env$x)
    )

    expect_equal(
      df |>
        f_mutate(new1 = 0L, new2 = .data$new1),
      df |>
        dplyr::mutate(new1 = 0L, new2 = .data$new1)
    )

    a <- 1:nrow(df)

    expect_equal(
      df |>
        f_mutate(a = 0L, b = .data$a, c = .env$a),
      df |>
        dplyr::mutate(a = 0L, b = .data$a, c = .env$a)
    )

    expect_equal(
      df |> f_mutate(new1 = mean(x), new2 = sum(x), n = n(), .by = g),
      df |> dplyr::mutate(new1 = mean(x), new2 = sum(x), n = n(), .by = g)
    )

    expect_equal(
      df |> f_mutate(new1 = mean(x), new2 = sum(x), n = n(), new3 = identity(y), new4 = x + y, .by = g),
      df |> dplyr::mutate(new1 = mean(x), new2 = sum(x), n = n(), new3 = identity(y), new4 = x + y, .by = g)
    )

    # Verify that optimisations work for complex group-unaware exprs
    options(fastplyr.inform = TRUE)
    expect_message(df |> f_mutate(new1 = round(identity(y) + .data$x), new2 = (\(x) 0L)()))
    options(fastplyr.inform = FALSE)

    expect_equal(
      df |> f_mutate(new1 = round(identity(y) + .data$x - abs(y)), .by = g),
      df |> dplyr::mutate(new1 = round(identity(y) + .data$x - abs(y)), .by = g)
    )

    # Make sure that the correct variables are being used

})



test_that("mutate + across", {

  expect_equal(
    df |>
      f_mutate(across(x, sum)),
    df |>
      dplyr::mutate(across(x, sum))
  )

  expect_equal(
    df |>
      f_mutate(across(x, list(sum = sum), .names = "{.col}")),
    df |>
      dplyr::mutate(across(x, list(sum = sum), .names = "{.col}"))
  )

  expect_equal(
    df |>
      f_mutate(across(1:2, list(sum = sum), .names = "{.col}")),
    df |>
      dplyr::mutate(across(1:2, list(sum = sum), .names = "{.col}"))
  )

  expect_equal(
    df |>
      f_mutate(across(1:2, list_tidy(sum, .named = TRUE), .names = "{.col}")),
    df |>
      dplyr::mutate(across(1:2, list_tidy(sum, .named = TRUE), .names = "{.col}"))
  )

  expect_equal(
    df |>
      f_mutate(across(1:2, list_tidy(sum, mean, .named = TRUE), .names = "{.col}_fn_{.fn}")),
    df |>
      dplyr::mutate(across(1:2, list_tidy(sum, mean, .named = TRUE), .names = "{.col}_fn_{.fn}"))
  )

  expect_equal(
    df |>
      f_mutate(
        across(
          1:2, list_tidy(sum, mean, .named = TRUE),
          .names = "{.col}_fn_{.fn}"
        ), .by = g
      ),
    df |>
      dplyr::mutate(
        across(
          1:2, list_tidy(sum, mean, .named = TRUE), .names = "{.col}_fn_{.fn}"
        ), .by = g
      )
  )

  # .unpack
  expect_equal(
    df |>
      f_mutate(across(1:2, \(x) new_tbl(new1 = x, new2 = mean(x)), .unpack = FALSE)),
    df |>
      dplyr::mutate(across(1:2, \(x) new_tbl(new1 = x, new2 = mean(x)), .unpack = FALSE))
  )

  expect_equal(
    df |>
      f_mutate(across(1:2, \(x) new_tbl(new1 = x, new2 = mean(x)), .unpack = TRUE)),
    df |>
      dplyr::mutate(across(1:2, \(x) new_tbl(new1 = x, new2 = mean(x)), .unpack = TRUE))
  )

})


test_that("more tests", {


  # Removing variables

  expect_equal(
    df |> f_mutate(across(everything(), \(x) NULL)),
    f_select(df, nothing())
  )

  options(fastplyr.optimise = FALSE)
  expect_error(df |> f_mutate(y = NULL, new = y))
  options(fastplyr.optimise = TRUE)

  # Using .data[[

  ok <- "x"
  expect_equal(
    df |>
      f_mutate(
        new1 = sum(.data[["x"]]),
        new2 = sum(.data[[ok]]),
        new3 = sum(.data[[unique(c(ok, "x"))]])
      ),
    df |>
      dplyr::mutate(
        new1 = sum(.data[["x"]]),
        new2 = sum(.data[[ok]]),
        new3 = sum(.data[[unique(c(ok, "x"))]])
      )
  )
})
