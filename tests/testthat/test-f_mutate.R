test_that("mutate", {

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

    options(fastplyr.inform = TRUE)
    # Verify that optimisations work for complex group-unaware exprs
    expect_message(df |> f_mutate(new1 = round(identity(y) + .data$x), new2 = (\(x) 0L)()))
    options(fastplyr.inform = FALSE)

    expect_equal(
      df |> f_mutate(new1 = round(identity(y) + .data$x - abs(y)), .by = g),
      df |> dplyr::mutate(new1 = round(identity(y) + .data$x - abs(y)), .by = g)
    )

})

