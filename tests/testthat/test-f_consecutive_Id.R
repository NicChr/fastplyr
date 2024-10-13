test_that("consecutive id", {
  df <- new_tbl(a = round(rnorm(10^4, 2)),
                b = paste(letters, as.character(round(rnorm(10^4), 2))),
                c = complex(real = cheapr::na_insert(rnorm(10^4), prop = 1/4),
                            imaginary = rnorm(10^4)),
                d = sample.int(5, 10^4, TRUE) - 2L)

  df <- rapply(df, function(x) cheapr::na_insert(x, prop = 1/4), how = "replace")

  expect_identical(
    dplyr::consecutive_id(df$a),
    f_consecutive_id(df$a)
  )
  expect_identical(
    dplyr::consecutive_id(df$b),
    f_consecutive_id(df$b)
  )
  expect_identical(
    dplyr::consecutive_id(df$c),
    f_consecutive_id(df$c)
  )
  expect_identical(
    dplyr::consecutive_id(df$d),
    f_consecutive_id(df$d)
  )
  expect_identical(
    dplyr::consecutive_id(df),
    f_consecutive_id(df)
  )
})
