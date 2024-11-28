test_that("Object precedence and other tests", {
  x <- 1:10

  expect_equal(
    list_tidy(x = 1, a = x + 1, b = .data$x, c = .env$x),
    list(x = 1, a = 2, b = 1, c = 1:10)
  )

  expect_equal(
    list_tidy(x = 1, 1:3, .data$x, .named = TRUE),
    list(x = 1, `1:3` = 1:3, `.data$x` = 1)
  )

  expect_equal(
    list_tidy(x = 1, NULL,  2, NULL, y = 0, 10, .keep_null = FALSE, .named = TRUE),
    list(x = 1, `2` = 2, y = 0, `10` = 10)
  )
})
