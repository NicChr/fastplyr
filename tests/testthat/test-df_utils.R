
options(fastplyr.inform = FALSE)

test_that("order defaults", {

  options(.fastplyr.order.groups = NULL)

  expect_true(group_by_order_default(iris))

  df2 <- dplyr::group_by(iris, Species)
  df1 <- dplyr::group_by(iris, Species)

  df3 <- f_group_by(iris, Species, .order = FALSE)
  df4 <- f_group_by(iris, Species, .order = TRUE)

  options(.fastplyr.order.groups = FALSE)

  df5 <- f_group_by(iris, Species)
  df6 <- f_group_by(iris, Species)

  options(.fastplyr.order.groups = TRUE)

  df7 <- f_group_by(iris, Species)
  df8 <- f_group_by(iris, Species)

  options(.fastplyr.order.groups = FALSE)

  df9 <- f_group_by(iris, Species, .order = FALSE)
  df10 <- f_group_by(iris, Species, .order = TRUE)

  options(.fastplyr.order.groups = FALSE)

  expect_false(group_by_order_default(iris))
  expect_true(group_by_order_default(df1))
  expect_true(group_by_order_default(df2))
  expect_false(group_by_order_default(df3))
  expect_true(group_by_order_default(df4))
  expect_false(group_by_order_default(df5))
  expect_false(group_by_order_default(df6))
  expect_true(group_by_order_default(df7))
  expect_true(group_by_order_default(df8))
  expect_false(group_by_order_default(df9))
  expect_true(group_by_order_default(df10))

  options(.fastplyr.order.groups = NULL)

})
