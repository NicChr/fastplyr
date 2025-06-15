
df <- dplyr::starwars

left <- cheapr::sset(df, 1:35)
right <- cheapr::sset(df, 30:50)
left$skin_color <- as.factor(left$skin_color)
right$skin_color <- as.factor(right$skin_color)

join_cols <- c("eye_color", "films")

test_that("joins", {

  target <- dplyr::left_join(left, right, by = join_cols, relationship = "many-to-many")
  actual <- f_left_join(left, right, by = join_cols, multiple = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::right_join(left, right, by = join_cols, relationship = "many-to-many")
  actual <- f_right_join(left, right, by = join_cols, multiple = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::inner_join(left, right, by = join_cols, relationship = "many-to-many")
  actual <- f_inner_join(left, right, by = join_cols, multiple = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::full_join(left, right, by = join_cols, relationship = "many-to-many")
  actual <- f_full_join(left, right, by = join_cols, multiple = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::anti_join(left, right, by = join_cols)
  actual <- f_anti_join(left, right, by = join_cols)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::semi_join(left, right, by = join_cols)
  actual <- f_semi_join(left, right, by = join_cols)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::cross_join(left, right)
  actual <- f_cross_join(left, right)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)
})

test_that("joins with `keep = TRUE`", {

  target <- dplyr::left_join(left, right, by = join_cols, relationship = "many-to-many", keep = TRUE)
  actual <- f_left_join(left, right, by = join_cols, multiple = TRUE, keep = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::right_join(left, right, by = join_cols, relationship = "many-to-many", keep = TRUE)
  actual <- f_right_join(left, right, by = join_cols, multiple = TRUE, keep = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::inner_join(left, right, by = join_cols, relationship = "many-to-many", keep = TRUE)
  actual <- f_inner_join(left, right, by = join_cols, multiple = TRUE, keep = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::full_join(left, right, by = join_cols, relationship = "many-to-many", keep = TRUE)
  actual <- f_full_join(left, right, by = join_cols, multiple = TRUE, keep = TRUE)

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)
})

test_that("joins with suffix", {

  target <- dplyr::left_join(left, right, by = join_cols, relationship = "many-to-many", suffix = c("_left", "_right"))
  actual <- f_left_join(left, right, by = join_cols, multiple = TRUE, suffix = c("_left", "_right"))

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::right_join(left, right, by = join_cols, relationship = "many-to-many", suffix = c("_left", "_right"))
  actual <- f_right_join(left, right, by = join_cols, multiple = TRUE, suffix = c("_left", "_right"))

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::inner_join(left, right, by = join_cols, relationship = "many-to-many", suffix = c("_left", "_right"))
  actual <- f_inner_join(left, right, by = join_cols, multiple = TRUE, suffix = c("_left", "_right"))

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::full_join(left, right, by = join_cols, relationship = "many-to-many", suffix = c("_left", "_right"))
  actual <- f_full_join(left, right, by = join_cols, multiple = TRUE, suffix = c("_left", "_right"))

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)

  target <- dplyr::cross_join(left, right, suffix = c("_left", "_right"))
  actual <- f_cross_join(left, right, suffix = c("_left", "_right"))

  expect_identical(names(target), names(actual))
  expect_equal(nrow(dplyr::anti_join(target, actual, by = names(target))), 0)
})
