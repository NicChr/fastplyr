test_that("groups", {
  target <- iris |>
    dplyr::group_by(Species) |>
    dplyr::group_by(max(Sepal.Length), .add = TRUE)
  result <- iris |>
    f_group_by(Species) |>
    f_group_by(max(Sepal.Length), .add = TRUE)
  attr(attr(result, "groups"), "ordered") <- NULL
  attr(result, "GRP") <- NULL
  class(result) <- class(target)
  expect_equal(result, target)
})
