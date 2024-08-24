test_that("groups", {
  target <- iris %>%
    dplyr::group_by(Species) %>%
    dplyr::group_by(max(Sepal.Length), .add = TRUE)
  result <- iris %>%
    f_group_by(Species) %>%
    f_group_by(max(Sepal.Length), .add = TRUE)
  attr(attr(result, "groups"), "sorted") <- NULL
  expect_equal(result, target)
})
