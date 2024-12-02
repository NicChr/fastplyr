test_that("row-bind", {


  expect_identical(f_bind_rows(), new_tbl())

  expect_identical(f_bind_rows(NULL), new_tbl())

  expect_identical(
    f_bind_rows(f_select(iris, .cols = 0)),
    cheapr::new_df(.nrows = 150)
  )

  expect_identical(
    f_bind_rows(f_select(as_tbl(iris), .cols = 0), f_select(iris, .cols = 0)),
    new_tbl(.nrows = 300)
  )

  expect_identical(
    f_bind_rows(f_select(iris, .cols = 0), f_select(iris, .cols = 0)),
    cheapr::new_df(.nrows = 300)
  )


  expect_identical(
    f_bind_rows(iris, iris),
    rbind(iris, iris)
  )

  expect_identical(
    f_bind_rows(iris, iris, iris),
    f_bind_rows(list(iris, iris, iris))
  )

  expect_identical(
    f_bind_rows(iris, f_select(iris, .cols = c(4, 3, 5, 1, 2))),
    rbind(iris, iris)
  )

  temp <- iris
  temp$l1 <- as.list(1:150)
  temp$l2 <- as.list(150:1)

  expect_identical(
    f_bind_rows(temp, temp),
    dplyr::bind_rows(temp, temp)
  )

  expect_identical(
    f_bind_rows(f_select(temp, .cols = 1:2), temp),
    dplyr::bind_rows(f_select(temp, .cols = 1:2), temp)
  )

})

test_that("col-bind", {

  expect_identical(
    f_bind_cols(f_select(iris, 1:3), f_select(iris, 4:5)),
    iris
  )

  # Check that naming is correct, non df objs can be joined and recycled
  expect_identical(
    f_bind_cols(iris, ok = 1L, 1:3),
    dplyr::mutate(iris, ok = 1L, `...7` = rep_len(1:3, 150))
  )

  # Check that class is kept
  expect_identical(
    f_bind_cols(as_tbl(iris), okay = 0L),
    as_tbl(dplyr::mutate(iris, okay = 0L))
  )

  # Order of bind
  expect_identical(
    f_bind_cols(f_select(iris, 5:4), f_select(iris, c(2, 3, 1))),
    f_select(iris, .cols = c(5, 4, 2, 3, 1))
  )

  expect_identical(
    f_bind_cols(), new_tbl()
  )

  expect_identical(
    f_bind_cols(NULL), new_tbl()
  )

  expect_identical(
    f_bind_cols(ok1 = 1, NULL, NULL, ok2 = 2, NULL), new_tbl(ok1 = 1, ok2 = 2)
  )

  expect_identical(
    f_bind_cols(iris, iris),
    add_names(fast_bind_cols(iris, iris), unique_name_repair(rep(names(iris), 2)))
  )

  ## List of data frames

  expect_identical(
    f_bind_cols(iris, iris, iris),
    f_bind_cols(list(iris, iris, iris))
  )


  # 0-col data frames

  expect_identical(
    f_bind_cols(f_select(iris, .cols = 0)), cheapr::new_df(.nrows = 150)
  )

  expect_identical(
    f_bind_cols(f_select(iris, .cols = 0), iris, f_select(iris, .cols = 0)),
    iris
  )

})
