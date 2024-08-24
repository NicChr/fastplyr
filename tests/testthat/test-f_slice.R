test_that("f_slice", {
  flights <- nycflights13::flights
  flights[["id"]] <- seq_len(nrow(flights))
  set.seed(5199123)
  ids <- sample(1:10^3)
  expect_error(flights %>% f_slice(c(1, -1)))
  expect_equal(flights %>%
                 f_slice(),
               flights %>%
                 dplyr::slice())
  expect_equal(flights %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice(),
               flights %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice())
  expect_equal(flights %>%
                 f_slice(ids, keep_order = TRUE),
               flights %>%
                 dplyr::slice(ids) %>%
                 dplyr::arrange(id))
  expect_equal(flights %>%
                 f_slice(ids, keep_order = FALSE),
               flights %>%
                 dplyr::slice(ids))
  expect_equal(flights %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice(ids),
               flights %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice(ids))
  expect_equal(flights %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice(ids, keep_order = TRUE),
               flights %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice(ids) %>%
                 dplyr::arrange(id))
  # expect_equal(flights %>%
  #                f_slice(ids, .by = c(origin, dest),
  #                        sort_groups = FALSE),
  #              flights %>%
  #                dplyr::slice(ids, .by = c(origin, dest)))
  expect_equal(flights %>%
                 f_slice(-ids, keep_order = FALSE),
               flights %>%
                 dplyr::slice(-ids))
  expect_equal(flights %>%
                 f_slice(-ids, keep_order = TRUE),
               flights %>%
                 dplyr::slice(-ids) %>%
                 dplyr::arrange(id))
  expect_equal(flights %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice(1:5, keep_order = TRUE),
               flights %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice(1:5) %>%
                 dplyr::arrange(id))
  expect_equal(flights %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice(1:5, keep_order = FALSE),
               flights %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice(1:5))
  expect_equal(flights %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice(-ids, keep_order = TRUE),
               flights %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice(-ids) %>%
                 dplyr::arrange(id))
  expect_equal(flights %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice(-ids, keep_order = FALSE),
               flights %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice(-ids))
})
test_that("f_slice_head", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  expect_error(flights2 %>%
                 f_slice_head(1, -1))
  expect_equal(flights2 %>%
                 f_slice_head(),
               flights2 %>%
                 dplyr::slice_head())
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_head(),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_head())
  # expect_equal(flights2 %>%
  #                f_slice_head(.by = c(origin, dest),
  #                             sort_groups = FALSE),
  #              flights2 %>%
  #                dplyr::slice_head(by = c(origin, dest)))
  expect_equal(flights2 %>%
                 f_slice_head(n = 150),
               flights2 %>%
                 dplyr::slice_head(n = 150))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_head(n = 4, keep_order = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_head(n = 4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_head(n = 4, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_head(n = 4) %>%
                 dplyr::arrange(id))

  expect_equal(flights2 %>%
                 f_slice_head(n = -4, keep_order = TRUE),
               flights2 %>%
                 dplyr::slice_head(n = -4) %>%
                 dplyr::arrange(id))
  expect_equal(flights2 %>%
                 f_slice_head(n = -4, keep_order = FALSE),
               flights2 %>%
                 dplyr::slice_head(n = -4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_head(n = -10, keep_order = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_head(n = -10))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_head(n = -10, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_head(n = -10) %>%
                 dplyr::arrange(id))
})

test_that("f_slice_tail", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  expect_error(flights2 %>%
                 f_slice_tail(1, -1))
  expect_equal(flights2 %>%
                 f_slice_tail(),
               flights2 %>%
                 dplyr::slice_tail())
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_tail(),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_tail())
  # expect_equal(flights2 %>%
  #                f_slice_tail(.by = c(origin, dest),
  #                             sort_groups = FALSE),
  #              flights2 %>%
  #                dplyr::slice_tail(by = c(origin, dest)))
  expect_equal(flights2 %>%
                 f_slice_tail(n = 150),
               flights2 %>%
                 dplyr::slice_tail(n = 150))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_tail(n = 4, keep_order = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_tail(n = 4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_tail(n = 4, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_tail(n = 4) %>%
                 dplyr::arrange(id))

  expect_equal(flights2 %>%
                 f_slice_tail(n = -4, keep_order = TRUE),
               flights2 %>%
                 dplyr::slice_tail(n = -4) %>%
                 dplyr::arrange(id))
  expect_equal(flights2 %>%
                 f_slice_tail(n = -4, keep_order = FALSE),
               flights2 %>%
                 dplyr::slice_tail(n = -4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_tail(n = -10, keep_order = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_tail(n = -10))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_tail(n = -10, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_tail(n = -10) %>%
                 dplyr::arrange(id))
})
test_that("f_slice_sample", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::slice_sample(n = 1),
               flights2 %>%
                 f_slice_sample(n = 1, seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_sample(n = Inf),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_sample(seed = 42))
  # set.seed(42)
  # expect_equal(flights2 %>%
  #                dplyr::slice_sample(n = Inf,
  #                                    by = c(origin, dest)),
  #              flights2 %>%
  #                f_slice_sample(.by = c(origin, dest),
  #                               sort_groups = FALSE,
  #                               seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::slice_sample(n = 150),
               flights2 %>%
                 f_slice_sample(n = 150, seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_sample(n = 4),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_sample(n = 4, keep_order = FALSE, seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_sample(n = 4) %>%
                 dplyr::arrange(id),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_sample(n = 4, keep_order = TRUE, seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::slice_sample(n = -4) %>%
                 dplyr::arrange(id),
               flights2 %>%
                 f_slice_sample(n = -4, keep_order = TRUE, seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::slice_sample(n = -4),
               flights2 %>%
                 f_slice_sample(n = -4, keep_order = FALSE, seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_sample(n = -10),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_sample(n = -10, keep_order = FALSE, seed = 42))
  set.seed(42)
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_sample(n = -10) %>%
                 dplyr::arrange(id),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_sample(n = -10, keep_order = TRUE, seed = 42))
})
test_that("f_slice_min", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  expect_equal(flights2 %>%
                 f_slice_min(arr_time, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_min(arr_time))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, na_rm = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time))
  # expect_equal(flights2 %>%
  #                f_slice_min(arr_time, .by = c(origin, dest),
  #                            sort_groups = FALSE, na_rm = FALSE),
  #              flights2 %>%
  #                dplyr::slice_min(arr_time, by = c(origin, dest)))
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, n = 150),
               flights2 %>%
                 dplyr::slice_min(arr_time, n = 150))

  # Prop
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, prop = 0, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_min(arr_time, prop = 0, na_rm = FALSE))
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, prop = 1, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_min(arr_time, prop = 1, na_rm = FALSE))
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, prop = 1, na_rm = TRUE),
               flights2 %>%
                 dplyr::slice_min(arr_time, prop = 1, na_rm = TRUE))
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, prop = 0.33, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_min(arr_time, prop = 0.33, na_rm = FALSE))
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, prop = 0.33, na_rm = TRUE),
               flights2 %>%
                 dplyr::slice_min(arr_time, prop = 0.33, na_rm = TRUE))
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, prop = -0.33, na_rm = TRUE),
               flights2 %>%
                 dplyr::slice_min(arr_time, prop = -0.33, na_rm = TRUE))


  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = 4, na_rm = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = 4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = 4, na_rm = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = 4, na_rm = TRUE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = 4, na_rm = TRUE, with_ties = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = 4, na_rm = TRUE, with_ties = FALSE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = 100, na_rm = TRUE, with_ties = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = 100, na_rm = TRUE, with_ties = FALSE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = 100, na_rm = FALSE, with_ties = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = 100, na_rm = FALSE, with_ties = FALSE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = 4, na_rm = FALSE, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = 4, na_rm = FALSE) %>%
                 dplyr::arrange(id))

  expect_equal(flights2 %>%
                 f_slice_min(arr_time, n = -4, na_rm = FALSE, keep_order = TRUE),
               flights2 %>%
                 dplyr::slice_min(arr_time, n = -4) %>%
                 dplyr::arrange(id))
  expect_equal(flights2 %>%
                 f_slice_min(arr_time, n = -4, na_rm = FALSE, keep_order = FALSE),
               flights2 %>%
                 dplyr::slice_min(arr_time, n = -4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = -10, na_rm = FALSE, keep_order = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = -10))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_min(arr_time, n = -10, na_rm = FALSE, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_min(arr_time, n = -10) %>%
                 dplyr::arrange(id))
})
test_that("f_slice_max", {
  flights2 <- nycflights13::flights
  set.seed(81243844)
  flights2 <- flights2[sample(seq_len(nrow(flights2)),
                              size = 10^4), ]
  flights2[["id"]] <- seq_len(nrow(flights2))

  expect_equal(flights2 %>%
                 f_slice_max(arr_time, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_max(arr_time))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, na_rm = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time))
  expect_equal(flights2 %>%
                 f_group_by(origin, dest, order = FALSE) %>%
                 f_slice_max(arr_time, na_rm = FALSE) %>%
                 df_ungroup(),
               flights2 %>%
                 dplyr::slice_max(arr_time, by = c(origin, dest)))
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, n = 150),
               flights2 %>%
                 dplyr::slice_max(arr_time, n = 150))

  # Prop
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, prop = 0, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_max(arr_time, prop = 0, na_rm = FALSE))
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, prop = 1, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_max(arr_time, prop = 1, na_rm = FALSE))
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, prop = 1, na_rm = TRUE),
               flights2 %>%
                 dplyr::slice_max(arr_time, prop = 1, na_rm = TRUE))
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, prop = 0.33, na_rm = FALSE),
               flights2 %>%
                 dplyr::slice_max(arr_time, prop = 0.33, na_rm = FALSE))
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, prop = 0.33, na_rm = TRUE),
               flights2 %>%
                 dplyr::slice_max(arr_time, prop = 0.33, na_rm = TRUE))
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, prop = -0.33, na_rm = TRUE),
               flights2 %>%
                 dplyr::slice_max(arr_time, prop = -0.33, na_rm = TRUE))


  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = 4, na_rm = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = 4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = 4, na_rm = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = 4, na_rm = TRUE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = 4, na_rm = TRUE, with_ties = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = 4, na_rm = TRUE, with_ties = FALSE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = 100, na_rm = TRUE, with_ties = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = 100, na_rm = TRUE, with_ties = FALSE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = 100, na_rm = FALSE, with_ties = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = 100, na_rm = FALSE, with_ties = FALSE))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = 4, na_rm = FALSE, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = 4, na_rm = FALSE) %>%
                 dplyr::arrange(id))

  expect_equal(flights2 %>%
                 f_slice_max(arr_time, n = -4, na_rm = FALSE, keep_order = TRUE),
               flights2 %>%
                 dplyr::slice_max(arr_time, n = -4) %>%
                 dplyr::arrange(id))
  expect_equal(flights2 %>%
                 f_slice_max(arr_time, n = -4, na_rm = FALSE, keep_order = FALSE),
               flights2 %>%
                 dplyr::slice_max(arr_time, n = -4))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = -10, na_rm = FALSE, keep_order = FALSE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = -10))
  expect_equal(flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 f_slice_max(arr_time, n = -10, na_rm = FALSE, keep_order = TRUE),
               flights2 %>%
                 dplyr::group_by(origin, dest) %>%
                 dplyr::slice_max(arr_time, n = -10) %>%
                 dplyr::arrange(id))
})

test_that("Additional seed tests", {
  # The seed = 42 part should be set locally and original
  # global seed should be restored
  set.seed(42)
  ok <- f_slice_sample(iris, seed = 42)
  ok2 <- dplyr::slice_sample(iris, n = nrow(iris))
  expect_identical(ok, ok2)


  # Let's ensure seed continuity..

  set.seed(600)
  x <- rnorm(100)

  set.seed(600)
  y <- rnorm(50)
  f_slice_sample(iris, seed = 11230)
  expect_equal(x, c(y, rnorm(50)))

  set.seed(600)
  y <- rnorm(50)
  f_slice_sample(iris, seed = 600)
  expect_equal(x, c(y, rnorm(50)))

  # If we DONT set a local seed, seed continuity SHOULDNT be kept.
  # As one would expect
  set.seed(600)
  y <- rnorm(50)
  f_slice_sample(iris)
  expect_true(!isTRUE(all.equal(x, c(y, rnorm(50)))))

  current_seed <- .Random.seed
  f_slice_sample(iris, seed = 99)
  expect_identical(current_seed, .Random.seed)
})

test_that("test asan issues", {
  expect_equal(
    f_slice(iris, c(3L, 0L, 0L, 2L, 0L, 0L, 0L, 10000L, 0L)),
    cheapr::sset(iris, c(3L, 2L))
  )
})
