test_that("Compare to dplyr", {
  set.seed(42)
  weights <- sample(1:150)
  iris[["weight"]] <- weights
  flights <- nycflights13::flights
  expect_equal(iris %>% dplyr::count(),
                             iris %>% f_count())
  expect_identical(
    iris %>%
      collapse::qDT() %>%
      f_count(),
    collapse::qDT(cheapr::new_df(n = 150L))
  )
  expect_identical(
    iris %>%
      collapse::qDT() %>%
      dplyr::slice(0L) %>%
      f_count(),
    collapse::qDT(cheapr::new_df(n = 0L))
  )
  # Unused factor levels

  expect_equal(iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = FALSE) %>%
                           f_count(),
                         iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = FALSE) %>%
                           dplyr::count() %>%
                 dplyr::ungroup())
  expect_equal(iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = TRUE) %>%
                           f_count(),
                         iris %>%
                           dplyr::slice(2, 128, 125) %>%
                           dplyr::group_by(Species, .drop = TRUE) %>%
                           dplyr::count() %>%
                 dplyr::ungroup())
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of("Species"))) %>%
                 dplyr::ungroup(),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_count())
                               # f_count(across(all_of("Species"))))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of(c("Species", "Sepal.Length")))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_count(across(all_of(c("Sepal.Length")))))
                               # f_count(across(all_of(c("Species", "Sepal.Length")))))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(Species) %>%
                 dplyr::ungroup(),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_count(Species))
  expect_equal(iris %>% dplyr::count(NULL),
                             iris %>% f_count(NULL))
  expect_equal(iris %>% dplyr::slice(0) %>% dplyr::count(),
                             iris %>% dplyr::slice(0) %>% f_count())
  expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% dplyr::count() %>%
                 dplyr::ungroup(),
                             iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% f_count())
  expect_equal(iris %>%
                               dplyr::slice(0) %>% dplyr::count(Species),
                             iris %>%
                               dplyr::slice(0) %>% f_count(Species))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               dplyr::count(Species) %>%
                 dplyr::ungroup(),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               f_count(Species))
  expect_equal(iris %>% dplyr::count(NA),
                             iris %>% f_count(NA))
  expect_equal(iris %>% dplyr::count(across(dplyr::everything())),
                             iris %>% f_count(across(dplyr::everything())))
  expect_equal(iris %>% dplyr::count(across(dplyr::everything()),
                                                   wt = weight),
                             iris %>% f_count(across(dplyr::everything()),
                                             wt = weight) %>%
                               dplyr::mutate(n = as.integer(n)))
  expect_equal(iris %>% dplyr::count(),
                             iris %>% f_count())
  expect_equal(iris %>% dplyr::count(name = ".count"),
                             iris %>% f_count(name = ".count"))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count(Sepal.Length) %>%
                               dplyr::count(n, name = "n") %>%
                 dplyr::ungroup(),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_count(Sepal.Length) %>%
                               f_count(n, name = "n"))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::count() %>%
                 dplyr::ungroup(),
                             iris %>% dplyr::group_by(Species) %>% f_count())
  expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::count() %>%
                 dplyr::ungroup(),
                             iris %>% dplyr::group_by(across(everything())) %>% f_count())
  expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::count(Species) %>%
                 dplyr::ungroup(),
                             iris %>% dplyr::group_by(across(everything())) %>% f_count(Species))
  expect_equal(iris %>% dplyr::group_by(Species) %>% dplyr::count(Species) %>%
                 dplyr::ungroup(),
                             iris %>% dplyr::group_by(Species) %>% f_count(Species))
  expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::count(across(all_of(c("Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               f_count(across(all_of(c("Sepal.Length")))))
  expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::count(across(dplyr::any_of(c("Species", "Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               f_count(across(dplyr::any_of(c("Species", "Sepal.Length")))))
  expect_equal(iris %>%
                               dplyr::count(as.character(Species)),
                             iris %>%
                               f_count(as.character(Species)))
  expect_equal(flights %>% dplyr::count(tailnum, origin, dest),
                             flights %>% f_count(tailnum, origin, dest))
  expect_equal(flights %>% dplyr::count(tailnum, origin, dest, sort = TRUE),
                             flights %>% f_count(tailnum, origin, dest, sort = TRUE))

  # With weights
  res1 <- flights %>%
    f_count(origin, dest)
  set.seed(812123123)
  wt1 <- rep_len(3L, nrow(res1))
  wt2 <- sample(1:10, size = nrow(res1), replace = TRUE)

  res1 <- res1 %>%
    dplyr::mutate(wt2)
  expect_equal(res1 %>%
                               dplyr::count(origin, dest, wt = wt2),
                             res1 %>%
                               f_count(origin, dest, wt = wt2))
  expect_equal(res1 %>%
                               dplyr::mutate(wt1) %>%
                               dplyr::count(origin, dest, wt = wt1),
                             res1 %>%
                               f_count(origin, dest, wt = wt1))
  expect_equal(res1 %>%
                               dplyr::count(origin, dest, n, wt = wt2),
                             res1 %>%
                               f_count(origin, dest, n, wt = wt2))
  expect_equal(res1 %>%
                               dplyr::count(origin, dest, wt = n),
                             res1 %>%
                               f_count(origin, dest, wt = n))

  # Overwriting existing groups
  expect_equal(iris %>%
                           dplyr::group_by(Species) %>%
                           dplyr::count(Species = Sepal.Length) %>%
                 dplyr::ungroup(),
                         iris %>%
                           dplyr::group_by(Species) %>%
                           f_count(Species = Sepal.Length))
})

test_that("Compare to dplyr, add_count", {
  set.seed(42)
  weights <- sample(1:150)
  iris[["weight"]] <- weights
  flights <- nycflights13::flights
  # expect_equal(iris %>%
  #                              dplyr::mutate(interval = lubridate::interval(
  #                                Sys.Date(), Sys.Date())) %>%
  #                              f_add_count(),
  #                            iris %>%
  #                              dplyr::mutate(interval = lubridate::interval(
  #                                Sys.Date(), Sys.Date())) %>%
  #                              dplyr::add_count())
  expect_equal(iris %>% f_add_count(),
                             iris %>%
                               # data.table::as.data.table() %>%
                               dplyr::mutate(n = dplyr::n()))
  expect_equal(iris %>% dplyr::add_count(),
                             iris %>% f_add_count())
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of("Species"))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_add_count())
  # f_add_count(across(all_of("Species"))))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of(c("Species", "Sepal.Length")))),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_add_count(across(all_of(c("Sepal.Length")))))
  # f_add_count(across(all_of(c("Species", "Sepal.Length")))))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_add_count(Species))
  expect_equal(iris %>% dplyr::add_count(NULL),
                             iris %>% f_add_count(NULL))
  expect_equal(iris %>% dplyr::slice(0) %>% dplyr::add_count(),
                             iris %>% dplyr::slice(0) %>% f_add_count())
  expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% dplyr::add_count(),
                             iris %>% dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>% f_add_count())
  expect_equal(iris %>%
                               dplyr::slice(0) %>% dplyr::add_count(Species),
                             iris %>%
                               dplyr::slice(0) %>% f_add_count(Species))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               dplyr::add_count(Species),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::slice(0) %>%
                               f_add_count(Species))
  expect_equal(iris %>% dplyr::add_count(NA),
                             iris %>% f_add_count(NA))
  expect_equal(iris %>% dplyr::add_count(across(dplyr::everything())),
                             iris %>% f_add_count(across(dplyr::everything())))
  expect_equal(iris %>% dplyr::add_count(across(dplyr::everything()),
                                                   wt = weight),
                             iris %>% f_add_count(across(dplyr::everything()),
                                             wt = weight) %>%
                               dplyr::mutate(n = as.integer(n)))
  expect_equal(iris %>% dplyr::add_count(),
                             iris %>% f_add_count())
  expect_equal(iris %>% dplyr::add_count(name = ".count"),
                             iris %>% f_add_count(name = ".count"))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(Sepal.Length) %>%
                               dplyr::add_count(n, name = "n"),
                             iris %>%
                               dplyr::group_by(Species) %>%
                               f_add_count(Sepal.Length) %>%
                               f_add_count(n, name = "n"))
  expect_equal(iris %>%
                               dplyr::group_by(Species) %>%
                               dplyr::add_count(),
                             iris %>% dplyr::group_by(Species) %>% f_add_count())
  expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::add_count(),
                             iris %>% dplyr::group_by(across(everything())) %>% f_add_count())
  expect_equal(iris %>% dplyr::group_by(across(everything())) %>% dplyr::add_count(Species),
                             iris %>% dplyr::group_by(across(everything())) %>% f_add_count(Species))
  expect_equal(iris %>% dplyr::group_by(Species) %>% dplyr::add_count(Species),
                             iris %>% dplyr::group_by(Species) %>% f_add_count(Species))
  expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::add_count(across(all_of(c("Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               f_add_count(across(all_of(c("Sepal.Length")))))
  expect_equal(iris %>% dplyr::group_by(Species) %>%
                               dplyr::add_count(across(dplyr::any_of(c("Species", "Sepal.Length")))),
                             iris %>% dplyr::group_by(Species) %>%
                               f_add_count(across(dplyr::any_of(c("Species", "Sepal.Length")))))
  expect_equal(iris %>%
                               dplyr::add_count(as.character(Species)),
                             iris %>%
                               f_add_count(as.character(Species)))
  expect_equal(flights %>% dplyr::add_count(tailnum, origin, dest),
                             flights %>% f_add_count(tailnum, origin, dest))
  expect_equal(flights %>% dplyr::add_count(tailnum, origin, dest, sort = TRUE),
                             flights %>% f_add_count(tailnum, origin, dest, sort = TRUE))

  # With weights
  res1 <- flights %>%
    f_count(origin, dest)
  set.seed(812123123)
  wt1 <- rep_len(3L, nrow(res1))
  wt2 <- sample(1:10, size = nrow(res1), replace = TRUE)

  res1 <- res1 %>%
    dplyr::mutate(wt2)
  expect_equal(res1 %>%
                               dplyr::add_count(origin, dest, wt = wt2),
                             res1 %>%
                               f_add_count(origin, dest, wt = wt2))
  expect_equal(res1 %>%
                               dplyr::mutate(wt1) %>%
                               dplyr::add_count(origin, dest, wt = wt1),
                             res1 %>%
                               dplyr::mutate(wt1) %>%
                               f_add_count(origin, dest, wt = wt1))
  expect_equal(res1 %>%
                               dplyr::add_count(origin, dest, n, wt = wt2),
                             res1 %>%
                               f_add_count(origin, dest, n, wt = wt2))
  expect_equal(res1 %>%
                               dplyr::add_count(origin, dest, wt = n),
                             res1 %>%
                               f_add_count(origin, dest, wt = n))
  # Overwriting existing groups
  expect_equal(iris %>%
                           dplyr::group_by(Species) %>%
                           dplyr::add_count(Species = Sepal.Length),
                         iris %>%
                           dplyr::group_by(Species) %>%
                           f_add_count(Species = Sepal.Length))
})

