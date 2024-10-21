
dplyr_quantiles <- function(data, vars, probs = seq(0, 1, 0.25), .by = NULL){
  dplyr::reframe(
    data,
    .quantile = cheapr::factor_(paste0("p", round(probs * 100, 10)), order = FALSE),
    across(all_of(vars), function(x) unname(quantile(x, na.rm = TRUE, probs = probs)))
  , .by = {{ .by }})
}

test_that("Edge cases", {

  quantile_factor <- cheapr::factor_(c("p0", "p25", "p50", "p75", "p100"), order = FALSE)

  expect_identical(
   tidy_quantiles(iris, Sepal.Length, probs = numeric(), pivot = "long"),
   cheapr::new_df(.quantile = factor(),
                  Sepal.Length = numeric())
  )

  expect_identical(
    tidy_quantiles(iris, Sepal.Length, probs = numeric(), pivot = "wide"),
    cheapr::new_df()
  )

  expect_identical(
    tidy_quantiles(f_slice(iris, 0), Sepal.Length, pivot = "long"),
    cheapr::new_df(
      .quantile = quantile_factor[0],
      Sepal.Length = numeric()
    )
  )

  expect_identical(
    tidy_quantiles(f_slice(iris, 0), Sepal.Length, pivot = "wide"),
    cheapr::new_df(
      p0 = numeric(),
      p25 = numeric(),
      p50 = numeric(),
      p75 = numeric(),
      p100 = numeric()
    )
  )

  expect_identical(
    tidy_quantiles(iris, pivot = "long"),
    cheapr::new_df(.quantile = quantile_factor)
  )

  expect_identical(
    tidy_quantiles(iris, pivot = "wide"),
    cheapr::new_df(
      p0 = numeric(),
      p25 = numeric(),
      p50 = numeric(),
      p75 = numeric(),
      p100 = numeric()
    )
  )

  expect_identical(
    tidy_quantiles(iris, pivot = "wide", .by = Species),
    cheapr::new_df(
      Species = iris$Species[0],
      p0 = numeric(),
      p25 = numeric(),
      p50 = numeric(),
      p75 = numeric(),
      p100 = numeric()
    )
  )

  expect_identical(
    tidy_quantiles(iris, pivot = "long", .by = Species),
    cheapr::new_df(
      Species = rep(unique(iris$Species), each = 5),
      .quantile = rep(quantile_factor, 3)
    )
  )

  expect_identical(
    tidy_quantiles(f_slice(iris, 0), Sepal.Length, pivot = "long", .by = Species),
    cheapr::new_df(
      Species = iris$Species[0],
      .quantile = quantile_factor[0],
      Sepal.Length = numeric()
    )
  )

  expect_identical(
    tidy_quantiles(f_slice(iris, 0), Sepal.Length, pivot = "wide", .by = Species),
    cheapr::new_df(
      Species = iris$Species[0],
      p0 = numeric(),
      p25 = numeric(),
      p50 = numeric(),
      p75 = numeric(),
      p100 = numeric()
    )
  )

  expect_identical(
    tidy_quantiles(f_slice(iris, 0), Sepal.Length, Sepal.Width, pivot = "wide", .by = Species),
    structure(
      list(Species = structure(integer(0), levels = c("setosa",
                                                      "versicolor", "virginica"),
                               class = "factor"), Sepal.Length_p0 = numeric(0),
           Sepal.Length_p25 = numeric(0), Sepal.Length_p50 = numeric(0),
           Sepal.Length_p75 = numeric(0), Sepal.Length_p100 = numeric(0),
           Sepal.Width_p0 = numeric(0), Sepal.Width_p25 = numeric(0),
           Sepal.Width_p50 = numeric(0), Sepal.Width_p75 = numeric(0),
           Sepal.Width_p100 = numeric(0)),
      class = "data.frame", row.names = integer(0)
    )
  )

  expect_identical(
    tidy_quantiles(f_slice(iris, 0), Sepal.Length, Sepal.Width,
                   pivot = "wide"),
    structure(
      list(Sepal.Length_p0 = numeric(0), Sepal.Length_p25 = numeric(0),
           Sepal.Length_p50 = numeric(0), Sepal.Length_p75 = numeric(0),
           Sepal.Length_p100 = numeric(0), Sepal.Width_p0 = numeric(0),
           Sepal.Width_p25 = numeric(0), Sepal.Width_p50 = numeric(0),
           Sepal.Width_p75 = numeric(0), Sepal.Width_p100 = numeric(0)),
      class = "data.frame", row.names = integer(0)
    )
  )

  # Unsorted, duplicate and integer probs

  expect_identical(
    tidy_quantiles(iris, Sepal.Length, probs = c(0L, 1L, 0L)),
    cheapr::new_df(
      .quantile = factor(c("p0", "p100", "p0")),
      Sepal.Length = c(
        min(iris$Sepal.Length), max(iris$Sepal.Length), min(iris$Sepal.Length)
      )
    )
  )

  expect_equal(
    tidy_quantiles(iris, Sepal.Length, probs = c(0, 0.5, 0, 0.25, 0.5)),
    dplyr_quantiles(iris, "Sepal.Length", probs = c(0, 0.5, 0, 0.25, 0.5))
  )

  expect_equal(
    tidy_quantiles(iris, Sepal.Length, probs = c(0, 0.5, 0, 0.25, 0.5),
                   .by = Species, .order = FALSE),
    dplyr_quantiles(iris, "Sepal.Length", probs = c(0, 0.5, 0, 0.25, 0.5),
                    .by = Species)
  )

})

test_that("unsorted probs", {
  expect_equal(
    iris %>%
      tidy_quantiles(Sepal.Length, probs = seq(1, 0, -0.01)),
    iris %>%
      dplyr_quantiles("Sepal.Length", probs = seq(1, 0, -0.01))
  )
  expect_equal(
    iris %>%
      tidy_quantiles(Sepal.Length, probs = seq(1, 0, -0.01),
                     .by = Species, .order = FALSE),
    iris %>%
      dplyr_quantiles("Sepal.Length", probs = seq(1, 0, -0.01),
                     .by = Species)
  )
})

test_that("Standard tests", {
  numeric_vars <- airquality %>%
    f_select(-Month) %>%
    names()

  expect_equal(
    dplyr_quantiles(airquality, "Ozone"),
    tidy_quantiles(airquality, .cols = "Ozone", pivot = "long")
  )
  expect_equal(
    dplyr_quantiles(airquality, "Ozone") %>%
      tidyr::pivot_wider(names_from = ".quantile",
                         values_from = "Ozone"),
    tidy_quantiles(as_tbl(airquality), .cols = "Ozone", pivot = "wide")
  )

  expect_equal(
    dplyr_quantiles(dplyr::group_by(airquality, Month), "Ozone"),
    tidy_quantiles(f_group_by(airquality, Month), .cols = "Ozone", pivot = "long")
  )

  expect_equal(
    dplyr_quantiles(dplyr::group_by(airquality, Month), "Ozone") %>%
      tidyr::pivot_wider(names_from = ".quantile",
                         values_from = "Ozone"),
    tidy_quantiles(f_group_by(airquality, Month), .cols = "Ozone", pivot = "wide")
  )


  expect_equal(
    dplyr_quantiles(dplyr::group_by(airquality, Month), "Ozone") %>%
      dplyr::group_by(Month),
    tidy_quantiles(dplyr::group_by(airquality, Month), .cols = "Ozone", pivot = "long",
                   .drop_groups = FALSE)
  )

  expect_equal(
    dplyr_quantiles(airquality, "Ozone") %>%
      tidyr::pivot_wider(names_from = ".quantile",
                         values_from = "Ozone"),
    tidy_quantiles(as_tbl(airquality), .cols = "Ozone", pivot = "wide")
  )

  expect_equal(
    dplyr_quantiles(dplyr::group_by(airquality, Month), numeric_vars),
    tidy_quantiles(as_tbl(airquality), .cols = numeric_vars,
                   .by = Month, pivot = "long")
  )

  expect_equal(
    dplyr_quantiles(dplyr::group_by(airquality, Month), numeric_vars) %>%
      tidyr::pivot_wider(names_from = ".quantile",
                         values_from = 3:7),
    tidy_quantiles(as_tbl(airquality), .cols = numeric_vars,
                   .by = Month, pivot = "wide")
  )

  expect_equal(
    dplyr_quantiles(trees, names(trees)),
    tidy_quantiles(trees, .cols = names(trees), pivot = "long")
  )

  expect_equal(
    dplyr_quantiles(trees, names(trees)) %>%
      tidyr::pivot_wider(names_from = 1, values_from = -1),
    tidy_quantiles(as_tbl(trees), .cols = names(trees), pivot = "wide")
  )

  expect_equal(
    dplyr_quantiles(as_tbl(EuStockMarkets), colnames(EuStockMarkets),
                    probs = seq(0, 1, 0.01)),
    tidy_quantiles(as_tbl(EuStockMarkets), .cols = colnames(EuStockMarkets),
                   probs = seq(0, 1, 0.01), pivot = "long")
  )
  expect_equal(
    dplyr_quantiles(as_tbl(EuStockMarkets), colnames(EuStockMarkets),
                    probs = seq(0, 1, 0.01)) %>%
      tidyr::pivot_wider(names_from = 1, values_from = -1),
    tidy_quantiles(as_tbl(EuStockMarkets), .cols = colnames(EuStockMarkets),
                   probs = seq(0, 1, 0.01), pivot = "wide")
  )

})

