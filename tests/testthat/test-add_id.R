fastplyr_disable_informative_msgs()

df <- as_tbl(cheapr::with_local_seed(f_slice_sample(iris, n = 300, replace = TRUE), 42))

test_that("row IDs", {
  expect_equal(
    add_row_id(df),
    cheapr::df_modify(df, list(row_id = 1:nrow(df)))
  )

  expect_equal(
    add_row_id(df, Species),
    cheapr::df_modify(df, list(row_id = row_id(df$Species)))
  )

  expect_equal(
    add_row_id(df, .by = Species),
    cheapr::df_modify(df, list(row_id = row_id(df$Species)))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_row_id() |> as_tbl(),
    cheapr::df_modify(df, list(row_id = row_id(df$Species)))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_row_id(Sepal.Width, Species) |> as_tbl(),
    cheapr::df_modify(df, list(row_id = row_id(df |> f_select(.cols = c("Species", "Sepal.Width")))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_row_id(.cols = "Sepal.Width") |> as_tbl(),
    cheapr::df_modify(df, list(row_id = row_id(df |> f_select(.cols = c("Species", "Sepal.Width")))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_row_id(.cols = c("Species", "Sepal.Width")) |> as_tbl(),
    cheapr::df_modify(df, list(row_id = row_id(df |> f_select(.cols = c("Species", "Sepal.Width")))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_row_id(.cols = c( "Sepal.Width", "Species")) |> as_tbl(),
    cheapr::df_modify(df, list(row_id = row_id(df |> f_select(.cols = c("Species", "Sepal.Width")))))
  )

  expect_equal(
    add_row_id(df, Species, .name = ".id"),
    cheapr::df_modify(df, list(.id = row_id(df$Species)))
  )
})

test_that("group IDs", {

  expect_equal(
    add_group_id(df),
    cheapr::df_modify(df, list(group_id = rep_len(1L, nrow(df))))
  )

  expect_equal(
    add_group_id(df, Species),
    cheapr::df_modify(df, list(group_id = group_id(df$Species)))
  )

  expect_equal(
    add_group_id(df, .by = Species),
    cheapr::df_modify(df, list(group_id = group_id(df$Species)))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_group_id() |> as_tbl(),
    cheapr::df_modify(df, list(group_id = group_id(df$Species)))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_group_id(Sepal.Width, Species) |> as_tbl(),
    cheapr::df_modify(df, list(group_id = group_id(df |> f_select(.cols = c("Sepal.Width", "Species")))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_group_id(.cols = "Sepal.Width") |> as_tbl(),
    cheapr::df_modify(df, list(group_id = group_id(df |> f_select(.cols = c("Species", "Sepal.Width")))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_group_id(.cols = c("Species", "Sepal.Width")) |> as_tbl(),
    cheapr::df_modify(df, list(group_id = group_id(df |> f_select(.cols = c("Species", "Sepal.Width")))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_group_id(.cols = c( "Sepal.Width", "Species")) |> as_tbl(),
    cheapr::df_modify(df, list(group_id = group_id(df |> f_select(.cols = c("Sepal.Width", "Species")))))
  )

  expect_equal(
    add_group_id(df, Species, .name = ".id"),
    cheapr::df_modify(df, list(.id = group_id(df$Species)))
  )
})


test_that("consecutive IDs", {

  expect_equal(
    add_consecutive_id(df),
    cheapr::df_modify(df, list(consecutive_id = rep_len(1L, nrow(df))))
  )

  expect_equal(
    add_consecutive_id(df, Species),
    cheapr::df_modify(df, list(consecutive_id = f_consecutive_id(df$Species)))
  )

  expect_equal(
    add_consecutive_id(df, .by = Species),
    cheapr::df_modify(df, list(consecutive_id = rep_len(1L, nrow(df))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_consecutive_id() |> as_tbl(),
    cheapr::df_modify(df, list(consecutive_id = rep_len(1L, nrow(df))))
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_consecutive_id(Sepal.Width, Species) |> as_tbl(),
    df |>
      dplyr::mutate(Species2 = Species) |>
      dplyr::mutate(
        consecutive_id = dplyr::consecutive_id(pick(Sepal.Width, Species)),
        .by = Species2
      ) |>
      dplyr::select(-Species2)
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_consecutive_id(.cols = "Sepal.Width") |> as_tbl(),
    df |>
      dplyr::mutate(consecutive_id = dplyr::consecutive_id(Sepal.Width), .by = Species)
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_consecutive_id(.cols = c("Species", "Sepal.Width")) |> as_tbl(),
    df |>
      dplyr::mutate(Species2 = Species) |>
      dplyr::mutate(consecutive_id = dplyr::consecutive_id(dplyr::pick(Species, Sepal.Width)), .by = Species2) |>
      dplyr::select(-Species2)
  )

  expect_equal(
    f_group_by(df, .cols = "Species") |> add_consecutive_id(.cols = c( "Sepal.Width", "Species")) |> as_tbl(),
    df |>
      dplyr::mutate(Species2 = Species) |>
      dplyr::mutate(consecutive_id = dplyr::consecutive_id(dplyr::pick(Sepal.Width, Species)), .by = Species2) |>
      dplyr::select(-Species2)
  )

  expect_equal(
    add_consecutive_id(df, Species, .name = ".id"),
    cheapr::df_modify(df, list(.id = dplyr::consecutive_id(df$Species)))
  )
})
