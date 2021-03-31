test_that("data is up-to-date", {
  skip_on_os("windows")

  pkg_path <- system.file(package = "munch")
  data_path <- file.path(pkg_path, "data")

  data <- swc_read_data()

  expect_equal(
    as_tibble(swc_get_cantons()),
    data$canton
  )
  expect_equal(
    as_tibble(swc_get_district_mutations()) %>%
      mutate(across(where(is.factor), as.character)),
    data$district %>%
      mutate(across(where(is.factor), as.character))
  )
  expect_equal(
    as_tibble(swc_get_municipality_mutations()) %>%
      mutate(across(where(is.factor), as.character)),
    data$municipality %>%
      mutate(across(where(is.factor), as.character))
  )
})
