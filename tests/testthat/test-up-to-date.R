context("up-to-date")

test_that("data is up-to-date", {
  skip("Broken")
  skip_on_os("windows")

  pkg_path <- system.file(package = "SwissCommunes")
  data_path <- file.path(pkg_path, "data")

  data <- swc_read_data()

  expect_identical(cantons, data$canton)
  expect_identical(district_mutations, data$district)
  expect_identical(municipality_mutations, data$municipality)

  cantons <- data$canton
  district_mutations <- data$district
  municipality_mutations <- data$municipality

  old <- usethis::proj_set(pkg_path)

  usethis::use_data(
    cantons, district_mutations, municipality_mutations,
    overwrite = TRUE
  )
})
